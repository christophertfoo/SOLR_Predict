averageHours <- function(row) {
  mean(row, na.rm=T)
}

getColNames <- function(merged, start=6, end=18) {
  ignore_list <- c("YEAR", "MON", "DAY", "HR", "MIN", "TMZN", "DT", "DT_NUM", "TIME")
  raw_cols <- setdiff(names(merged), ignore_list)
  
  cols <- new.env()
  for(i in start:end) {
    for(col in raw_cols) {
      if(is.null(cols[[col]])) {
        cols[[col]] <- character(0)
      }
      cols[[col]] <- c(cols[[col]], paste(col,i,sep="_"))
    }
  }
  return(cols)
}

getMeans <- function(data, col_names) {
  ignore_list <- c("day")
  
  for(i in 0:23) {
    ignore_list <- c(ignore_list, paste("TIME", i, sep="_"), paste("MIN", i, sep="_"), paste("DT", i, sep="_"), paste("DT_NUM", i, sep="_"))
  }
  
  means <- data.frame(day=data$day)
  for(col in ls(col_names)) {
    means[[col]] <- apply(data[,intersect(names(data), col_names[[col]])], 1, averageHours)
  }
  return(means)
}

matchClusterings <- function(means, clustering, type="kmeans") {
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  means[['cluster']] <- clusters
  return(means)  
}

getColOffset <- function(matched, value_col, new_col_name, noffset) {
  day <- 60 * 60 * 24
  matches <- numeric(0)
  for(i in 1:nrow(matched)) {
    matches <- c(matches, matched[which(matched[,'day'] - matched[i,'day'] == -(noffset * day))[1], value_col])
  }
  
  matched[[new_col_name]] <- matches
  return(matched)
}

lloydMinMax <- function(data, k = 10, num_iter = 1000, min_diff = NULL) {
  sorted <- sort(data)
  
  if(is.null(min_diff)) {
    min_diff <- 0.1 * max(sorted, na.rm = TRUE)
  }
  
  # Initialize centroids
  values <- unique(sorted)
  values <- values[which(!is.na(values))]
  num_unique <- length(values)
  centroids <- numeric(0)
  if(num_unique <= k || k == 1){
    k <- num_unique
  }
  else {
    values <- sample(values, k)
  }
  for(value in values) {
    matches <- which(sorted == value)
    if(length(matches) == 1) {
      centroids <- c(centroids, sorted[matches[1]])
    }
    else {
      centroids <- c(centroids, sorted[sample(matches, 1)])
    }      
  }
  
  centroids <- sort(centroids)
  iter <- 0
  diff <- Inf
  while(iter < num_iter && diff > min_diff) {
    writeLines(paste(iter + 1, num_iter, sep=" / "))
    boundaries <- sorted[1]
    for(i in 1:(length(centroids) - 1)) {
      boundaries <- c(boundaries, mean(c(centroids[i], centroids[i + 1])))
    }
    boundaries <- c(boundaries, sorted[length(sorted)])
    new_centroids <- numeric(0)
    new_centroids <- c(new_centroids, mean(sorted[which(sorted <= boundaries[2])]))
    if(length(boundaries) >= 4) {
      for(i in 2:(length(boundaries) - 2)) {
        new_centroids <- c(new_centroids, mean(sorted[which(sorted > boundaries[i] & sorted <= boundaries[i + 1])]))
      }
    }
    new_centroids <- c(new_centroids, mean(sorted[which(sorted > boundaries[length(boundaries) - 1])]))  
    diff <- dist(rbind(centroids, new_centroids))
    centroids <- new_centroids
    iter <- iter + 1
  }
  
  result <- list()
  # Set boundaries
  result[["boundaries"]] <- boundaries
  
  # Set range strings
  result[["range_str"]] <- paste("[", round(boundaries[1], 2), ",", round(boundaries[2], 2), "]", sep="")
  for(i in 2:(length(boundaries) - 1)) {
    result[["range_str"]] <-c(result[["range_str"]], paste("(", round(boundaries[i], 2), ",", round(boundaries[i + 1], 2), "]", sep=""))
  }
  return(result)
}

lloydDiscretize <- function(data, ranges) {
  boundaries <- ranges[["boundaries"]]
  next_bin <- 1
  bins <- numeric(0)
  bins[which(data <= boundaries[2])] <- next_bin
  next_bin <- next_bin + 1
  
  for(i in 2:(length(boundaries) - 2)) {
    bins[which(data > boundaries[i] & data <= boundaries[i + 1])] <- next_bin 
    next_bin <- next_bin + 1
  }
  
  bins[which(data > boundaries[length(boundaries) - 1])] <- next_bin
  return(bins)
}

lloydMatchStrings <- function(discretized, ranges) {
  i <- 1
  range_strs <- ranges[["range_str"]]
  matched <- discretized
  for(range in range_strs) {
    matched[which(matched == i)] <- range
    i <- i + 1
  }
  return(matched)
}

countDiscretized <- function(discretized, ranges = NULL) {
  counts <- numeric(0)
  counts <- c(counts, length(discretized[which(is.na(discretized))]))
  if(!is.null(ranges)) {
    bins <- 1:length(ranges[["range_str"]])
  }
  else {
    bins <- sort(unique(discretized))
  }  
  found_bins <- list()
  for(bin in bins) {
    counts <- c(counts, length(discretized[which(discretized == bin)]))
    if(!is.null(ranges)) {
      found_bins <- c(found_bins, ranges[["range_str"]][bin])
    }
    else {
      found_bins <- c(found_bins, bin)
    }
  }
  names(counts) <- c("Missing", as.character(found_bins))
  return(counts)
}

outputCounts <- function(discretized, col, bins = NULL, file = NULL) {
  counts <- NULL
  row_labels <- character(0)
  for(partition in names(discretized)) {
    if(is.null(bins)) {
      if(is.null(counts)) {
        counts <- discretized[[partition]][[col]]
      } else {
        counts <- rbind(counts, discretized[[partition]][[col]]) 
      }
    } else {
      if(is.null(counts)) {
        counts <- countDiscretized(discretized[[partition]][[col]], bins[[col]])
        
      } else {
        counts <- rbind(counts, countDiscretized(discretized[[partition]][[col]], bins[[col]])) 
      }
    }
    row_labels <- c(row_labels, paste("Partition", partition))
  }
  rownames(counts) <- row_labels
  if(is.null(file)) {
    file <- paste(col, "counts", sep="_")
  }
  write.csv(counts, file = paste(file, ".csv", sep=""))
  png(paste(file, ".png", sep=""))
  barplot(counts, beside=TRUE, col=rainbow(nrow(counts)), legend=rownames(counts), xlab=col, ylab="Counts", main=paste(col, "Counts"))
  dev.off()
}

joinClusters <- function(first, other, first_cluster_t, first_n_cluster, other_cluster_t, other_n_cluster, offset=0) {
  
  if(offset == 0) {
    
    # Change days to character strings so that comparisons are based on contents, not enum value
    first_days <- as.character(get(paste("data",first,sep="_"))[["day"]])
    other_days <- as.character(get(paste("data",other,sep="_"))[["day"]])
    
    # Get set of all days present
    all <- unique(c(first_days, other_days))  
    
    # Filter to get only the days in common between both clusterings
    join <- all[which(all %in% other_days & all %in% first_days)]
    days <- character(0)
    first_clusters <- integer(0)
    other_clusters <- integer(0)
    
    # Get appropriate key
    first_cluster_key <- ""
    other_cluster_key <- ""
    if(first_cluster_t == "kmeans") {
      first_cluster_key <- 'cluster'
    } else if(first_cluster_t == "pam") {
      first_cluster_key <- 'clustering'
    }
    if(other_cluster_t == "kmeans") {
      other_cluster_key <- 'cluster'
    } else if(other_cluster_t == "pam") {
      other_cluster_key <- 'clustering'
    }
    
    # Get clusterings
    first_clustered <- get(paste(first_cluster_t,first,first_n_cluster,sep="_"))[[first_cluster_key]]
    other_clustered <- get(paste(other_cluster_t,other,other_n_cluster,sep="_"))[[other_cluster_key]]
    
    # Match the days between clusterings
    for(day in join) {
      days <- c(days, day)
      first_clusters <- c(first_clusters, first_clustered[which(first_days == day)])
      other_clusters <- c(other_clusters, other_clustered[which(other_days == day)])
    }
    
    
    frame <- data.frame(day=days, cluster1=first_clusters, cluster2=other_clusters, stringsAsFactors = FALSE)
    names(frame) <- c("day", paste(first, first_n_cluster, sep="_"), paste(other, other_n_cluster, sep="_"))
    
    # Return joined clusterings
    return(frame)
  } else {
    # Change days to character strings so that comparisons are based on contents, not enum value
    first_days <- as.character(get(paste("data",first,sep="_"))[["day"]])
    other_days <- as.character(get(paste("data",other,sep="_"))[["day"]])
    
    offset_days <- as.character(strptime(first_days, "%Y-%m-%d") - 86400 * offset)
    
    # Filter to get only the days in common between both clusterings
    matched_index <- which(offset_days %in% other_days)
    days <- character(0)
    first_clusters <- integer(0)
    other_clusters <- integer(0)
    
    # Get appropriate key
    first_cluster_key <- ""
    other_cluster_key <- ""
    if(first_cluster_t == "kmeans") {
      first_cluster_key <- 'cluster'
    } else if(first_cluster_t == "pam") {
      first_cluster_key <- 'clustering'
    }
    if(other_cluster_t == "kmeans") {
      other_cluster_key <- 'cluster'
    } else if(other_cluster_t == "pam") {
      other_cluster_key <- 'clustering'
    }
    
    # Get clusterings
    first_clustered <- get(paste(first_cluster_t,first,first_n_cluster,sep="_"))[[first_cluster_key]]
    other_clustered <- get(paste(other_cluster_t,other,other_n_cluster,sep="_"))[[other_cluster_key]]
    
    # Match the days between clusterings
    for(index in matched_index) {
      days <- c(days, first_days[index])
      first_clusters <- c(first_clusters, first_clustered[index])
      other_clusters <- c(other_clusters, other_clustered[which(other_days == offset_days[index])])
    }
    
    
    frame <- data.frame(day=days, cluster1=first_clusters, cluster2=other_clusters, stringsAsFactors = FALSE)
    names(frame) <- c("day", paste(first, first_n_cluster, sep="_"), paste(other, other_n_cluster, offset, sep="_"))
    
    # Return joined clusterings
    return(frame)
  }
}

joinJoined <- function(joined1, joined2) {
  days <- sort(unique(c(joined1[['day']], joined2[['day']])))
  days <- days[which(days %in% joined1[['day']] & days %in% joined2[['day']])]
  
  cols1 <- names(joined1)
  cols2 <- names(joined2)
  cols <- unique(c(cols1, cols2))
  cols <- cols[which(cols != 'day')]
  
  frame <- data.frame(day = days)
  for(col in cols) {
    if(col %in% cols1) {
      frame <- cbind(frame, joined1[which(joined1$day %in% days), col])
    } else {
      frame <- cbind(frame, joined2[which(joined2$day %in% days), col])
    }
  }
  names(frame) <- c("day", cols)
  
  return(frame)
}

filterJoined <- function(joined, years) {
  indexes <- NULL
  days <- as.character(joined[,1])
  for(year in years) {
    if(is.null(indexes)) {
      indexes <- days >= paste(year, "-01-01", sep="") & days <= paste(year, "-12-31", sep="")
    } else {
      indexes <- indexes | (days >= paste(year, "-01-01", sep="") & days <= paste(year, "-12-31", sep=""))
    }
  }
  return(joined[which(indexes),])
}

clusterCondEntropy <- function(joined, file=NULL) {
  discretized <- list()
  probs <- list()
  cluster1 <- names(joined)[2]
  cluster2 <- names(joined)[3]
  
  if(is.null(file)) {
    file <- paste("cluster_cond_entropy_", cluster1, "_", cluster2, ".csv", sep="")
  }
  
  cluster2_last_bin = range(joined[[cluster2]])[2]
  cluster1_partitions = sort(unique(joined[[cluster1]]))
  for(part in cluster1_partitions) {
    probs[[part]] <- length(which(joined[[cluster1]] == part)) / nrow(joined)
  }
  entropies <- numeric(0)
  row_names <- character(0)
  sink(file)
  writeLines("Partition,Conditional Entropy")
  for(part in cluster1_partitions) {
    row_names <- c(row_names, paste("Partition",part))
    discretized[[as.character(part)]] <- discretize(joined[which(joined[[cluster1]] == part), cluster2], cluster2_last_bin)
    entropy <- probs[[part]] * entropy(discretized[[as.character(part)]])
    entropies <- c(entropies, entropy)
    writeLines(paste("Partition ", part, ",", entropy, sep=""))
  }
  writeLines(paste("Average",mean(entropies),sep=","))
  sink()
  
  part_names <- names(discretized)
  sink(paste("cluster_discretized_", cluster1, "_", cluster2, ".csv", sep=""))
  writeLines(paste(",", paste(paste("\"", names(discretized[[part_names[1]]]), "\"", sep=""), collapse = ","), sep=""))
  for(part in part_names) {
    writeLines(paste("Partition ", part, ",", paste(discretized[[part]], collapse = ","), sep=""))
  }
  sink()
  return(discretized)
}

conditionalProbabilityDiscretized <- function(discretized, column, file=NULL) {
  
  if(is.null(file)) {
    file <- paste(column,"_conditional_probability.csv", sep="")
  }
  
  filtered <- discretized[['1']][[column]]
  row_names <- "Partition 1"
  for(i in 2:length(names(discretized))) {
    filtered <- rbind(filtered, discretized[[as.character(i)]][[column]])
    row_names <- c(row_names, paste("Partition", i))
  }
  
  col_sums <- numeric(0)
  for(i in 1:ncol(filtered)) {
    col_sums <- c(col_sums, sum(filtered[,i]))
  }
  
  col_names <- paste("\"", colnames(filtered), "\"", sep="")
  sink(file)
  writeLines(paste(",", paste(col_names, collapse = ","), sep=""))
  for(i in 1:length(row_names)) {
    row <- row_names[i]
    for(j in 1:ncol(filtered)) {
      row <- c(row, filtered[i, j] / col_sums[j])
    }
    writeLines(paste(row, collapse=","))
  }
  sink()
}

conditionalProbabilityClusters <- function(joined, file=NULL) {
  cols <- names(joined)
  first_col <- cols[2]
  other_col <- cols[3]
  if(is.null(file)) {
    file <- paste(first_col, other_col, "cond_prob.csv", sep="_")
  }
  first_clusters <- sort(unique(joined[,2]))
  other_clusters <- sort(unique(joined[,3]))
  sink(file)
  row <- character(0)
  for(i in other_clusters) {
    row <- c(row, paste(other_col, i))
  }
  writeLines(paste(",", paste(row, collapse=",")))
  
  for(i in first_clusters) {
    row <- paste(first_col, i)
    for(j in other_clusters) {
      row <- c(row, length(which(joined[,2] == i & joined[,3] == j)) / length(which(joined[,3] == j)))
    }
    writeLines(paste(row, collapse = ","))
  }
  sink()
}

