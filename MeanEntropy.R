require('entropy')

cluster_col = "SOLR"

load("Data.RData")
load(paste("Clusters_", cluster_col, ".RData", sep = ""))
# load("Haar.RData")

averageHours <- function(row) {
  mean(row, na.rm=T)
}

sumHours <- function(row) {
  sum(row, na.rm=T)
}

getColNames <- function(merged) {
  ignore_list <- c("YEAR", "MON", "DAY", "HR", "MIN", "TMZN", "DT", "DT_NUM", "TIME")
  raw_cols <- setdiff(names(merged), ignore_list)
  
  cols <- new.env()
  for(i in 6:18) {
    for(col in raw_cols) {
      if(is.null(cols[[col]])) {
        cols[[col]] <- character(0)
      }
      cols[[col]] <- c(cols[[col]], paste(col,i,sep="_"))
    }
  }
  return(cols)
}

toDegrees <- function(rad) {
  return(rad * 180 / pi)
}

outputMeanEntropy <- function(cluster_name, data, col_names, file_name, num_bins,type="kmeans", offset=0) {
  clustering <- get(cluster_name)
  
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  if(offset != 0) {
    days <- strptime(as.character(data$day), "%Y-%m-%d")
    clusters <- c(NA,clusters[-length(clusters)])
    missing <- numeric(0)
    for(i in 1:(length(days) - offset)) { 
      if(days[i + offset] - days[i] != offset) { 
        missing <- c(missing, i + offset) 
      } 
    }
    clusters[missing] <- NA
  }
  
  ignore_list <- c("day")
  
  for(i in 0:23) {
    ignore_list <- c(ignore_list, paste("TIME", i, sep="_"), paste("MIN", i, sep="_"), paste("DT", i, sep="_"), paste("DT_NUM", i, sep="_"))
  }
  
  mean_cols <- character(0)
  
  # Calculate means for each feature
  means <- data.frame(day=data$day)
  for(col in ls(col_names)) {
    if(col == "PREC") {
      selected_cols <- intersect(names(data), col_names[[col]])
      filtered <- data[,selected_cols]
      for(c in selected_cols) {
        filtered[which(filtered[,c] >= 4), c] <- NA        
      }      
      means[[col]] <- apply(filtered[,selected_cols], 1, sumHours)
      mean_cols <- c(mean_cols, col)
    } else if(col != "SKNT" && col != "DRCT"  && col != "PEAK"&& col != "PDIR") {
      means[[col]] <- apply(data[,intersect(names(data), col_names[[col]])], 1, averageHours)
      mean_cols <- c(mean_cols, col)
    }    
  }
  
  # TODO DRY this part
  
  means[["SKNT"]] <- sqrt(means[["SKNT_E"]]^2 + means[["SKNT_N"]]^2)
  mean_cols <- c(mean_cols, "SKNT")
  drcts <- numeric(0)
  for(i in 1:length(means[["SKNT_N"]])) {
    n <- means[["SKNT_N"]][i]
    e <- means[["SKNT_E"]][i]
    if(n >= 0) {
      
      if(e >= 0) {
        # Q3 = S -> W
        drcts <- c(drcts, toDegrees(abs(atan(e / n))) + 180)
      } else {
        # Q2 = E -> S
        drcts <- c(drcts, toDegrees(abs(atan(n / e))) + 90)
      }
      
    } else {
      if(e >= 0) {
        # Q4 = W -> N
        drcts <- c(drcts, toDegrees(abs(atan(n / e))) + 270)
      } else {
        # Q1 = N -> E
        drcts <- c(drcts, toDegrees(abs(atan(e / n))))
      }
    }    
  }
  means[["DRCT"]] <- drcts
  mean_cols <- c(mean_cols, "DRCT")
  
  means[["PEAK"]] <- sqrt(means[["PEAK_E"]]^2 + means[["PEAK_N"]]^2)
  mean_cols <- c(mean_cols, "PEAK")
  drcts <- numeric(0)
  for(i in 1:length(means[["PEAK_N"]])) {
    n <- means[["PEAK_N"]][i]
    e <- means[["PEAK_E"]][i]
    if(n >= 0) {
      
      if(e >= 0) {
        # Q3 = S -> W
        drcts <- c(drcts, toDegrees(abs(atan(e / n))) + 180)
      } else {
        # Q2 = E -> S
        drcts <- c(drcts, toDegrees(abs(atan(n / e))) + 90)
      }
      
    } else {
      if(e >= 0) {
        # Q4 = W -> N
        drcts <- c(drcts, toDegrees(abs(atan(n / e))) + 270)
      } else {
        # Q1 = N -> E
        drcts <- c(drcts, toDegrees(abs(atan(e / n))))
      }
    }    
  }
  means[["PDIR"]] <- drcts
  mean_cols <- c(mean_cols, "PDIR")
  
  partitions <- list()
  partition_cols <- setdiff(names(means), ignore_list)
  num_partitions <- nrow(centroids)
  headings <- "Feature"
  
  total <- nrow(means)
  probs <- list()
  
  for(i in 1:num_partitions) {
    headings <- paste(headings, ",Partition ", i, sep="")
    partitions[[i]] <- as.data.frame(means[which(clusters == i), partition_cols])
    probs[[i]] <- nrow(partitions[[i]]) / total
  }
  headings <- paste(headings, "Conditional Entropy", "Standard Deviation", sep=",")
  
  # discretized[[partition]][[Feature]]
  discretized <- list()
  sink(file_name)
  writeLines(headings)
  for(col in partition_cols) {
    row <- col
    entropies <- numeric(0)
    for(i in 1:num_partitions) {
      partition <- partitions[[i]]
      range <- range(means[,col], na.rm=T)
      if(range[1] == range[2]) {
        row <- paste(row, 0, sep=",")
      } else {
        discretized[[as.character(i)]][[col]] <- discretize(partition[which(!is.na(partition[[col]])),col], num_bins, range)
        entropy_val <- probs[[i]] * entropy(discretized[[as.character(i)]][[col]])
        entropies <- c(entropies, entropy_val)
        row <- paste(row, entropy_val, sep=",")
      }
    }
    row <- paste(row, sum(entropies), sd(entropies), sep=",")
    writeLines(row)
  }
  
  
  sink()
  save(means, file=paste(paste(cluster_name, "means", offset, sep="_"), ".RData", sep=""))
  save(discretized, file=paste(paste(cluster_name, "discretized", offset, sep="_"), ".RData", sep=""))
}

col_names <- getColNames(merged)

num_bins <- 10
offset = 1
suffix <- paste("_mean_cond_entropy_", offset, ".csv", sep="")

for(i in 4:10) {
  kmeans_name <- paste("kmeans",cluster_col, i,sep="_")  
  outputMeanEntropy(kmeans_name, get(paste("data", cluster_col, sep="_")), col_names, paste(kmeans_name, suffix, sep=""), num_bins, offset=offset)
  
#   haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
#   outputMeanEntropy(haar_kmeans_name, haar_solr_data, col_names, paste(haar_kmeans_name, suffix, sep=""), num_bins, offset=offset)
  
  pam_name <- paste("pam", cluster_col, i,sep="_")
  outputMeanEntropy(pam_name, get(paste("data", cluster_col, sep="_")), col_names, paste(pam_name, suffix, sep=""), num_bins, type="pam", offset=offset)
  
#   haar_pam_name <- paste("coef_pam",i,sep="_")
#   outputMeanEntropy(haar_pam_name, haar_solr_data, col_names, paste(haar_pam_name, suffix, sep=""), num_bins, type="pam", offset=offset)
}