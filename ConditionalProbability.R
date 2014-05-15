load("Clusters.RData")
load("Haar.RData")

getClusterings <- function(data, clustering, type="kmeans") {
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  matched <- data.frame(day=strptime(as.character(data[,"day"]), '%Y-%m-%d'), cluster=clusters)
  return(matched)  
}

getClusteringOffset <- function(matched, col_name, noffset) {
  day <- 60 * 60 * 24
  
  matches <- numeric(0)
  for(i in 1:nrow(matched)) {
    matches <- c(matches, matched[which(matched[,'day'] - matched[i,'day'] == -(noffset * day))[1], 'cluster'])
  }
  
  matched[[col_name]] <- matches
  return(matched)
}

getProb <- function(matched, target_col, given_col) {
  probabilities <- list()
  for(i in 1:nrow(matched)) {
    if(!is.na(matched[i, given_col]) && !is.na(matched[i, target_col])) {
      given_val <- as.character(matched[i, given_col])
      target_val <- as.character(matched[i, target_col])
      if(is.null(probabilities[[given_val]])) {
        probabilities[[given_val]] <- list()
      }
      if(is.null(probabilities[[given_val]][[target_val]])) {
        probabilities[[given_val]][[target_val]] <- 1
      } else {
        probabilities[[given_val]][[target_val]] <- probabilities[[given_val]][[target_val]] + 1
      }
    }
  }
  
  for(given_val in names(probabilities)) {
    sum <- 0
    for(target_val in names(probabilities[[given_val]])) {
      sum <- sum + probabilities[[given_val]][[target_val]]
    }
    for(target_val in names(probabilities[[given_val]])) {
      probabilities[[given_val]][[target_val]] <- probabilities[[given_val]][[target_val]] / sum
    }
  }
  
  return(probabilities)
}

outputProb <- function(probabilities, col_name, file_name) {
  row <- paste(paste(col_name, ' -> current cluster', sep=""), "probability",sep=",")
  sink(file_name)
  writeLines(row)
  for(given_val in sort(names(probabilities))) {
    for(target_val in sort(names(probabilities[[given_val]]))) {
      row <- paste(paste(given_val, target_val, sep=" -> "), probabilities[[given_val]][[target_val]],sep=",")
      writeLines(row)
    }
    writeLines("")
  }
  sink()
}