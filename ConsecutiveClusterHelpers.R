getPartitions <- function(data, clustering, type="kmeans") {
  
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  partitions <- list()
  npartitions <- nrow(centroids)
  times <- strptime(as.character(data[,"day"]), '%Y-%m-%d')
  for(i in 1:npartitions) {
    partitions[[i]] <- times[which(clusters == i)]
  }
  return(partitions)
}

findConsecutive <- function(partition, margin=1) {
  consecutive <- list()
  current <- list()
  for(i in 2:length(partition)) {
    if(partition[i] - partition[i - 1] <= margin) {
      if(length(current) > 0) {
        current <- c(current, partition[i])
      } else {
        current <- c(partition[i - 1], partition[i])
      }
    }
    else {
      if(length(current) > 0) {
        consecutive <- c(consecutive, list(current))
        current <- list()
      }
    }
  }
  if(length(current) > 0) {
    consecutive <- c(consecutive, list(current))
  }
  return(consecutive)
}

outputConsecutive <- function(data, cluster_name, file_name=NULL, type="kmeans") {
  clustering <- get(cluster_name)
  partitions <- getPartitions(data, clustering, type)
  consecutives <- list()
  for(i in 1:length(partitions)) {
    consecutives[[i]] <- findConsecutive(partitions[[i]])
  }
  
  lengths <- list()
  for(i in 1:length(consecutives)) {
    lengths[[i]] <- unlist(lapply(consecutives[[i]], length))
  }
  
  if(is.null(file_name)) {
    sink(paste(cluster_name,"_consecutives.csv", sep=""))
  } else {
    sink(file_name)    
  }
  row <- ""
  for(i in 1:length(partitions)) {
    row <- paste(row,paste("Partition",i), sep=",")
  }
  writeLines(row)
  
  row <- "Average"
  for(i in 1:length(partitions)) {
    row <- paste(row, mean(lengths[[i]]), sep=",")
  }
  writeLines(row)
  
  row <- "Max"
  for(i in 1:length(partitions)) {
    row <- paste(row, max(lengths[[i]]), sep=",")
  }
  writeLines(row)
  
  sink()
  
  save(consecutives, file=paste(cluster_name,"_consecutives.RData", sep=""))
}