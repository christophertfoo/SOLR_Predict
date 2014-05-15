require('entropy')

load("Data.RData")
load("Clusters.RData")
load("Haar.RData")

averageHours <- function(row) {
  mean(row, na.rm=T)
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

outputMeanEntropy <- function(cluster_name, data, col_names, file_name, num_bins,type="kmeans") {
  clustering <- get(cluster_name)
  
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  ignore_list <- c("day")
  
  for(i in 0:23) {
    ignore_list <- c(ignore_list, paste("TIME", i, sep="_"), paste("MIN", i, sep="_"), paste("DT", i, sep="_"), paste("DT_NUM", i, sep="_"))
  }
  
  means <- data.frame(day=data$day)
  for(col in ls(col_names)) {
    means[[col]] <- apply(data[,intersect(names(data), col_names[[col]])], 1, averageHours)
  }
  
  partitions <- list()
  partition_cols <- setdiff(names(means), ignore_list)
  num_partitions <- nrow(centroids)
  headings <- "Feature"
  for(i in 1:num_partitions) {
    headings <- paste(headings, ",Partition ", i, sep="")
    partitions[[i]] <- as.data.frame(means[which(clusters == i), partition_cols])
  }
  headings <- paste(headings, "Average Entropy", "Standard Deviation", sep=",")
  discretized <- list()
  sink(file_name)
  writeLines(headings)
  for(col in ls(col_names)) {
    row <- col
    entropies <- numeric(0)
    for(i in 1:num_partitions) {
      partition <- partitions[[i]]
      range <- range(means[,col], na.rm=T)
      if(range[1] == range[2]) {
        row <- paste(row, 0, sep=",")
      } else {
        discretized[[as.character(i)]][[col]] <- discretize(partition[which(!is.na(partition[[col]])),col], num_bins, range)
        entropy_val <- entropy(discretized[[as.character(i)]][[col]])
        entropies <- c(entropies, entropy_val)
        row <- paste(row, entropy_val, sep=",")
      }
    }
    row <- paste(row, mean(entropies), sd(entropies), sep=",")
    writeLines(row)
  }
  
  
  sink()
  save(discretized, file=paste(cluster_name, "discretized.RData", sep="_"))
}

col_names <- getColNames(merged)

num_bins <- 10

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputMeanEntropy(kmeans_name, solr_data, col_names, paste(kmeans_name, "_mean_entropy.csv", sep=""), num_bins)
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputMeanEntropy(haar_kmeans_name, haar_solr_data, col_names, paste(haar_kmeans_name, "_mean_entropy.csv", sep=""), num_bins)
  
  pam_name <- paste("pam",i,sep="_")
  outputMeanEntropy(pam_name, solr_data, col_names, paste(pam_name, "_mean_entropy.csv", sep=""), num_bins, type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputMeanEntropy(haar_pam_name, haar_solr_data, col_names, paste(haar_pam_name, "_mean_entropy.csv", sep=""), num_bins, type="pam")
}