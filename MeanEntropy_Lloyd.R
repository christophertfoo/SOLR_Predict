require('entropy')

source('ClusterHelpers.R')

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
  
  # Calculate means for each feature
  bins <- list()
  means <- data.frame(day=data$day)
  for(col in ls(col_names)) {
    writeLines(paste("    ", col))
    means[[col]] <- apply(data[,intersect(names(data), col_names[[col]])], 1, averageHours)
    bins[[col]] <- lloydMinMax(means[[col]], num_bins)
  }
  
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
  counts <- list()
  

  sink(file_name)
  writeLines(headings)
  for(col in ls(col_names)) {
    row <- col
    entropies <- numeric(0)
    for(i in 1:num_partitions) {
      partition <- partitions[[i]]
      discretized[[as.character(i)]][[col]] <- lloydDiscretize(partition[which(!is.na(partition[[col]])),col], bins[[col]])
      entropy_val <- probs[[i]] * entropy(discretized[[as.character(i)]][[col]])
      entropies <- c(entropies, entropy_val)
      row <- paste(row, entropy_val, sep=",")      
      counts[[as.character(i)]][[col]] <- countDiscretized(discretized[[as.character(i)]][[col]], bins[[col]])
    }
    row <- paste(row, sum(entropies), sd(entropies), sep=",")
    writeLines(row)
  }
  
  
  sink()
  save(means, file=paste(cluster_name, "means.RData", sep="_"))
  save(discretized, bins, counts, file=paste(cluster_name, "discretized_lloyd.RData", sep="_"))
}

col_names <- getColNames(merged)

num_bins <- 10

suffix <- "_lloyd_mean_entropy.csv"

for(i in 4:10) {
  writeLines(paste("kmeans", i, sep="_"))
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputMeanEntropy(kmeans_name, solr_data, col_names, paste(kmeans_name, suffix, sep=""), num_bins)
  
#   writeLines(paste("coef_kmeans", i, sep="_"))
#   haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
#   outputMeanEntropy(haar_kmeans_name, haar_solr_data, col_names, paste(haar_kmeans_name, suffix, sep=""), num_bins)
  
  writeLines(paste("pam", i, sep="_"))
  pam_name <- paste("pam",i,sep="_")
  outputMeanEntropy(pam_name, solr_data, col_names, paste(pam_name, suffix, sep=""), num_bins, type="pam")
  
#   writeLines(paste("coef_pam", i, sep="_"))
#   haar_pam_name <- paste("coef_pam",i,sep="_")
#   outputMeanEntropy(haar_pam_name, haar_solr_data, col_names, paste(haar_pam_name, suffix, sep=""), num_bins, type="pam")
}