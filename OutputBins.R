require('entropy')
source('ClusterHelpers.R')

load("Data.RData")
load("Clusters.RData")
load("Haar.RData")



outputBins <- function(cluster_name, means, col, file_name, num_bins,type="kmeans", filter_col=NULL, filter_min=NULL) {
  clustering <- get(cluster_name)
  
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  matched <- matchClusterings(means, clustering, type)
  
  num_partitions <- nrow(centroids)
  
  range <- range(matched[,col], na.rm=T)
  if(!is.null(filter_col) && !is.null(filter_min)) {
    data <- matched[which(!is.na(matched[[col]]) & matched[[filter_col]] >= filter_min), c('cluster', col)]
  } else {
    data <- matched[which(!is.na(matched[[col]])), c('cluster', col)]
  }
  frame <- NULL
  for(i in 1:num_partitions) {
    if(is.null(frame)) {
      discretized <- discretize(data[which(data$cluster == i), col], num_bins, range)
      frame <- data.frame(Ranges=names(discretized))
      frame[[paste("Partition", i)]] <- discretized
    } else {
      discretized <- discretize(data[which(data$cluster == i), col], num_bins, range)
      frame[[paste("Partition", i)]] <- discretized
    }
  }
  write.csv(frame, file=file_name, row.names = F)
}

col_names <- getColNames(merged)
means <- getMeans(solr_data, col_names)
haar_means <- getMeans(haar_solr_data, col_names)

num_bins <- 10

bin_col <- "DRCT"

filter_col <- NULL
filter_min <- NULL
file_suffix <- '_drct_filter_1.csv'

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputBins(kmeans_name, means, bin_col, paste(kmeans_name, file_suffix, sep=""), num_bins, filter_col=filter_col, filter_min=filter_min)
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputBins(haar_kmeans_name, haar_means, bin_col, paste(haar_kmeans_name, file_suffix, sep=""), num_bins, filter_col=filter_col, filter_min=filter_min)
  
  pam_name <- paste("pam",i,sep="_")
  outputBins(pam_name, means, bin_col, paste(pam_name, file_suffix, sep=""), num_bins, type="pam", filter_col=filter_col, filter_min=filter_min)
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputBins(haar_pam_name, haar_means, bin_col, paste(haar_pam_name, file_suffix, sep=""), num_bins, type="pam", filter_col=filter_col, filter_min=filter_min)
}