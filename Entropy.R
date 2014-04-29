require('entropy')

load("Clusters.RData")
load("Haar.RData")

outputEntropy <- function(clustering, data, file_name, num_bins,type="kmeans") {
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
  
  col_names <- setdiff(names(data), ignore_list)
  headings <- "Partition"
  for(col in col_names) {
    headings <- paste(headings, col, sep=",")
  }
  sink(file_name)
  writeLines(headings)
  for(i in 1:nrow(centroids)) {
    row <- as.character(i)
    partition <- data[which(clusters == i),]
    for(col in col_names) {
      range <- range(data[which(!is.na(data[[col]])),col])
      if(range[1] == range[2]) {
        row <- paste(row, 0, sep=",")
      } else {
        row <- paste(row, entropy(discretize(partition[which(!is.na(partition[[col]])),col], num_bins, range)), sep=",")
      }      
    }
    writeLines(row)
  }
  sink()
}

num_bins <- 20

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputEntropy(get(kmeans_name), solr_data, paste(kmeans_name, "_entropy.csv", sep=""), num_bins)
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputEntropy(get(haar_kmeans_name), haar_solr_data, paste(haar_kmeans_name, "_entropy.csv", sep=""), num_bins)
  
  pam_name <- paste("pam",i,sep="_")
  outputEntropy(get(pam_name), solr_data, paste(pam_name, "_entropy.csv", sep=""), num_bins, type="pam")

  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputEntropy(get(haar_pam_name), haar_solr_data, paste(haar_pam_name, "_entropy.csv", sep=""), num_bins, type="pam")
}