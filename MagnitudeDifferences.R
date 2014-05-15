load("Data.RData")
load("Clusters.RData")
load("Haar.RData")

diff <- function(col) {
  col - mean(col, na.rm=T)
}

calculateDifferenceVectors <- function(partition) {
  return(apply(partition,2,diff))
}

getColNames <- function(merged) {
  ignore_list <- c("YEAR", "MON", "DAY", "HR", "MIN", "TMZN", "DT", "DT_NUM", "TIME")
  raw_cols <- setdiff(names(merged), ignore_list)
  
  cols <- new.env()
  for(i in 0:23) {
    for(col in raw_cols) {
      if(is.null(cols[[col]])) {
        cols[[col]] <- character(0)
      }
      cols[[col]] <- c(cols[[col]], paste(col,i,sep="_"))
    }
  }
  return(cols)
}

magnitude <- function(row) {
  sqrt(sum(row^2, na.rm=T))
}

outputDifferences <- function(clustering, data, col_names, file_name, type="kmeans") {
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
  
  partitions <- list()
  partition_cols <- setdiff(names(data), ignore_list)
  num_partitions <- nrow(centroids)
  headings <- "Feature"
  for(i in 1:num_partitions) {
    headings <- paste(headings, ",Partition ", i, sep="")
    partitions[[i]] <- as.data.frame(calculateDifferenceVectors(data[which(clusters == i), partition_cols]))
  }
  sink(file_name)
  writeLines(headings)
  for(col in ls(col_names)) {
    row <- col
    for(i in 1:num_partitions) {
      partition <- partitions[[i]]
      row <- paste(row, mean(apply(partition[,intersect(names(partition), col_names[[col]])], 1,magnitude), na.rm=T),sep=",")
    }
    writeLines(row)
  }
  sink()
}


col_names <- getColNames(merged)

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  writeLines(kmeans_name)
  outputDifferences(get(kmeans_name), solr_data, col_names, paste(kmeans_name, "_magdiff.csv", sep=""))
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  writeLines(haar_kmeans_name)
  outputDifferences(get(haar_kmeans_name), haar_solr_data, col_names, paste(haar_kmeans_name, "_magdiff.csv", sep=""))
  
  pam_name <- paste("pam",i,sep="_")
  writeLines(pam_name)  
  outputDifferences(get(pam_name), solr_data, col_names, paste(pam_name, "_magdiff.csv", sep=""), type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  writeLines(haar_pam_name)
  outputDifferences(get(haar_pam_name), haar_solr_data, col_names, paste(haar_pam_name, "_magdiff.csv", sep=""), type="pam")
}