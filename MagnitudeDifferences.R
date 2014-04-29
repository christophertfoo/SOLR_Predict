load("Data.RData")
load("Clusters.RData")
load("Haar.RData")

calculateDifferenceVectors <- function(partition) {
  for(col in names(partition)) {
    mean <- mean(partition[[col]])
    if(!is.na(mean)) {
      partition[[col]] <- partition[[col]] - mean      
    }
  }
  return(partition)
}

getColNames <- function(merged) {
  ignore_list <- c("YEAR", "MON", "DAY", "HR", "TMZN", "DT", "DT_NUM", "TIME")
  raw_cols <- setdiff(names(merged), ignore_list)
  
  cols <- new.env()
  for(i in 1:23) {
    for(col in raw_cols) {
      if(is.null(cols[[col]])) {
        cols[[col]] <- character(0)
      }
      cols[[col]] <- c(cols[[col]], paste(col,i,sep="_"))
    }
  }
  return(cols)
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
    partitions <- c(partitions, list(calculateDifferenceVectors(data[which(clusters == i), partition_cols])))
  }
  print(length(partitions))
  sink(file_name)
  writeLines(headings)
  for(col in ls(col_names)) {
    row <- col
    for(i in 1:num_partitions) {
      partition <- partitions[[i]]
      magnitudes <- numeric(0)
      for(j in 1:nrow(partition)) {
        magnitude <- 0
        for(hr in col_names[[col]]) {
          if(!is.na(partition[j, hr])) {
            magnitude <- magnitude + (partition[j, hr]^2)
          }
        }
        magnitudes <- c(magnitudes, sqrt(magnitude))
      }
      row <- paste(row, mean(magnitudes),sep=",")
    }
    writeLines(row)
  }
  sink()
}


col_names <- getColNames(merged)

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputDifferences(get(kmeans_name), solr_data, col_names, paste(kmeans_name, "_magdiff.csv", sep=""))
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputDifferences(get(haar_kmeans_name), haar_solr_data, col_names, paste(haar_kmeans_name, "_magdiff.csv", sep=""))
  
  pam_name <- paste("pam",i,sep="_")
  outputDifferences(get(pam_name), solr_data, col_names, paste(pam_name, "_magdiff.csv", sep=""), type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputDifferences(get(haar_pam_name), haar_solr_data, col_names, paste(haar_pam_name, "_magdiff.csv", sep=""), type="pam")
}