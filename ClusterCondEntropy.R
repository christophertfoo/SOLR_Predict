require('entropy')

load("Clusters.RData")

# Join on days
#   character vector for days from both
#   all <- unique(join vectors)
#   join <- all[which(all %in% other & all %in% first)]
#   for( j in join ) { match indexes from both }

outputMeanEntropy <- function(cluster_name, data, col_names, file_name, num_bins,type="kmeans") {
  clustering <- get(cluster_name)
  
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
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
  save(means, file=paste(cluster_name, "means.RData", sep="_"))
  save(discretized, file=paste(cluster_name, "discretized.RData", sep="_"))
}

col_names <- getColNames(merged)

num_bins <- 10
suffix <- "_cluster_cond_entropy.csv"

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputMeanEntropy(kmeans_name, solr_data, col_names, paste(kmeans_name, suffix, sep=""), num_bins)
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputMeanEntropy(haar_kmeans_name, haar_solr_data, col_names, paste(haar_kmeans_name, suffix, sep=""), num_bins)
  
  pam_name <- paste("pam",i,sep="_")
  outputMeanEntropy(pam_name, solr_data, col_names, paste(pam_name, suffix, sep=""), num_bins, type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputMeanEntropy(haar_pam_name, haar_solr_data, col_names, paste(haar_pam_name, suffix, sep=""), num_bins, type="pam")
}