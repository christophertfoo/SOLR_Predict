require('wavelets')

load("Clusters.RData")
load("Haar.RData")

getClusterStats <- function(clustering, data, solr_cols, type="kmeans") {
  temp <- dwt(rep(0,16), filter='haar')
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  centroid_signals <- numeric(0)
  inner_dist <- numeric(0)

for(i in 1:nrow(centroids)) {
  centroid_signal <- as.numeric(centroids[i,solr_cols])
  centroid_signals <- rbind(centroid_signals, centroid_signal)
  actual <- data[which(clusters == i), solr_cols]
  for(j in 1:nrow(actual)) {      
    inner_dist <- c(inner_dist, dist(rbind(as.numeric(actual[j,]), centroid_signal)))
  }  
}

return(list(in_cluster=mean(inner_dist), bet_cluster=mean(as.numeric(dist(centroid_signals)))))
}

getCoefClusterStats <- function(coef_clustering, data, solr_cols, type="kmeans") {
  temp <- dwt(rep(0,16), filter='haar')
  if(type == "kmeans") {
    centroids <- coef_clustering$centers
    clusters <- coef_clustering$cluster
  } else if(type == "pam") {
    centroids <- coef_clustering$medoids
    clusters <- coef_clustering$clustering
  }
  
  centroid_signals <- numeric(0)
  inner_dist <- numeric(0)
  
  for(i in 1:nrow(centroids)) {
    temp@V$V4[1,1] <- centroids[i,1]
    temp@W$W4[1,1] <- centroids[i,2]
    temp@W$W3[1,1] <- centroids[i,3]
    temp@W$W3[2,1] <- centroids[i,4]
    temp@W$W2[1,1] <- centroids[i,5]
    temp@W$W2[2,1] <- centroids[i,6]
    temp@W$W2[3,1] <- centroids[i,7]
    temp@W$W2[4,1] <- centroids[i,8]
    temp@W$W1[1,1] <- centroids[i,9]
    temp@W$W1[2,1] <- centroids[i,10]
    temp@W$W1[3,1] <- centroids[i,11]
    temp@W$W1[4,1] <- centroids[i,12]
    temp@W$W1[5,1] <- centroids[i,13]
    temp@W$W1[6,1] <- centroids[i,14]
    temp@W$W1[7,1] <- centroids[i,15]
    temp@W$W1[8,1] <- centroids[i,16]
    
    centroid_signal <- idwt(temp)
    centroid_signals <- rbind(centroid_signals, centroid_signal)
    actual <- data[which(clusters == i), solr_cols]
    for(j in 1:nrow(actual)) {      
      inner_dist <- c(inner_dist, dist(rbind(as.numeric(actual[j,]), centroid_signal)))
    }  
  }
  return(list(in_cluster=mean(inner_dist), bet_cluster=mean(as.numeric(dist(centroid_signals)))))
}

solr_cols <- character(0)
for(i in 5:20) {
  solr_cols <- c(solr_cols, paste("SOLR",i,sep="_"))
}

  
sink("cluster_compare.csv")
writeLines("Cluster Setup,Average Inner Cluster Distance, Average Between Cluster Distance")
for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  result <- getClusterStats(get(kmeans_name), solr_data, solr_cols, "kmeans")
  writeLines(paste(kmeans_name, result$in_cluster, result$bet_cluster, sep=","))
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  result <- getCoefClusterStats(get(haar_kmeans_name), haar_solr_data, solr_cols, "kmeans")
  writeLines(paste(haar_kmeans_name, result$in_cluster, result$bet_cluster, sep=","))
  
  pam_name <- paste("pam",i,sep="_")
  result <- getClusterStats(get(pam_name), solr_data, solr_cols, "pam")
  writeLines(paste(pam_name, result$in_cluster, result$bet_cluster, sep=","))
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  result <- getCoefClusterStats(get(haar_pam_name), haar_solr_data, solr_cols, "pam")
  writeLines(paste(haar_pam_name, result$in_cluster, result$bet_cluster, sep=","))
  writeLines("")
}
sink()