require('entropy')

source('ClusterHelpers.R')

load("Data.RData")
load("Clusters.RData")
load("Haar.RData")

# Mutual information with feature of day before and the next day cluster


getMI <- function(matched, col_1, col_2, nbin_1 = 10, nbin_2 = 10) {
  filtered <- matched[which(!is.na(matched[[col_1]]) & !is.nan(matched[[col_1]]) & !is.na(matched[[col_2]])  & !is.nan(matched[[col_2]])), c(col_1, col_2)]
  disc <- discretize2d(filtered[[col_1]], filtered[[col_2]], nbin_1, nbin_2)
  return(mi.plugin(disc))
}

getClusterMIString <- function(matched, col, nbins = 10) {
  row <- paste(col, getMI(matched, 'cluster', col, 5, nbins), sep=",")
  return(row)
}

outputMI <- function(means, clustering, col_names, file_name, noffset, type="kmeans") {
  clusters <- get(clustering)
  matched <- matchClusterings(means, clusters, type)
  names <- col_names
  for(i in 1:noffset) {
    matched <- getColOffset(matched, 'cluster', paste('cluster', i, sep="_"), i)
    names <- c(names, paste('cluster', i, sep="_"))
  }
  sink(file_name)
  writeLines("Feature,Mutual Information")
  for(col in names) {
    writeLines(getClusterMIString(matched, col))
  }
  sink()
}

# MAIN

noffset <- 1

col_names <- getColNames(merged)
means <- getMeans(solr_data, col_names)
means[['day']] <- strptime(as.character(means[,"day"]), '%Y-%m-%d')
means_names <- character(0)

for(i in 1:noffset) {
  for(col in setdiff(names(means), c('day'))) {
    means <- getColOffset(means, col, paste(col, i, sep="_"), i)
    means_names <- c(means_names, paste(col, i, sep="_"))
  }
}

haar_means <- getMeans(haar_solr_data, col_names)
haar_means[['day']] <- strptime(as.character(haar_means[,"day"]), '%Y-%m-%d')
haar_means_names <- character(0)

for(i in 1:noffset) {
  for(col in setdiff(names(haar_means), c('day'))) {
    haar_means <- getColOffset(haar_means, col, paste(col, i, sep="_"), i)
    haar_means_names <- c(haar_means_names, paste(col, i, sep="_"))
  }
}

num_bins <- 10

file_suffix <- '_mi.csv'

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputMI(means, kmeans_name, means_names, paste(kmeans_name, file_suffix, sep=""), noffset)
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputMI(haar_means, haar_kmeans_name, haar_means_names, paste(haar_kmeans_name, file_suffix, sep=""), noffset)  
  
  pam_name <- paste("pam",i,sep="_")
  outputMI(means, pam_name, means_names, paste(pam_name, file_suffix, sep=""), noffset, type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputMI(haar_means, haar_pam_name, haar_means_names, paste(haar_pam_name, file_suffix, sep=""), noffset, type="pam")
}