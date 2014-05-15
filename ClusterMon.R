load("Clusters.RData")
load("Haar.RData")

outputYear <- function(data, clustering, file_name, type="kmeans") {
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  years <- strptime(as.character(data[,"day"]), '%Y-%m-%d')$year
  
  year_range <- range(years)
  
  npartition <- nrow(centroids)
  
  sink(file_name)
  
  row <- "Year"
  for(i in 1:npartition) {
    row <- paste(row,paste("Partition",i),sep=",")
  }
  writeLines(row)
  
  for(year in year_range[1]:year_range[2]) {
    row <- as.character(year + 1900)
    for(i in 1:npartition) {
      row <- paste(row, length(years[which(clusters == i & years == year)]),sep=",")
    }
    writeLines(row)
  }
  sink()
}

outputMon <- function(data, clustering, file_name, type="kmeans") {
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  months <- strptime(as.character(data[,"day"]), '%Y-%m-%d')$mon
  
  month_range <- range(months)
  
  npartition <- nrow(centroids)
  
  sink(file_name)
  
  row <- "Month"
  for(i in 1:npartition) {
    row <- paste(row,paste("Partition",i),sep=",")
  }
  writeLines(row)
  
  for(month in month_range[1]:month_range[2]) {
    row <- as.character(month + 1)
    for(i in 1:npartition) {
      row <- paste(row, length(months[which(clusters == i & months == month)]),sep=",")
    }
    writeLines(row)
  }
  sink()
}

for(i in 4:10) {
  kmeans_name <- paste("kmeans",i,sep="_")  
  outputMon(solr_data, get(kmeans_name), paste(kmeans_name, "_months.csv", sep=""))
  outputYear(solr_data, get(kmeans_name), paste(kmeans_name, "_years.csv", sep=""))
  
  haar_kmeans_name <- paste("coef_kmeans",i,sep="_")
  outputMon(haar_solr_data, get(haar_kmeans_name), paste(haar_kmeans_name, "_months.csv", sep=""))
  outputYear(haar_solr_data, get(haar_kmeans_name), paste(haar_kmeans_name, "_years.csv", sep=""))
  
  pam_name <- paste("pam",i,sep="_")
  outputMon(solr_data, get(pam_name), paste(pam_name, "_months.csv", sep=""), type="pam")
  outputYear(solr_data, get(pam_name), paste(pam_name, "_years.csv", sep=""), type="pam")
  
  haar_pam_name <- paste("coef_pam",i,sep="_")
  outputMon(haar_solr_data, get(haar_pam_name), paste(haar_pam_name, "_months.csv", sep=""), type="pam")
  outputYear(haar_solr_data, get(haar_pam_name), paste(haar_pam_name, "_years.csv", sep=""), type="pam")
}