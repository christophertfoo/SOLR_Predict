require('cluster')

if(!exists("vectorized")) {
  if(file.exists("Vectorized.RData")) {
    writeLines("Loading Vectorized.RData")
    load("Vectorized.RData")
    source('SOLR_Predict.R')
  } else {
    source("VectorizeDays.R")
  }
}

min_clusters <- 4
max_clusters <- 10

to_save <- c("vectorized", "solr_data")

solr_cols <- character()
for(i in 0:23) {
  solr_cols <- c(solr_cols, paste("SOLR", i, sep="_"))
}
solr_data <- removeVectorizedNa(vectorized, c("day", solr_cols))
solr <- solr_data[,solr_cols]

for(i in min_clusters:max_clusters) {
  kmeans_name <- paste("kmeans",i,sep="_")
  pam_name <- paste("pam",i,sep="_")
  assign(kmeans_name, kmeans(solr, centers=i, nstart=5))
  assign(pam_name, pam(solr, i))
  
  outputKmeansResult(result=get(kmeans_name), data=solr, file_name=paste(kmeans_name,"csv", sep="."), type="kmeans")
  outputKmeansResult(result=get(pam_name), data=solr, file_name=paste(pam_name,"csv", sep="."), type="pam")
  
  
  to_save <- c(to_save, kmeans_name, pam_name)
}

save(list=to_save, file="Clusters.RData")