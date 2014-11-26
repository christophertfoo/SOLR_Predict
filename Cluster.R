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

col_names <- c("SKNT_N", "SKNT_E")
identifier <- paste(col_names, collapse="_")

to_save <- c(paste("data", identifier, sep="_"))

cols <- character()
for(col in col_names) {
  for(i in 0:23) {
    cols <- c(cols, paste(col, i, sep="_"))
  }
}

data <- removeVectorizedNa(vectorized, c("day", cols))
prec_cols <- cols[grep("PREC", cols)]
for(col in prec_cols) {
  data <- data[which(data[[col]] < 4),]
}
filtered <- data[,cols]



for(i in min_clusters:max_clusters) {
  kmeans_name <- paste("kmeans", identifier, i ,sep="_")
  pam_name <- paste("pam", identifier, i, sep="_")
  assign(kmeans_name, kmeans(filtered, centers=i, nstart=5))
  assign(pam_name, pam(filtered, i))
  
  outputKmeansResult(result=get(kmeans_name), data=filtered, file_name=paste(kmeans_name,"csv", sep="."), type="kmeans")
  outputKmeansResult(result=get(pam_name), data=filtered, file_name=paste(pam_name,"csv", sep="."), type="pam")
  colors = rainbow(i)
  png(paste(kmeans_name, ".png", sep=""))
  plot(get(kmeans_name)$centers[1,], ylim=range(get(kmeans_name)$centers), type="b", col=colors[1], lwb=2, xlab="Hour", ylab=identifier, main=kmeans_name)
  for(j in 2:i) {
    lines(get(kmeans_name)$centers[j,], type="b", col=colors[j], lwd=2)
  }
  dev.off()
  
  png(paste(pam_name, ".png", sep=""))
  plot(get(pam_name)$medoids[1,], ylim=range(get(pam_name)$medoids), type="b", col=colors[1], lwb=2, xlab="Hour", ylab=identifier, main=pam_name)
  for(j in 2:i) {
    lines(get(pam_name)$medoids[j,], type="b", col=colors[j], lwd=2)
  }
  dev.off()
  
  to_save <- c(to_save, kmeans_name, pam_name)
}

assign(paste("data", identifier, sep="_"), data)
save(list=to_save, file=paste("Clusters_", identifier, ".RData", sep = ""))