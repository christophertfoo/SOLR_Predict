require('cluster')
require('wavelets')

if(!exists("vectorized")) {
  if(file.exists("Vectorized.RData")) {
    writeLines("Loading Vectorized.RData")
    load("Vectorized.RData")
    source('SOLR_Predict.R')
  } else {
    source("VectorizeDays.R")
  }
}

solr_names <- character(0)
for(i in 0:23) {
  solr_names <- c(solr_names, paste("SOLR",i,sep="_"))
}

haar_solr_data <- removeVectorizedNa(vectorized, c("day", solr_names))

solr_names <- character(0)
for(i in 5:20) {
  solr_names <- c(solr_names, paste("SOLR",i,sep="_"))
}
solr <- haar_solr_data[,solr_names]
num_rows <- nrow(solr)
haar <- vector()
for(i in 1:num_rows) {
  writeLines(paste(i,num_rows,sep="/"))
  haar <- c(haar, dwt(as.numeric(solr[i,]), filter="haar"))
}

coef <- data.frame(v4=rep(NA,num_rows),w4=rep(NA,num_rows),w3_1=rep(NA,num_rows),w3_2=rep(NA,num_rows),w2_1=rep(NA,num_rows),w2_2=rep(NA,num_rows),w2_3=rep(NA,num_rows),w2_4=rep(NA,num_rows),w1_1=rep(NA,num_rows),w1_2=rep(NA,num_rows),w1_3=rep(NA,num_rows),w1_4=rep(NA,num_rows),w1_5=rep(NA,num_rows),w1_6=rep(NA,num_rows),w1_7=rep(NA,num_rows),w1_8=rep(NA,num_rows))
for(i in 1:num_rows) {
  writeLines(paste(i,num_rows,sep="/"))
  haar_result <- haar[[i]]
  coefficients <- c(haar_result@V$V4, haar_result@W$W4, haar_result@W$W3, haar_result@W$W2, floor(haar_result@W$W1 / 10) * 10)
  coef[i,] <- coefficients
}

to_save <- c("haar_solr_data", "haar", "coef")
for(i in 4:10) {
  kmeans_name <- paste("coef","kmeans",i,sep="_")
  pam_name <- paste("coef","pam",i,sep="_")
  assign(kmeans_name, kmeans(coef, centers=i))
  assign(pam_name, pam(coef, i))
  
  outputCoefKmeansResult(result=get(kmeans_name), data=solr, file_name=paste(kmeans_name,"csv", sep="."), type="kmeans")
  outputCoefKmeansResult(result=get(pam_name), data=solr, file_name=paste(pam_name,"csv", sep="."), type="pam")
  to_save <- c(to_save, kmeans_name, pam_name)
}
save(list=to_save, file="Haar.RData")