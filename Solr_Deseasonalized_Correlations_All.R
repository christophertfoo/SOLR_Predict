if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  } else {
    source("Deseasonalize.R")
  }
}

source('SOLR_Predict.R')

# Get SOLR correlations

writeLines("Calculating Solr Deseasonalized Correlations")

num_pentads <- 73
num_groups <- ceiling(73 / num_pentads)

P_Deseasonalized_Solr <- list()
K_Deseasonalized_Solr <- list()
S_Deseasonalized_Solr <- list()

station <- "SCSH1"

for(pentad in 1:num_groups) {
  
  start <- 1 + (pentad - 1) * num_pentads    
  end <- (start + num_pentads - 1)
  
  writeLines(paste("Pentad:", pentad))
  if(end > 73) {
    data <- deseasonalized_offset[which((deseasonalized_offset$PENTAD >= start | deseasonalized_offset$PENTAD <= end %% 73)),]
  } else {
    data <- deseasonalized_offset[which(deseasonalized_offset$PENTAD >= start & deseasonalized_offset$PENTAD <= end),]
  } 
  writeLines(paste("# valid rows:", as.character(nrow(data[data$SOLR_6 > 0,]))))
  if(nrow(data[data$SOLR_6 > 0,]) > 0) {    
    writeLines("  Pearson")
    result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="pearson")
    P_Deseasonalized_Solr[[as.character(pentad)]] <- result
    
    writeLines("  Spearman")
    result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="spearman")
    S_Deseasonalized_Solr[[as.character(pentad)]] <- result
    
#     writeLines("  Kendall")
#     result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="kendall")
#     K_Deseasonalized_Solr[[as.character(pentad)]] <- result
  } 
  
}

# save(P_Deseasonalized_Solr, S_Deseasonalized_Solr, K_Deseasonalized_Solr, file="Solr_Deseasonalized_Correlations_All.RData")
save(P_Deseasonalized_Solr, S_Deseasonalized_Solr, file="Solr_Deseasonalized_Correlations_All.RData")

return_dir <- getwd()
dir.create("Solr_Deseasonalized_Correlations_All")
setwd("Solr_Deseasonalized_Correlations_All")

dir.create("P")
setwd("P")
for(pentad in 1:num_groups) {
  write.csv(P_Deseasonalized_Solr[[as.character(pentad)]], file=paste(paste(station,"Solr_P",pentad, sep="_"), ".csv", sep=""), row.names=F)
}
setwd("..")

dir.create("S")
setwd("S")
for(pentad in 1:num_groups) {
  write.csv(S_Deseasonalized_Solr[[as.character(pentad)]], file=paste(paste(station,"Solr_S",pentad, sep="_"), ".csv", sep=""), row.names=F)
}
setwd("..")

# dir.create("K")
# setwd("K")
# for(pentad in 1:num_groups) {
#   write.csv(K_Deseasonalized_Solr[[as.character(pentad)]], file=paste(paste(station,"Solr_K",pentad, sep="_"), ".csv", sep=""), row.names=F)
# }
# setwd("..")
setwd(return_dir)