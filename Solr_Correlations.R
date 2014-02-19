if(!exists("offset_solr")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  }
  else {
    source("Load_Data.R")
  }
}

source('SOLR_Predict.R')

# Get SOLR correlations

writeLines("Calculating Solr Correlations")

num_pentads <- 12
num_groups <- ceiling(73 / num_pentads)

P_Solr <- list()
K_Solr <- list()
S_Solr <- list()

for(pentad in 1:num_groups) {
  
  start <- 1 + (pentad - 1) * num_pentads  
  
  for(hour in 0:23) {
    writeLines(paste("Pentad:", pentad, "- Hour:", hour))
    data <- offset_solr[offset_solr$PENTAD >= start & offset_solr$PENTAD <= (start + num_pentads - 1) & offset_solr$HR == hour,]
    writeLines(paste("# valid rows:", as.character(nrow(data[data$SOLR_6 > 0,]))))
    if(nrow(data[data$SOLR_6 > 0,]) > 0) {    
      writeLines("  Pearson")
      result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="pearson")
      P_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Spearman")
      result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="spearman")
      S_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Kendall")
      result <- correlateNoMT(col="SOLR_6", data=data, selectedMethod="kendall")
      K_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
    } 
  }
}

save(P_Solr, S_Solr, K_Solr, file="Solr_Correlations.RData")

return_dir <- getwd()
dir.create("Solr_Correlations")
setwd("Solr_Correlations")

dir.create("P")
setwd("P")
for(pentad in 1:num_groups) {
  writeCorrelationResult("SCBH1_Solr", "P", pentad, P_Solr)
}
setwd("..")

dir.create("S")
setwd("S")
for(pentad in 1:num_groups) {
  writeCorrelationResult("SCBH1_Solr", "S", pentad, S_Solr)
}
setwd("..")

dir.create("K")
setwd("K")
for(pentad in 1:num_groups) {
  writeCorrelationResult("SCBH1_Solr", "K", pentad, K_Solr)
}
setwd("..")
setwd(return_dir)