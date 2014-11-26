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

num_pentads <- 6
num_groups <- ceiling(73 / num_pentads)

args <- commandArgs(TRUE)
solr_col <- 'SOLR_6'
if(length(args) > 0) {
  solr_col <- args[1]
}

P_Deseasonalized_Solr <- list()
K_Deseasonalized_Solr <- list()
S_Deseasonalized_Solr <- list()

for(pentad in 1:num_groups) {
  
  start <- 1 + (pentad - 1) * num_pentads 
  end <- (start + num_pentads - 1)
  
  for(hour in 0:23) {
    writeLines(paste("Pentad:", pentad, "- Hour:", hour))
    if(end > 73) {
      data <- deseasonalized_offset[which((deseasonalized_offset$PENTAD >= start | deseasonalized_offset$PENTAD <= end %% 73) & deseasonalized_offset$HR == hour),]
    } else {
      data <- deseasonalized_offset[which(deseasonalized_offset$PENTAD >= start & deseasonalized_offset$PENTAD <= end & deseasonalized_offset$HR == hour),]
    }   
    writeLines(paste("# valid rows:", as.character(nrow(data[data[[solr_col]] > 0,]))))
    if(nrow(data[data[[solr_col]] > 0,]) > 0) {    
      writeLines("  Pearson")
      result <- correlateNoMT(col=solr_col, data=data, selectedMethod="pearson")
      P_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Spearman")
      result <- correlateNoMT(col=solr_col, data=data, selectedMethod="spearman")
      S_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Kendall")
      result <- correlateNoMT(col=solr_col, data=data, selectedMethod="kendall")
      K_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]] <- result
    } 
  }
}

save(P_Deseasonalized_Solr, S_Deseasonalized_Solr, K_Deseasonalized_Solr, file="Solr_Deseasonalized_Correlations.RData")

return_dir <- getwd()
dir.create("Solr_Deseasonalized_Correlations")
setwd("Solr_Deseasonalized_Correlations")

dir.create("P")
setwd("P")
for(pentad in 1:num_groups) {
  writeCorrelationResult("Solr", "P", pentad, P_Deseasonalized_Solr)
}
setwd("..")

dir.create("S")
setwd("S")
for(pentad in 1:num_groups) {
  writeCorrelationResult("Solr", "S", pentad, S_Deseasonalized_Solr)
}
setwd("..")

dir.create("K")
setwd("K")
for(pentad in 1:num_groups) {
  writeCorrelationResult("Solr", "K", pentad, K_Deseasonalized_Solr)
}
setwd("..")
setwd(return_dir)