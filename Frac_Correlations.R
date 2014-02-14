source('SOLR_Predict.R')

if(is.null(offset_solr_frac)) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  }
  else {
    source("Load_Data.R")
  }
}

# Get Fraction correlations

writeLines("Calculating Fraction Correlations")

P_Frac <- list()
K_Frac <- list()
S_Frac <- list()

for(pentad in 17:73) {
  for(hour in 0:23) {
    writeLines(paste("Pentad:", pentad, "- Hour:", hour))
    data <- offset_solr_frac[offset_solr_frac$PENTAD == pentad & offset_solr_frac$HR == hour,]
    writeLines(paste("# valid rows:", as.character(nrow(data[data$SOLR_FRAC_6 > 0,]))))
    if(nrow(data[data$SOLR_FRAC_6 > 0,]) > 0) {    
      writeLines("  Pearson")
      result <- correlateNoMT(col="SOLR_FRAC_6", data=data, selectedMethod="pearson")
      P_Frac[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Spearman")
      result <- correlateNoMT(col="SOLR_FRAC_6", data=data, selectedMethod="spearman")
      S_Frac[[as.character(pentad)]][[as.character(hour)]] <- result
      
      writeLines("  Kendall")
      result <- correlateNoMT(col="SOLR_FRAC_6", data=data, selectedMethod="kendall")
      K_Frac[[as.character(pentad)]][[as.character(hour)]] <- result
    } 
  }
}

save(P_Solr, S_Solr, K_Solr, file="Frac_Correlations.RData")

return_dir <- getwd()
dir.create("Frac_Correlations")
setwd("Frac_Correlations")

dir.create("P")
setwd("P")
for(pentad in 1:73) {
  writeCorrelationResult("SCBH1_Frac", "P", pentad, P_Frac)
}
setwd("..")

dir.create("S")
setwd("S")
for(pentad in 1:73) {
  writeCorrelationResult("SCBH1_Frac", "S", pentad, S_Frac)
}
setwd("..")

dir.create("K")
setwd("K")
for(pentad in 1:73) {
  writeCorrelationResult("SCBH1_Frac", "K", pentad, K_Frac)
}
setwd("..")
setwd(return_dir)