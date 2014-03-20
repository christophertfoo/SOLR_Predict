library("Cubist")

if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  } else {
    source("Deseasonalize.R")
  }
}

source('SOLR_Predict.R')

num_pentads <- 73
num_groups <- ceiling(73 / num_pentads)


# Test SOLR

SCBH1_Solr <- list()
columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4")

for(test_year in 2010:2013) {
  test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year,]
  training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
  for(pentad in 1:num_groups) {
    start <- 1 + (pentad - 1) * num_pentads
    for(hour in 0:23) {
      writeLines(paste(" Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
      training_data <- training[training$PENTAD >= (start) & training$PENTAD <= (start + num_pentads - 1) & training$HR == hour,]
      test_data <- test[test$PENTAD >= (start) & test$PENTAD <= (start + num_pentads - 1) & test$HR == hour,]
      if(nrow(test_data) > 0 && nrow(training_data) > 0) {        
        if(is.null(SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]])) {
          SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()          
        }
        if(is.null(SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
          SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
        }
        SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,columns], y=training_data[,"SOLR_6"])
        SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(SCBH1_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6", deseasonalized_signal, "SOLR")
        
      }
      writeLines("")
    }
  }
}

return_dir <- getwd()
dir.create("Solr_Preset_Cubist_Deseasonalized_Results")
setwd("Solr_Preset_Cubist_Deseasonalized_Results")

save(SCBH1_Solr, file="Solr_Preset_Cubist_Deseasonalized_Results.RData")

dir.create("Averaged")
setwd("Averaged")
for(i in 1:num_groups) {
  writeTestResults(name=paste("SCBH1_Preset_Cubist_Deseasonalized_Solr_", i, "_Results.csv", sep=""), results=SCBH1_Solr, pentad=i)
}

setwd("..")
dir.create("Raw")
setwd("Raw")

for(i in 1:num_groups) {
  pentad_dir <- paste("Pentad_", i, sep="")
  dir.create(pentad_dir)
  setwd(pentad_dir)
  for(j in 0:23) {
    writeRawTestResults(name=paste("SCBH1_Preset_Cubist_Deseasonalized_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_Solr, pentad=i, hour=j)
  }
  setwd("..")
}

setwd("../..")