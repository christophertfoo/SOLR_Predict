source('SOLR_Predict.R')

merge_groups = c(12)
max_num_past <- 1

if(!exists("offset_days")) {
  if(file.exists("DaysOffset.RData")) {
    load("DaysOffset.RDta")
  }
  else {
    if(!exists("merged")) {
      if(file.exists("Data.RData")) {
        writeLines("Loading Data.RData")
        load("Data.RData")
        source('SOLR_Predict.R')
      }
      else {
        source("Load_Data.R")
      }
    }
    
    offset_days <- merged
    for(i in 1:max_num_past) {
      offset_days <- offsetDay(i, "SOLR", offset_days)
    }
    save(offset_days, file="DaysOffset.RData")
  }
}

for(num_pentads in merge_groups) {
  num_groups <- ceiling(73 / num_pentads)
  
  return_dir <- getwd()
  dir.create(paste("Merged_", num_pentads, sep=""))
  setwd(paste("Merged_", num_pentads, sep=""))
  dir.create("Solr_Persistence_Day_Results")
  setwd("Solr_Persistence_Day_Results")
  
  
  for(num_past in 1:max_num_past) {    
    function_string <- character(0)
    for(i in 1:num_past) {
      if(i > 1) {
        
        function_string <- paste(function_string, "+ SOLR_DAY_", num_past, sep="")
      }
      else {
        function_string <- "SOLR ~ SOLR_DAY_1"
      }
    }
    
    # Test Solr
    
    SCBH1_Results_Solr <- list()
    
    for(test_year in 2010:2013) {
      test <- offset_days[offset_days$YEAR == test_year,]
      training <- offset_days[offset_days$YEAR != test_year,]
      for(pentad in 1:num_groups) {
        start <- 1 + (pentad - 1) * num_pentads
        for(hour in 0:23) {
          writeLines(paste("Num Merged - ", num_pentads, " Num Past - ", num_past," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
          training_data <- training[training$PENTAD >= (start) & training$PENTAD <= (start + num_pentads - 1) & training$HR == hour,]
          test_data <- test[test$PENTAD >= (start) & test$PENTAD <= (start + num_pentads - 1) & test$HR == hour,]
          if(nrow(training[training$SOLR_6 > 0,]) > 0) {
            if(is.null(SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()          
            }
            if(is.null(SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- function_string
            SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
            SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
          }
          writeLines("")
        }
      }
    }
    
    dir.create(as.character(num_past))
    setwd(as.character(num_past))
    
    dir.create("Averaged")
    setwd("Averaged")
    for(i in 1:num_groups) {
      writeTestResults(name=paste("SCBH1_Persistence_Solr_Day_", i, "_Results.csv", sep=""), results=SCBH1_Results_Solr, pentad=i)
    }
    
    setwd("..")
    dir.create("Raw")
    setwd("Raw")
    
    for(i in 1:num_groups) {
      pentad_dir <- paste("Pentad_", i, sep="")
      dir.create(pentad_dir)
      setwd(pentad_dir)
      for(j in 0:23) {
        writeRawTestResults(name=paste("SCBH1_Persistence_Solr_Day_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_Results_Solr, pentad=i, hour=j)
      }
      setwd("..")
    }
    setwd("../..")
  }
  setwd(return_dir)
}
