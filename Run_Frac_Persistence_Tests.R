if(!exists("offset_solr_frac")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  } else {
    source("Load_Data.R")
  }
}

source('SOLR_Predict.R')

merge_groups = c(1, 2, 6, 12)
max_num_past <- 6
max_offset <- 6

for(num_pentads in merge_groups) {
  num_groups <- ceiling(73 / num_pentads)
  
  return_dir <- getwd()
  dir.create(paste("Merged_", num_pentads, sep=""))
  setwd(paste("Merged_", num_pentads, sep=""))
  dir.create("Frac_Persistence_Results")
  setwd("Frac_Persistence_Results")
  
  
  for(num_past in 1:max_num_past) {    
    function_string <- character(0)
    for(i in 1:num_past) {
      if(i == max_offset) {
        function_string <- paste(function_string, "+ SOLR_Frac")
      } else if(i > 1) {
        
        function_string <- paste(function_string, "+ SOLR_Frac_", (max_offset - num_past), sep="")
      } else {
        function_string <- paste("SOLR_Frac_", max_offset, " ~ SOLR_Frac_", (max_offset - 1), sep="")
      }
    }
    
    # Test Frac
    
    SCBH1_Results_Frac <- list()
    
    for(test_year in 2010:2013) {
      test <- offset_solr_frac[offset_solr_frac$YEAR == test_year,]
      training <- offset_solr_frac[offset_solr_frac$YEAR != test_year,]
      for(pentad in 1:num_groups) {
        start <- 1 + (pentad - 1) * num_pentads
        end <- (start + num_pentads - 1)
        for(hour in 0:23) {
          writeLines(paste("Num Merged - ", num_pentads, " Num Past - ", num_past," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
          if(end > 73) {
            training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73) & training$HR == hour,]
            test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73) & test$HR == hour,]
          } else {
            training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end & training$HR == hour,]
            test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end & test$HR == hour,]
          }

          if(nrow(training[training$SOLR_Frac_6 > 0,]) > 0) {
            if(is.null(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]])) {
              SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]] <- list()          
            }
            if(is.null(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- function_string
            SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
            SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrFrac(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6", "SOLR_MAX_6")
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
      writeTestResults(name=paste("SCBH1_Persistence_Frac_", i, "_Results.csv", sep=""), results=SCBH1_Results_Frac, pentad=i)
    }
    
    setwd("..")
    dir.create("Raw")
    setwd("Raw")
    
    for(i in 1:num_groups) {
      pentad_dir <- paste("Pentad_", i, sep="")
      dir.create(pentad_dir)
      setwd(pentad_dir)
      for(j in 0:23) {
        writeRawTestResults(name=paste("SCBH1_Persistence_Frac_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_Results_Frac, pentad=i, hour=j)
      }
      setwd("..")
    }
    setwd("../..")
  }
  setwd(return_dir)
}

