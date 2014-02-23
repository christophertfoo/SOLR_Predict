if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  }
  else {
    source("Deseasonalize.R")
  }
}

if(!exists("P_Deseasonalized_Solr") || !exists("S_Deseasonalized_Solr") || !exists("K_Deseasonalized_Solr")) {
  if(file.exists("Solr_Deseasonalized_Correlations.RData")) {
    writeLines("Loading Solr_Deseasonalized_Correlations.RData")
    load("Solr_Deseasonalized_Correlations.RData")
  }
  else {
    source("Solr_Deseasonalized_Correlations.R")
  }
}

source('SOLR_Predict.R')

feature_counts <- c(5, 10, 20, 30, 40, 50)
num_pentads <- 73
num_groups <- ceiling(73 / num_pentads)

for(num_features in feature_counts) {
  
  # Test SOLR
  
  SCBH1_P_Solr <- list()
  SCBH1_S_Solr <- list()
  SCBH1_K_Solr <- list()
  
  for(test_year in 2010:2013) {
    test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year,]
    training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
    for(pentad in 1:num_groups) {
      start <- 1 + (pentad - 1) * num_pentads
      for(hour in 0:23) {
        writeLines(paste("# of Features - ", num_features," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
        training_data <- training[training$PENTAD >= (start) & training$PENTAD <= (start + num_pentads - 1) & training$HR == hour,]
        test_data <- test[test$PENTAD >= (start) & test$PENTAD <= (start + num_pentads - 1) & test$HR == hour,]
        if(!is.null(P_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  P")
          if(is.null(SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()          
          }
          if(is.null(SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=P_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])
          SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(SCBH1_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
        }
        if(!is.null(S_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  S")
          if(is.null(SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
          }
          if(is.null(SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=S_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])
          SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(SCBH1_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
        }
        if(!is.null(K_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  K")
          if(is.null(SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
          }
          if(is.null(SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=K_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])
          SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(SCBH1_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
        }
        writeLines("")
      }
    }
  }
  
  return_dir <- getwd()
  dir.create("Solr_Deseasonalized_Results")
  setwd("Solr_Deseasonalized_Results")
  dir.create(as.character(num_features))
  setwd(as.character(num_features))
  
  dir.create("P")
  setwd("P")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_P_Solr_", i, "_Results.csv", sep=""), results=SCBH1_P_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_P_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_P_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("S")
  setwd("S")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_S_Solr_", i, "_Results.csv", sep=""), results=SCBH1_S_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_S_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_S_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("K")
  setwd("K")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_K_Solr_", i, "_Results.csv", sep=""), results=SCBH1_K_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_K_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_K_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  setwd(return_dir)
}