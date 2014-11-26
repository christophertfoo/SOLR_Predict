library("Cubist")

if(!exists("offset_solr")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  } else {
    source("Load_Data.R")
  }
}

if(!exists("P_Solr") || !exists("S_Solr") || !exists("K_Solr")) {
  if(file.exists("Solr_Correlations.RData")) {
    writeLines("Loading Solr_Correlations.RData")
    load("Solr_Correlations.RData")
  } else {
    source("Solr_Correlations.R")
  }
}

source('SOLR_Predict.R')

feature_counts <- c(5, 10, 20, 30, 40, 50)
num_pentads <- 73
num_groups <- ceiling(73 / num_pentads)

station <- "SCSH1"

result <- list()

for(num_features in feature_counts) {
  
  # Test SOLR
  
  Station_P_Solr <- list()
  Station_S_Solr <- list()
  Station_K_Solr <- list()
  
  for(test_year in 2010:2013) {
    test <- offset_solr[offset_solr$YEAR == test_year,]
    training <- offset_solr[offset_solr$YEAR != test_year,]
    for(pentad in 1:num_groups) {
      start <- 1 + (pentad - 1) * num_pentads
      end <- (start + num_pentads - 1)
      for(hour in 0:23) {
        writeLines(paste("# of Features - ", num_features," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
        if(end > 73) {
          training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73) & training$HR == hour,]
          test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73) & test$HR == hour,]
        } else {
          training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end & training$HR == hour,]
          test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end & test$HR == hour,]
        }
        if(nrow(training_data) > 0 && nrow(test_data) > 0) {
          if(!is.null(P_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  P")
            if(is.null(Station_P_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              Station_P_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()          
            }
            if(is.null(Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, "SOLR_6", num_features, results=P_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,"SOLR_6"])
            Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(Station_P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
          }
          if(!is.null(S_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  S")
            if(is.null(Station_S_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              Station_S_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, "SOLR_6", num_features, results=S_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,"SOLR_6"])
            Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(Station_S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
          }
          if(!is.null(K_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  K")
            if(is.null(Station_K_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              Station_K_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, "SOLR_6", num_features, results=K_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,"SOLR_6"])
            Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(Station_K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
          }
          writeLines("")
        }
      }
    }
  }
  
  return_dir <- getwd()
  dir.create("Solr_Cubist_Results")
  setwd("Solr_Cubist_Results")
  dir.create(as.character(num_features))
  setwd(as.character(num_features))
  
  dir.create("P")
  setwd("P")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_P_Solr_", i, "_Results.csv", sep=""), results=Station_P_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_P_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=Station_P_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("S")
  setwd("S")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_S_Solr_", i, "_Results.csv", sep=""), results=Station_S_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_S_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=Station_S_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("K")
  setwd("K")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_Results.csv", sep=""), results=Station_K_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=Station_K_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  setwd(return_dir)
  
  if(is.null(result[[as.character(num_features)]])) {
    result[[as.character(num_features)]] <- list()
  }
  result[[as.character(num_features)]][["P"]] <- Station_P_Solr
  result[[as.character(num_features)]][["S"]] <- Station_S_Solr
  result[[as.character(num_features)]][["K"]] <- Station_K_Solr
}

save(result, file="Solr_Cubist_Results.RData")