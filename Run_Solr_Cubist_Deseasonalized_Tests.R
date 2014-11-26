library("Cubist")

if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  } else {
    source("Deseasonalize.R")
  }
}

if(!exists("P_Deseasonalized_Solr") || !exists("S_Deseasonalized_Solr") || !exists("K_Deseasonalized_Solr")) {
  if(file.exists("Solr_Deseasonalized_Correlations.RData")) {
    writeLines("Loading Solr_Deseasonalized_Correlations.RData")
    load("Solr_Deseasonalized_Correlations.RData")
  } else {
    source("Solr_Deseasonalized_Correlations.R")
  }
}

source('SOLR_Predict.R')

feature_counts <- c(5, 10, 20, 30, 40, 50)
num_pentads <- 6
num_groups <- ceiling(73 / num_pentads)

test_years <- 2013 #2010:2013

args <- commandArgs(TRUE)

if(length(args) < 2) {
  writeLines("Usage: <station> <SOLR Col>")
}

station <- args[1]
solr_col <- args[2]

result <- list()

for(num_features in feature_counts) {
  
  # Test SOLR
  
  P_Solr <- list()
  S_Solr <- list()
  K_Solr <- list()
  
  for(test_year in test_years) {
    test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year,]
    training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
    for(pentad in 1:num_groups) {
      start <- 1 + (pentad - 1) * num_pentads
      end <- (start + num_pentads - 1)
      for(hour in 0:23) {
        writeLines(paste("# of Features - ", num_features," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
#         writeLines(paste("# of Features - ", num_features,", Pentad - ", pentad, ", Hour - ", hour, sep=""))
        
        if(end > 73) {
          training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73) & training$HR == hour,]
          test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73) & test$HR == hour,]
        } else {
          training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end & training$HR == hour,]
          test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end & test$HR == hour,]
        }
        if(nrow(test_data) > 0 && nrow(training_data) > 0) {
          if(!is.null(P_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  P")     
            if(is.null(P_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              P_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()          
            }
            
            #             [[as.character(test_year)]]
            
            if(is.null(P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, solr_col, num_features, results=P_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,solr_col])
            P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, solr_col, deseasonalized_signal, "SOLR")
          }
          if(!is.null(S_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  S")
            if(is.null(S_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              S_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, solr_col, num_features, results=S_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,solr_col])
            S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, solr_col, deseasonalized_signal, "SOLR")
          }
          if(!is.null(K_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])) {
            writeLines("  K")
            if(is.null(K_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              K_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]] <- as.vector(goodCorrelateTop(training_data, solr_col, num_features, results=K_Deseasonalized_Solr[[as.character(pentad)]][[as.character(hour)]])[["names"]])
            K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["columns"]]], y=training_data[,solr_col])
            K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, solr_col, deseasonalized_signal, "SOLR")
          }
        }
        writeLines("")
      }
    }
  }
  
  return_dir <- getwd()
  dir.create("Solr_Cubist_Deseasonalized_Results")
  setwd("Solr_Cubist_Deseasonalized_Results")
  dir.create(as.character(num_features))
  setwd(as.character(num_features))
  
  dir.create("P")
  setwd("P")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_P_Solr_", i, "_Results.csv", sep=""), results=P_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_P_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=P_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("S")
  setwd("S")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_S_Solr_", i, "_Results.csv", sep=""), results=S_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_S_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=S_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("K")
  setwd("K")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_Results.csv", sep=""), results=K_Solr, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=K_Solr, pentad=i, hour=j)
    }
    setwd("..")
  }
  setwd(return_dir)
  
  if(is.null(result[[as.character(num_features)]])) {
    result[[as.character(num_features)]] <- list()
  }
  result[[as.character(num_features)]][["P"]] <- P_Solr
  result[[as.character(num_features)]][["S"]] <- S_Solr
  result[[as.character(num_features)]][["K"]] <- K_Solr
}

save(result, file="Solr_Cubist_Deseasonalized_Results.RData")