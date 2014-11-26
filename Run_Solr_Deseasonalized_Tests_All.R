if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  } else {
    source("Deseasonalize.R")
  }
}

# if(!exists("P_Deseasonalized_Solr") || !exists("S_Deseasonalized_Solr") || !exists("K_Deseasonalized_Solr")) {
if(!exists("P_Deseasonalized_Solr") || !exists("S_Deseasonalized_Solr")) {
  if(file.exists("Solr_Deseasonalized_Correlations_All.RData")) {
    writeLines("Loading Solr_Deseasonalized_Correlations_All.RData")
    load("Solr_Deseasonalized_Correlations_All.RData")
  } else {
    source("Solr_Deseasonalized_Correlations_All.R")
  }
}

source('SOLR_Predict.R')

feature_counts <- c(5, 10, 20, 30, 40, 50)
num_pentads <- 73
num_groups <- ceiling(73 / num_pentads)

station <- 'SCSH1'

result <- list()

for(num_features in feature_counts) {
  
  # Test SOLR
  
  P_Solr <- list()
  S_Solr <- list()
  K_Solr <- list()
  
  for(test_year in 2010:2013) {
    test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year,]
    training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
    for(pentad in 1:num_groups) {
      start <- 1 + (pentad - 1) * num_pentads
      end <- (start + num_pentads - 1)
      writeLines(paste("# of Features - ", num_features," Test Data Year - ", test_year, ", Pentad - ", pentad, sep=""))
      if(end > 73) {
        training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73),]
        test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73),]
      } else {
        training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end,]
        test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end,]
      }
      if(nrow(test_data) > 0 && nrow(training_data) > 0) {
        if(!is.null(P_Deseasonalized_Solr[[as.character(pentad)]])) {
          writeLines("  P")
          if(is.null(P_Solr[[as.character(pentad)]])) {
            P_Solr[[as.character(pentad)]] <- list()          
          }
          if(is.null(P_Solr[[as.character(pentad)]][[as.character(test_year)]])) {
            P_Solr[[as.character(pentad)]][[as.character(test_year)]] <- list()
          }
          P_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=P_Deseasonalized_Solr[[as.character(pentad)]])
          P_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(P_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          for(hour in 0:23) {
            if(is.null(P_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              P_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            P_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(P_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]], test_data[test_data$HR == hour,], "SOLR_6", deseasonalized_signal, "SOLR")
          }        }
        if(!is.null(S_Deseasonalized_Solr[[as.character(pentad)]])) {
          writeLines("  S")
          if(is.null(S_Solr[[as.character(pentad)]])) {
            S_Solr[[as.character(pentad)]] <- list()
          }
          if(is.null(S_Solr[[as.character(pentad)]][[as.character(test_year)]])) {
            S_Solr[[as.character(pentad)]][[as.character(test_year)]] <- list()
          }
          S_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=S_Deseasonalized_Solr[[as.character(pentad)]])
          S_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(S_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          for(hour in 0:23) {
            if(is.null(S_Solr[[as.character(pentad)]][[as.character(hour)]])) {
              S_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
            }
            if(is.null(S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
              S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
            }
            S_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(S_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]], test_data[test_data$HR == hour,], "SOLR_6", deseasonalized_signal, "SOLR")
          }        }
        #         if(!is.null(K_Deseasonalized_Solr[[as.character(pentad)]])) {
        #           writeLines("  K")
        #           if(is.null(K_Solr[[as.character(pentad)]])) {
        #             K_Solr[[as.character(pentad)]] <- list()
        #           }
        #           if(is.null(K_Solr[[as.character(pentad)]][[as.character(test_year)]])) {
        #             K_Solr[[as.character(pentad)]][[as.character(test_year)]] <- list()
        #           }
        #           K_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_6", num_features, correlationResults=K_Deseasonalized_Solr[[as.character(pentad)]])
        #           K_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(K_Solr[[as.character(pentad)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
        #           for(hour in 0:23) {
        #             if(is.null(K_Solr[[as.character(pentad)]][[as.character(hour)]])) {
        #               K_Solr[[as.character(pentad)]][[as.character(hour)]] <- list()
        #             }
        #             if(is.null(K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
        #               K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
        #             }
        #             K_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(K_Solr[[as.character(pentad)]][[as.character(test_year)]][["model"]], test_data[test_data$HR == hour,], "SOLR_6", deseasonalized_signal, "SOLR")
        #           }
        #         }
      }
      writeLines("")
      
    }
  }
  
  return_dir <- getwd()
  dir.create("Solr_Deseasonalized_All_Results")
  setwd("Solr_Deseasonalized_All_Results")
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
    writeRawTestResults(name=paste(station, "_", num_features,"_P_Solr_", i, "_Raw_Results.csv", sep=""), results=P_Solr, pentad=i)      
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
    writeRawTestResults(name=paste(station, "_", num_features,"_S_Solr_", i, "_Raw_Results.csv", sep=""), results=S_Solr, pentad=i)
    setwd("..")
  }
  
  #   setwd("../..")
  #   dir.create("K")
  #   setwd("K")
  #   dir.create("Averaged")
  #   setwd("Averaged")
  #   for(i in 1:num_groups) {
  #     writeTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_Results.csv", sep=""), results=K_Solr, pentad=i)
  #   }
  #   
  #   setwd("..")
  #   dir.create("Raw")
  #   setwd("Raw")
  #   
  #   for(i in 1:num_groups) {
  #     pentad_dir <- paste("Pentad_", i, sep="")
  #     dir.create(pentad_dir)
  #     setwd(pentad_dir)
  #     for(j in 0:23) {
  #       writeRawTestResults(name=paste(station, "_", num_features,"_K_Solr_", i, "_Raw_Results.csv", sep=""), results=K_Solr, pentad=i, hour=j)
  #     }
  #     setwd("..")
  #   }
  setwd(return_dir)
  
  result[[as.character(num_features)]][["P"]] <- P_Solr
  result[[as.character(num_features)]][["S"]] <- S_Solr
  result[[as.character(num_features)]][["K"]] <- K_Solr
}

save(result, file="Solr_Deseasonalized_Single_Results.RData")