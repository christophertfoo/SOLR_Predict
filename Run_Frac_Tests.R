if(!exists("offset_solr_frac")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  } else {
    source("Load_Data.R")
  }
}

if(!exists("P_Frac") || !exists("S_Frac") || !exists("K_Frac")) {
  if(file.exists("Frac_Correlations.RData")) {
    writeLines("Loading Frac_Correlations.RData")
    load("Frac_Correlations.RData")
  } else {
    source("Frac_Correlations.R")
  }
}

source('SOLR_Predict.R')

feature_counts <- c(5, 10, 20, 30, 40, 50)
num_pentads <- 12
num_groups <- ceiling(73 / num_pentads)

for(num_features in feature_counts) {
  # Test Fraction
  
  SCBH1_P_Frac <- list()
  SCBH1_S_Frac <- list()
  SCBH1_K_Frac <- list()
  
  for(test_year in 2010:2013) {
    test <- offset_solr_frac[offset_solr_frac$YEAR == test_year,]
    training <- offset_solr_frac[offset_solr_frac$YEAR != test_year,]
    for(pentad in 1:num_groups) {
      start <- 1 + (pentad - 1) * num_pentads
      end <- (start + num_pentads - 1)
      for(hour in 0:23) {
        if(end > 73) {
          training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73) & training$HR == hour,]
          test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73) & test$HR == hour,]
        } else {
          training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end & training$HR == hour,]
          test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end & test$HR == hour,]
        }
        writeLines(paste("# of Features - ", num_features," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
        if(!is.null(P_Frac[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  P")
          if(is.null(SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]] <- list()          
          }
          if(is.null(SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1
          SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_FRAC_6", num_features, correlationResults=P_Frac[[as.character(pentad)]][[as.character(hour)]])
          #        writeLines(paste("    ", SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]))
          SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrFrac(SCBH1_P_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_FRAC_6", "SOLR_MAX_6")
        }
        if(!is.null(S_Frac[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  S")
          if(is.null(SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]] <- list()
          }
          if(is.null(SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_FRAC_6", num_features, correlationResults=S_Frac[[as.character(pentad)]][[as.character(hour)]])
          #        writeLines(paste("    ", SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]))
          SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrFrac(SCBH1_S_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_FRAC_6", "SOLR_MAX_6")
        }
        if(!is.null(K_Frac[[as.character(pentad)]][[as.character(hour)]])) {
          writeLines("  K")
          if(is.null(SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]])) {
            SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]] <- list()          
          }
          if(is.null(SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
            SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
          }
          SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- makeTopFunction(training_data, "SOLR_FRAC_6", num_features, correlationResults=K_Frac[[as.character(pentad)]][[as.character(hour)]])
          #        writeLines(paste("    ", SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]))
          SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
          SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrFrac(SCBH1_K_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_FRAC_6", "SOLR_MAX_6")
        }
        writeLines("")
      }
    }
  }
  
  return_dir <- getwd()
  dir.create("Frac_Results")
  setwd("Frac_Results")
  dir.create(as.character(num_features))
  setwd(as.character(num_features))
  
  dir.create("P")
  setwd("P")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_P_Frac_", i, "_Results.csv", sep=""), results=SCBH1_P_Frac, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_P_Frac_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_P_Frac, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("S")
  setwd("S")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_S_Frac_", i, "_Results.csv", sep=""), results=SCBH1_S_Frac, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_S_Frac_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_S_Frac, pentad=i, hour=j)
    }
    setwd("..")
  }
  
  setwd("../..")
  dir.create("K")
  setwd("K")
  dir.create("Averaged")
  setwd("Averaged")
  for(i in 1:num_groups) {
    writeTestResults(name=paste("SCBH1_", num_features,"_K_Frac_", i, "_Results.csv", sep=""), results=SCBH1_K_Frac, pentad=i)
  }
  
  setwd("..")
  dir.create("Raw")
  setwd("Raw")
  
  for(i in 1:num_groups) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      writeRawTestResults(name=paste("SCBH1_", num_features,"_K_Frac_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_K_Frac, pentad=i, hour=j)
    }
    setwd("..")
  }
  setwd(return_dir)
}