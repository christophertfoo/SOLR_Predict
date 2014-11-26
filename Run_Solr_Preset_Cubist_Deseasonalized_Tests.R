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

Results <- list()

# 0 Neighbor
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4")

# Q1 - 1 Neighbor
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "KTAH1_SOLR_5", "KTAH1_SOLR_4", "KTAH1_RELH_5", "KTAH1_RELH_4", "KTAH1_TMPF_5", "KTAH1_TMPF_4", "KTAH1_SKNT_5", "KTAH1_SKNT_4", "KTAH1_DRCT_5", "KTAH1_DRCT_4")

# Q2 - 1 Neighbor
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "PHHI_RELH_5", "PHHI_RELH_4", "PHHI_TMPF_5", "PHHI_TMPF_4", "PHHI_SKNT_5", "PHHI_SKNT_4", "PHHI_DRCT_5", "PHHI_DRCT_4")

# Q3 - 1 Neighbor
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "SCSH1_SOLR_5", "SCSH1_SOLR_4", "SCSH1_RELH_5", "SCSH1_RELH_4", "SCSH1_TMPF_5", "SCSH1_TMPF_4", "SCSH1_SKNT_5", "SCSH1_SKNT_4", "SCSH1_DRCT_5", "SCSH1_DRCT_4")

# Q4 - 1 Neighbor
columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "MKRH1_SOLR_5", "MKRH1_SOLR_4", "MKRH1_RELH_5", "MKRH1_RELH_4", "MKRH1_TMPF_5", "MKRH1_TMPF_4", "MKRH1_SKNT_5", "MKRH1_SKNT_4", "MKRH1_DRCT_5", "MKRH1_DRCT_4")

# Combined w/ DRCT 
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4","KTAH1_SOLR_5", "KTAH1_SOLR_4", "KTAH1_RELH_5", "KTAH1_RELH_4", "KTAH1_TMPF_5", "KTAH1_TMPF_4", "KTAH1_SKNT_5", "KTAH1_SKNT_4", "KTAH1_DRCT_5", "KTAH1_DRCT_4", "PHHI_RELH_5", "PHHI_RELH_4", "PHHI_TMPF_5", "PHHI_TMPF_4", "PHHI_SKNT_5", "PHHI_SKNT_4", "PHHI_DRCT_5", "PHHI_DRCT_4", "SCSH1_SOLR_5", "SCSH1_SOLR_4", "SCSH1_RELH_5", "SCSH1_RELH_4", "SCSH1_TMPF_5", "SCSH1_TMPF_4", "SCSH1_SKNT_5", "SCSH1_SKNT_4", "SCSH1_DRCT_5", "SCSH1_DRCT_4", "MKRH1_SOLR_5", "MKRH1_SOLR_4", "MKRH1_RELH_5", "MKRH1_RELH_4", "MKRH1_TMPF_5", "MKRH1_TMPF_4", "MKRH1_SKNT_5", "MKRH1_SKNT_4", "MKRH1_DRCT_5", "MKRH1_DRCT_4")

# Combined No DRCT
# columns <- c("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "KTAH1_SOLR_5", "KTAH1_SOLR_4", "KTAH1_RELH_5", "KTAH1_RELH_4", "KTAH1_TMPF_5", "KTAH1_TMPF_4", "KTAH1_SKNT_5", "KTAH1_SKNT_4",  "PHHI_RELH_5", "PHHI_RELH_4", "PHHI_TMPF_5", "PHHI_TMPF_4", "PHHI_SKNT_5", "PHHI_SKNT_4", "SCSH1_SOLR_5", "SCSH1_SOLR_4", "SCSH1_RELH_5", "SCSH1_RELH_4", "SCSH1_TMPF_5", "SCSH1_TMPF_4", "SCSH1_SKNT_5", "SCSH1_SKNT_4", "MKRH1_SOLR_5", "MKRH1_SOLR_4", "MKRH1_RELH_5", "MKRH1_RELH_4", "MKRH1_TMPF_5", "MKRH1_TMPF_4", "MKRH1_SKNT_5", "MKRH1_SKNT_4")


# 2 Hour Lead

# 0 Neighbor
# columns <- c("SOLR_4", "SOLR_3", "RELH_4", "RELH_3", "TMPF_4", "TMPF_3", "SKNT_4", "SKNT_3", "DRCT_4", "DRCT_3")

# Q1 - 1 Neighbor
# columns <- c("SOLR_4", "SOLR_3", "RELH_4", "RELH_3", "TMPF_4", "TMPF_3", "SKNT_4", "SKNT_3", "DRCT_4", "DRCT_3", "KTAH1_SOLR_4", "KTAH1_SOLR_3", "KTAH1_RELH_4", "KTAH1_RELH_3", "KTAH1_TMPF_4", "KTAH1_TMPF_3", "KTAH1_SKNT_4", "KTAH1_SKNT_3", "KTAH1_DRCT_4", "KTAH1_DRCT_3")

# Q2 - 1 Neighbor
# columns <- c("SOLR_4", "SOLR_3", "RELH_4", "RELH_3", "TMPF_4", "TMPF_3", "SKNT_4", "SKNT_3", "DRCT_4", "DRCT_3", "PHHI_RELH_4", "PHHI_RELH_3", "PHHI_TMPF_4", "PHHI_TMPF_3", "PHHI_SKNT_4", "PHHI_SKNT_3", "PHHI_DRCT_4", "PHHI_DRCT_3")

# Q3 - 1 Neighbor
# columns <- c("SOLR_4", "SOLR_3", "RELH_4", "RELH_3", "TMPF_4", "TMPF_3", "SKNT_4", "SKNT_3", "DRCT_4", "DRCT_3", "SCSH1_SOLR_4", "SCSH1_SOLR_3", "SCSH1_RELH_4", "SCSH1_RELH_3", "SCSH1_TMPF_4", "SCSH1_TMPF_3", "SCSH1_SKNT_4", "SCSH1_SKNT_3", "SCSH1_DRCT_4", "SCSH1_DRCT_3")

# Q4 - 1 Neighbor
# columns <- c("SOLR_4", "SOLR_3", "RELH_4", "RELH_3", "TMPF_4", "TMPF_3", "SKNT_4", "SKNT_3", "DRCT_4", "DRCT_3", "MKRH1_SOLR_4", "MKRH1_SOLR_3", "MKRH1_RELH_4", "MKRH1_RELH_3", "MKRH1_TMPF_4", "MKRH1_TMPF_3", "MKRH1_SKNT_4", "MKRH1_SKNT_3", "MKRH1_DRCT_4", "MKRH1_DRCT_3")


for(test_year in 2013:2013) {
  test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year & deseasonalized_offset$MON == 1 & deseasonalized_offset$DAY == 15, ]
  training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year & deseasonalized_offset$MON != 1 & deseasonalized_offset$DAY != 15,]
#   test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year, ]
#   training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
  for(pentad in 1:num_groups) {
    start <- 1 + (pentad - 1) * num_pentads
    end <- (start + num_pentads - 1)
    for(hour in 0:23) {
      writeLines(paste(" Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
      if(end > 73) {
        training_data <- training[(training$PENTAD >= (start) | training$PENTAD <= end %% 73) & training$HR == hour,]
        test_data <- test[(test$PENTAD >= (start) | test$PENTAD <= end %% 73) & test$HR == hour,]
      } else {
        training_data <- training[training$PENTAD >= (start) & training$PENTAD <= end & training$HR == hour,]
        test_data <- test[test$PENTAD >= (start) & test$PENTAD <= end & test$HR == hour,]
      }
      if(nrow(test_data) > 0 && nrow(training_data) > 0) {        
        if(is.null(Results[[as.character(pentad)]][[as.character(hour)]])) {
          Results[[as.character(pentad)]][[as.character(hour)]] <- list()          
        }
        if(is.null(Results[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
          Results[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
        }
        Results[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- cubist(x=training_data[,columns], y=training_data[,"SOLR_6"])
        Results[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(Results[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6", deseasonalized_signal, "SOLR")
        
      }
      writeLines("")
    }
  }
}

return_dir <- getwd()
dir.create("Solr_Preset_Cubist_Deseasonalized_Results")
setwd("Solr_Preset_Cubist_Deseasonalized_Results")

save(Results, file="Solr_Preset_Cubist_Deseasonalized_Results.RData")

dir.create("Averaged")
setwd("Averaged")
for(i in 1:num_groups) {
  writeTestResults(name=paste("SCBH1_Preset_Cubist_Deseasonalized_Solr_", i, "_Results.csv", sep=""), results=Results, pentad=i)
}

setwd("..")
dir.create("Raw")
setwd("Raw")

for(i in 1:num_groups) {
  pentad_dir <- paste("Pentad_", i, sep="")
  dir.create(pentad_dir)
  setwd(pentad_dir)
  for(j in 0:23) {
    writeRawTestResults(name=paste("SCBH1_Preset_Cubist_Deseasonalized_Solr_", i, "_", j, "_Raw_Results.csv", sep=""), results=Results, pentad=i, hour=j)
  }
  setwd("..")
}

setwd("..")
dir.create("Coefficients")
setwd("Coefficients")

for(i in 1:num_groups) {
  if(!is.null(Results[[as.character(i)]])) {
    pentad_dir <- paste("Pentad_", i, sep="")
    dir.create(pentad_dir)
    setwd(pentad_dir)
    for(j in 0:23) {
      if(!is.null(Results[[as.character(i)]][[as.character(j)]])) {
        dir.create(as.character(j))
        setwd(as.character(j))
        
        writeCubistCoefficients(paste("Coefficients_Pentad-",i,"_Hour-",j,".csv", sep=""), Results, i, j)
        setwd("..")
      }
    }
    setwd("..")
  }
  setwd("..")
}

setwd("..")
setwd(return_dir)