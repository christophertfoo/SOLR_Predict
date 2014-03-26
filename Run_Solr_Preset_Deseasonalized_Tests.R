if(!exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    writeLines("Loading Deseasonalized.RData")
    load("Deseasonalized.RData")
  } else {
    source("Deseasonalize.R")
  }
}

source('SOLR_Predict.R')

num_pentads <- 73 #c(12, 6, 4, 2, 1)
max_num_past <- 6
max_offset <- 6
num_groups <- ceiling(73 / num_pentads)
return_dir <- getwd()

# solr_stations <- c("KFWH1", "KTAH1", "MKRH1", "OFRH1", "PLHH1", "SCSH1", "WNVH1")
# for(num_pentads in merge_groups) {
#   num_groups <- ceiling(73 / num_pentads)
#   
#   return_dir <- getwd()
#   dir.create(paste("Merged_", num_pentads, sep=""))
#   setwd(paste("Merged_", num_pentads, sep=""))
#   dir.create("Solr_Preset_Results")
#   setwd("Solr_Preset_Results")
#   
#   
#   for(num_past in 1:max_num_past) {    
#     function_string <- character(0)
#     for(i in 1:num_past) {
#       if(i == max_offset) {
#         function_string <- paste(function_string, "+ SOLR")
#         for(station in solr_stations) {
#           function_string <- paste(function_string, paste(" + ", station,"_SOLR",sep=""))
#         }
#       } else if(i > 1) {        
#         function_string <- paste(function_string, "+ SOLR_", (max_offset - i), sep="")
#         for(station in solr_stations) {
#           function_string <- paste(function_string, paste(" + ", station,"_SOLR_", (max_offset - i),sep=""))
#         }
#       } else {
#         function_string <- paste("SOLR_", max_offset, " ~ SOLR_", (max_offset - 1), sep="")
#         for(station in solr_stations) {
#           function_string <- paste(function_string, paste(" + ", station,"_SOLR_", (max_offset - 1),sep=""))
#         }
#       }
#     }

# Test Solr

# 0 Neighbors
# function_string <- paste("SOLR_6 ~ ", paste("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", sep="+"), sep="")

# Q1 - 1 Neighbor
#function_string <- paste("SOLR_6 ~ ", paste("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "KTAH1_SOLR_5", "KTAH1_SOLR_4", "KTAH1_RELH_5", "KTAH1_RELH_4", "KTAH1_TMPF_5", "KTAH1_TMPF_4", "KTAH1_SKNT_5", "KTAH1_SKNT_4", "KTAH1_DRCT_5", "KTAH1_DRCT_4", sep="+"), sep="")

# Q2 - 1 Neighbor
# function_string <- paste("SOLR_6 ~ ", paste("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "PHHI_RELH_5", "PHHI_RELH_4", "PHHI_TMPF_5", "PHHI_TMPF_4", "PHHI_SKNT_5", "PHHI_SKNT_4", "PHHI_DRCT_5", "PHHI_DRCT_4", sep="+"), sep="")

# Q3 - 1 Neighbor
# function_string <- paste("SOLR_6 ~ ", paste("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "SCSH1_SOLR_5", "SCSH1_SOLR_4", "SCSH1_RELH_5", "SCSH1_RELH_4", "SCSH1_TMPF_5", "SCSH1_TMPF_4", "SCSH1_SKNT_5", "SCSH1_SKNT_4", "SCSH1_DRCT_5", "SCSH1_DRCT_4", sep="+"), sep="")

# Q4 - 1 Neighbor
function_string <- paste("SOLR_6 ~ ", paste("SOLR_5", "SOLR_4", "RELH_5", "RELH_4", "TMPF_5", "TMPF_4", "SKNT_5", "SKNT_4", "DRCT_5", "DRCT_4", "MKRH1_SOLR_5", "MKRH1_SOLR_4", "MKRH1_RELH_5", "MKRH1_RELH_4", "MKRH1_TMPF_5", "MKRH1_TMPF_4", "MKRH1_SKNT_5", "MKRH1_SKNT_4", "MKRH1_DRCT_5", "MKRH1_DRCT_4", sep="+"), sep="")

SCBH1_Results_Solr <- list()

for(test_year in 2010:2013) {
  test <- deseasonalized_offset[deseasonalized_offset$YEAR == test_year,]
  training <- deseasonalized_offset[deseasonalized_offset$YEAR != test_year,]
  for(pentad in 1:num_groups) {
    start <- 1 + (pentad - 1) * num_pentads
    for(hour in 0:23) {
      #       writeLines(paste("Num Merged - ", num_pentads, " Num Past - ", num_past," Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
      writeLines(paste("Num Merged - ", num_pentads, " Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
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
        SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolrDeseasonalized(SCBH1_Results_Solr[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6", deseasonalized_signal, "SOLR")
      }
      writeLines("")
    }
  }
}

# dir.create(as.character(num_past))
# setwd(as.character(num_past))
dir.create("Solr_Preset_Deseasonalized_Results")
setwd("Solr_Preset_Deseasonalized_Results")

save(SCBH1_Results_Solr, file="Solr_Preset_Deseasonalized_Results.RData")

dir.create("Averaged")
setwd("Averaged")
for(i in 1:num_groups) {
  writeTestResults(name=paste("SCBH1_Preset_Deseasonalized_Solr_", i, "_Results.csv", sep=""), results=SCBH1_Results_Solr, pentad=i)
}

setwd("..")
dir.create("Raw")
setwd("Raw")

for(i in 1:num_groups) {
  pentad_dir <- paste("Pentad_", i, sep="")
  dir.create(pentad_dir)
  setwd(pentad_dir)
  for(j in 0:23) {
    writeRawTestResults(name=paste("SCBH1_Preset_Solr_Deseasonalized_", i, "_", j, "_Raw_Results.csv", sep=""), results=SCBH1_Results_Solr, pentad=i, hour=j)
  }
  setwd("..")
}
setwd("../..")
# }
setwd(return_dir)
# }
