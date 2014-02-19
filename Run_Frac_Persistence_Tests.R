if(!exists("offset_solr_frac")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
  }
  else {
    source("Load_Data.R")
  }
}

num_pentads <- 12
num_groups <- ceiling(73 / num_pentads)
num_past <- 1

function_string <- "SOLR_6 ~ SOLR_5"

# Test Frac

SCBH1_Results_Frac <- list()

for(test_year in 2010:2013) {
  test <- offset_solr_frac[offset_solr_frac$YEAR == test_year,]
  training <- offset_solr_frac[offset_solr_frac$YEAR != test_year,]
  for(pentad in 1:num_groups) {
    start <- 1 + (pentad - 1) * num_pentads
    for(hour in 0:23) {
      writeLines(paste("Test Data Year - ", test_year, ", Pentad - ", pentad, ", Hour - ", hour, sep=""))
      training_data <- training[training$PENTAD >= (start) & training$PENTAD <= (start + num_pentads - 1) & training$HR == hour,]
      test_data <- test[test$PENTAD >= (start) & test$PENTAD <= (start + num_pentads - 1) & test$HR == hour,]
      if(nrow(training[training$SOLR_6 > 0,]) > 0) {
        if(is.null(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]])) {
          SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]] <- list()          
        }
        if(is.null(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]])) {
          SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]] <- list()
        }
        SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]] <- function_string
        SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]] <- lm(formula=as.formula(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["function"]]), data=training_data, na.action=na.omit)
        SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["result"]] <- testContinuousModelSolr(SCBH1_Results_Frac[[as.character(pentad)]][[as.character(hour)]][[as.character(test_year)]][["model"]], test_data, "SOLR_6")
      }
      writeLines("")
    }
  }
}

return_dir <- getwd()
dir.create("Frac_Persistence_Results")
setwd("Frac_Persistence_Results")

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
setwd(return_dir)