source('SOLR_Predict.R')

merge_groups = c(12)
max_num_past <- 1

if(!exists("deseasonalized") || !exists("deseasonalized_signal") || !exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    load("Deseasonalized.RData")
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
    current_time <- strptime("1 1 2014", "%m %d %Y")
    deseasonalized <- data.frame(merged)
    MON <- numeric(0)
    DAY <- numeric(0)
    HR <- numeric(0)
    MIN <- numeric(0)
    SOLR <- numeric(0)
    while(current_time$year == 114) {
      print(current_time)
      current_mon <- current_time$mon + 1
      current_day <- current_time$mday
      current_hour <- current_time$hour
      current_min <- current_time$min
      
      MON <- c(MON, current_mon)
      DAY <- c(DAY, current_day)
      HR <- c(HR, current_hour)
      MIN <- c(MIN, current_min)
      
      matches <- which(deseasonalized$MON == current_mon & deseasonalized$DAY == current_day & deseasonalized$HR == current_hour & deseasonalized$MIN == current_min)
      avg_solr <- mean(deseasonalized[matches, "SOLR"])
      SOLR <- c(SOLR, avg_solr)
      
      for(i in matches) {
        deseasonalized[["SOLR"]][i] <- deseasonalized[["SOLR"]][i] - avg_solr
      }
      
      current_time <- as.POSIXlt(current_time + 3600)
    }
    deseasonalized_signal <- data.frame(MON, DAY, HR, MIN, SOLR)
    deseasonalized_offset <- dataOffset(6, "SOLR", deseasonalized)
    save(deseasonalized, deseasonalized_signal, deseasonalized_offset, file="Deseasonalized.RData")
  }
}