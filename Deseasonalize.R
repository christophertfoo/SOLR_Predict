source('SOLR_Predict.R')

num_surrounding <- 2

if(!exists("deseasonalized") || !exists("deseasonalized_signal") || !exists("deseasonalized_offset")) {
  if(file.exists("Deseasonalized.RData")) {
    load("Deseasonalized.RData")
  } else {
    if(!exists("merged")) {
      if(file.exists("Data.RData")) {
        writeLines("Loading Data.RData")
        load("Data.RData")
        source('SOLR_Predict.R')
      } else {
        source("Load_Data.R")
      }
    }
    
    to_merge <- list()
    
    # Collect indexes of the rows that will be a part of the deseasonalized signal
    num_rows <- nrow(merged)
    for(i in 1:num_rows) {
      writeLines(paste(i,"/",num_rows, sep=""))
      row <- merged[i,]
      current_time <- strptime(paste(row$MON, row$DAY, 2014, row$HR, row$MIN, sep=" "), "%m %d %Y %H %M")
      to_merge[[as.character(current_time)]] <- c(to_merge[[as.character(current_time)]], i)
      
      for(j in 1:num_surrounding) {
        temp <- as.POSIXlt(current_time - (i * 24 * 3600))
        if(temp$year == 114) {
          to_merge[[as.character(temp)]] <- c(to_merge[[as.character(temp)]], i)
        }
        else {
          break
        }
      }
      
      end_time <- current_time
      for(j in 1:num_surrounding) {
        temp <- as.POSIXlt(end_time + (i * 24 * 3600))
        if(temp$year == 114) {
          to_merge[[as.character(temp)]] <- c(to_merge[[as.character(temp)]], i)
        }
        else {
          break
        }
      }
    }   
    
    current_time <- strptime("1 1 2014 0 0", "%m %d %Y %H %M")
    deseasonalized <- data.frame(merged)
    MON <- numeric(0)
    DAY <- numeric(0)
    HR <- numeric(0)
    MIN <- numeric(0)
    SOLR <- numeric(0)
    while(current_time$year == 114) {
      current_mon <- current_time$mon + 1
      current_day <- current_time$mday
      current_hour <- current_time$hour
      current_min <- current_time$min
      
      MON <- c(MON, current_mon)
      DAY <- c(DAY, current_day)
      HR <- c(HR, current_hour)
      MIN <- c(MIN, current_min)
      
      matches <- to_merge[[as.character(current_time)]]
      if(!is.null(matches)){
        writeLines(paste(current_time, " - Match", sep=""))
        avg_solr <- mean(deseasonalized[matches, "SOLR"])
        SOLR <- c(SOLR, avg_solr)
        
        for(i in matches) {
          deseasonalized[["SOLR"]][i] <- deseasonalized[["SOLR"]][i] - avg_solr
        }
      } else {
        writeLines(paste(current_time, " - No Match", sep=""))
      }     
      current_time <- as.POSIXlt(current_time + 3600)
    }
    deseasonalized_signal <- data.frame(MON, DAY, HR, MIN, SOLR)
    deseasonalized_offset <- dataOffset(6, c("SOLR"), deseasonalized)
    save(deseasonalized, deseasonalized_signal, deseasonalized_offset, file="Deseasonalized.RData")
  }
}