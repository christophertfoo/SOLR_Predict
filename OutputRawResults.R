args <- commandArgs(F)

# Variables

results_file <- 'Solr_Deseasonalized_Results.RData'
output_file <- 'LR_5_Monthly_2013_Results'

num_features <- '5'
method <- 'P'

groups <- as.character(1:13)
hours <- as.character(0:23)
years <- as.character(2013)

# MAIN

load(results_file)

times <- character(0)
actual <- numeric(0)
predicted <- numeric(0)
errors <- numeric(0)
rel_errors <- numeric(0)
for(hour in hours) {
  for(group in groups) {
    for(year in years) {
      current_result <- result[[num_features]][[method]][[group]][[hour]][[year]][['result']]      
      if(!is.null(current_result)) {
        times <- c(times, as.character(current_result[['time']]))
        actual <- c(actual, current_result[['actual']])
        predicted <- c(predicted, current_result[['predicted']])
        temp_errors <- abs(current_result[['actual']] - current_result[['predicted']])
        errors <- c(errors, temp_errors)
        
        temp_rel_errors <- temp_errors / current_result[['actual']] * 100
        rel_errors <- c(rel_errors, temp_rel_errors)
      }
    }
  }
}

frame <- data.frame(Time=times,Actual=actual,Predicted=predicted,Error=errors,RelError=rel_errors)
write.csv(frame, file=paste(output_file, "Raw.csv", sep="_"), row.names=F)