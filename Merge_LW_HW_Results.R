args <- commandArgs(F)

# Variables

lw_results_file <- 'Solr_Deseasonalized_Monthly_Results_LW.RData'
hw_results_file <- 'Solr_Deseasonalized_Monthly_Results_HW.RData'
output_file <- 'LR_10_Monthly_LW_HW_Results'

num_features <- '10'
method <- 'P'

groups <- as.character(1:13)
hours <- as.character(0:23)
years <- as.character(2010:2013)

# Functions

group_key <- function(group, sub) {
  return(paste("Group", group, sub, sep='_'))
}

gen_frame <- function(groups, years, hours) {
  frame <- data.frame(Hour=hours)
  num_hours <- length(hours)
  for(group in groups) {
    for(year in years) {
      frame[[group_key(group, year)]] <- numeric(num_hours)
    }
    frame[[group_key(group, 'Average')]] <- numeric(num_hours)
  }
  frame[['Average']] <- numeric(num_hours)
  return(frame)
}

# MAIN

load(lw_results_file)
result_lw <- result

load(hw_results_file)
result_hw <- result



error_frame <- gen_frame(groups, years, hours)
rel_error_frame <- gen_frame(groups, years, hours)

i <- 1
for(hour in hours) {
  errors <- numeric(0)
  rel_errors <- numeric(0)
  for(group in groups) {
    group_errors <- numeric(0)
    group_rel_errors <- numeric(0)
    for(year in years) {
      lw <- result_lw[[num_features]][[method]][[group]][[hour]][[year]][['result']]
      hw <- result_hw[[num_features]][[method]][[group]][[hour]][[year]][['result']]
      
      lw_errors <- numeric(0)
      lw_rel_errors <- numeric(0)
      
      hw_errors <- numeric(0)
      hw_rel_errors <- numeric(0)
      
      if(!is.null(lw)) {
        lw_errors <- abs(lw[['actual']] - lw[['predicted']])
        lw_rel_errors <- lw_errors / lw[['actual']] * 100
        lw_rel_errors <- lw_rel_errors[which(!is.nan(lw_rel_errors) & !is.infinite(lw_rel_errors))]
      }
      if(!is.null(hw)) {
        hw_errors <- abs(hw[['actual']] - hw[['predicted']])
        hw_rel_errors <- temp_errors / hw[['actual']] * 100
        hw_rel_errors <- temp_rel_errors[which(!is.nan(temp_rel_errors) & !is.infinite(temp_rel_errors))]
      }
      
      temp_errors <- c(lw_errors, hw_errors)
      temp_rel_errors <- c(lw_rel_errors, hw_rel_errors)
      
      group_errors <- c(group_errors, temp_errors)
      group_rel_errors <- c(group_rel_errors, temp_rel_errors)    
      
      error_frame[[group_key(group, year)]][i] <- mean(temp_errors)
      rel_error_frame[[group_key(group, year)]][i] <- mean(temp_rel_errors)
    }
    
    errors <- c(errors, group_errors)
    rel_errors <- c(rel_errors, group_rel_errors)
    
    error_frame[[group_key(group, 'Average')]][i] <- mean(group_errors)
    rel_error_frame[[group_key(group, 'Average')]][i] <- mean(group_rel_errors)
  }
  
  error_frame[['Average']][i] <- mean(errors)
  rel_error_frame[['Average']][i] <- mean(rel_errors)
  i <- i + 1
}

write.csv(error_frame, file=paste(output_file, "Error.csv", sep="_"), row.names=F)
write.csv(rel_error_frame, file=paste(output_file, "RelError.csv", sep="_"), row.names=F)