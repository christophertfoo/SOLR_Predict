args <- commandArgs(F)

results_types <- c('LR', 'Cubist')
features <- c(5, 10, 20, 30, 50)

# Variables

results_file <- 'Solr_Results.RData'
output_file_postfix <- 'Monthly_Results'

num_features <- '50'
method <- 'P'

groups <- as.character(1:13)
hours <- as.character(0:23)
years <- c("2013")#as.character(2010:2013)

day_hours <- as.character(7:20)

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
for(type in results_types) {
  if(type == "LR") {
    results_file <- 'Solr_Deseasonalized_Results.RData'
  } else if(type == 'Cubist') {
    results_file <- 'Solr_Cubist_Deseasonalized_Results.RData'
  }
  
  load(results_file)
  
  error_frame <- gen_frame(groups, years, hours)
  rel_error_frame <- gen_frame(groups, years, hours)
  
  all_errors <- numeric(0)
  all_rel_errors <- numeric(0)
  
  for(num_features in features) {
    output_file <- paste(type, num_features, output_file_postfix, sep="_")
    
    
    i <- 1
    for(hour in hours) {
      errors <- numeric(0)
      rel_errors <- numeric(0)
      for(group in groups) {
        group_errors <- numeric(0)
        group_rel_errors <- numeric(0)
        for(year in years) {
          current_result <- result[[as.character(num_features)]][[method]][[group]][[hour]][[year]][['result']]
          
          current_errors <- numeric(0)
          current_rel_errors <- numeric(0)
          
          if(!is.null(current_result)) {
            current_errors <- abs(current_result[['actual']] - current_result[['predicted']])
            current_rel_errors <- current_errors / current_result[['actual']] * 100
            current_rel_errors <- current_rel_errors[which(!is.nan(current_rel_errors) & !is.infinite(current_rel_errors))]
          }
          
          group_errors <- c(group_errors, current_errors)
          group_rel_errors <- c(group_rel_errors, current_rel_errors)    
          
          error_frame[[group_key(group, year)]][i] <- mean(current_errors)
          rel_error_frame[[group_key(group, year)]][i] <- mean(current_rel_errors)
        }
        
        errors <- c(errors, group_errors)
        rel_errors <- c(rel_errors, group_rel_errors)
        
        error_frame[[group_key(group, 'Average')]][i] <- mean(group_errors)
        rel_error_frame[[group_key(group, 'Average')]][i] <- mean(group_rel_errors)
      }
      
      if(hour %in% day_hours) {
        all_errors <- c(all_errors, errors)
        all_rel_errors <- c(all_rel_errors, rel_errors)
      }
      
      
      error_frame[['Average']][i] <- mean(errors)
      rel_error_frame[['Average']][i] <- mean(rel_errors)
      i <- i + 1
    }
    
    write.csv(error_frame, file=paste(output_file, "Error.csv", sep="_"), row.names=F)
    sink(file = paste(output_file, "Error.csv", sep="_"), append = TRUE)
    writeLines("")
    writeLines(paste("Overall Average", mean(all_errors), sep=","))
    sink()
    
    
    write.csv(rel_error_frame, file=paste(output_file, "RelError.csv", sep="_"), row.names=F)
    sink(file = paste(output_file, "RelError.csv", sep="_"), append = TRUE)
    writeLines("")
    writeLines(paste("Overall Average", mean(all_rel_errors), sep=","))
    sink()
  }  
}

