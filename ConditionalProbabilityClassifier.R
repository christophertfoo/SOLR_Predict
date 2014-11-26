source("ClusterHelpers.R")

training_years <- 2010:2012
test_years <- 2013

hours <- 7:20

start_offset <- 4

# One day before
createCondProb1DayModel <- function(npart_predictand, npart_predictor, years) {
  
  probs <- list()
  joined <- joinClusters("SOLR", "SOLR", "kmeans", npart_predictand, "kmeans", npart_predictor, start_offset)
  joined <- filterJoined(joined, years)
  
  for(part in sort(unique(joined[,3]))) {
    match <- joined[which(joined[,3] == part),]
    max_part <- NULL
    max <- NULL
    for(part_predict in sort(unique(joined[,2]))) {
      prob <- nrow(match[which(match[,2] == part_predict),]) / nrow(match)
      if(is.null(max) || (!is.na(prob) && max < prob)) {
        max <- prob
        max_part = part_predict
      } 
    }
    probs[[as.character(part)]] <- max_part
  }
  
  model <- list()
  model$probs <- probs
  model$npart_predictand <- npart_predictand
  model$npart_predictor <- npart_predictor
  return(model)
}

predictCondProb1Day <- function(model, args) {
  return(model$probs[[as.character(args[1])]])
}

evaluateCondProb1DayModel <- function(model, years) {
  joined <- joinClusters("SOLR", "SOLR", "kmeans", model$npart_predictand, "kmeans", model$npart_predictor, start_offset)
  joined <- filterJoined(joined, years)
  
  centers <- get(paste("kmeans", "SOLR", model$npart_predictand, sep = "_"))$centers
  
  errors <- numeric(0)
  signed <- numeric(0)
  partitions <- list()
  
  for(day in joined$day) {
    args <- list()
    row <- joined[which(joined$day == day),]
    expected <- row[1,2]
    result <- predictCondProb1Day(model, c(row[1,3]))    
    errors <- c(errors, abs(centers[expected, hours] - centers[result, hours]))
    signed <- c(signed, centers[result, hours] - centers[expected, hours])
    if(result != expected) {
      key <- as.character(paste(result, expected, sep = " -> "))
      if(is.null(partitions[[key]])) {
        partitions[[key]] <- 1
      }
      else {
        partitions[[key]] <- partitions[[key]] + 1
      }
    }
  }
  
  result_list <- list()
  result_list[['errors_no_sign']] <- errors
  result_list[['errors_sign']] <- signed
  result_list[['partition_counts']] <- partitions
  
  
  return(result_list)
}

runCondProb1DayEval <- function(predictand_parts = 4:10, file = "CondProb_1Day_Eval_Results") { 
  print(get('start_offset'))
  if(start_offset != 1) {
    file = paste("CondProb_1Day_", start_offset, "Before_EvalResults", sep="")
  }
  
  for(predictand in predictand_parts) {
    errors <- list()
    
    for(predictor in 4:10) {
        model_key <- paste(predictand, " <- ", predictor, sep = "")
        writeLines(model_key)
        model <- createCondProb1DayModel(predictand, predictor, training_years)
        errors[[model_key]] <- evaluateCondProb1DayModel(model, test_years)
      
    }
    
    save(errors, file = paste(file, "_", predictand, ".RData", sep=""))
    
    sink(paste(file, "_", predictand, ".csv", sep=""))
    writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
    for(key in names(errors)) {
      writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
    }    
    sink()
    
    sink(paste(file,"_", predictand, "_PartCounts.csv", sep=""))
    for(key in names(errors)) {
      writeLines(key)
      writeLines(paste("predicted -> actual,Count"))
      
      part_counts <- errors[[key]][['partition_counts']]      
      for(part_key in names(part_counts)) {
        writeLines(paste(part_key, part_counts[[part_key]], sep=","))
      }
      writeLines("")
    } 
    sink()
    
    rm(errors)
    gc()
  }    
}

# Two days before

createCondProbModel <- function(npart_predictand, npart_predictor_1, npart_predictor_2, years) {
  joined1 <- joinClusters("SOLR", "SOLR", "kmeans", npart_predictand, "kmeans", npart_predictor_1, start_offset)
  joined2 <- joinClusters("SOLR", "SOLR", "kmeans", npart_predictand, "kmeans", npart_predictor_2, start_offset + 1)
  joined <- joinJoined(joined1, joined2)
  joined <- filterJoined(joined, years)
  
  probs <- list()
  
  for(part1 in sort(unique(joined[,3]))) {
    for(part2 in sort(unique(joined[,4]))) {
      match <- joined[which(joined[,3] == part1 & joined[,4] == part2),]
      max_part <- NULL
      max <- NULL
      for(part_predict in sort(unique(joined[,2]))) {
        prob <- nrow(match[which(match[,2] == part_predict),]) / nrow(match)
        if(is.null(max) || (!is.na(prob) && max < prob)) {
          max <- prob
          max_part = part_predict
        }
      }
      
      if(is.null(probs[[as.character(part1)]])) {
        probs[[as.character(part1)]] <- list()
      }
      probs[[as.character(part1)]][[as.character(part2)]] <- max_part
    }
  }
  model <- list()
  model$probs <- probs
  model$npart_predictand <- npart_predictand
  model$npart_predictor_1 <- npart_predictor_1
  model$npart_predictor_2 <- npart_predictor_2
  return(model)
}

predictCondProb <- function(model, args) {
  return(model$probs[[as.character(args[1])]][[as.character(args[2])]])
}

evaluateCondProbModel <- function(model, years) {
  joined1 <- joinClusters("SOLR", "SOLR", "kmeans", model$npart_predictand, "kmeans", model$npart_predictor_1, start_offset)
  joined2 <- joinClusters("SOLR", "SOLR", "kmeans", model$npart_predictand, "kmeans", model$npart_predictor_2, start_offset + 1)
  joined <- joinJoined(joined1, joined2)   
  joined <- filterJoined(joined, years)
  print(nrow(joined))
  centers <- get(paste("kmeans", "SOLR", model$npart_predictand, sep = "_"))$centers
  
  errors <- numeric(0)
  signed <- numeric(0)
  partitions <- list()
  
  for(day in joined$day) {
    args <- list()
    row <- joined[which(joined$day == day),]
    expected <- row[1,2]
    result <- predictCondProb(model, c(row[1,3], row[1,4]))    
    errors <- c(errors, abs(centers[expected, hours] - centers[result, hours]))
    signed <- c(signed, centers[result, hours] - centers[expected, hours])
    if(result != expected) {
      key <- as.character(paste(result, expected, sep = " -> "))
      if(is.null(partitions[[key]])) {
        partitions[[key]] <- 1
      }
      else {
        partitions[[key]] <- partitions[[key]] + 1
      }
    }
  }
  
  result_list <- list()
  result_list[['errors_no_sign']] <- errors
  result_list[['errors_sign']] <- signed
  result_list[['partition_counts']] <- partitions
  
  
  return(result_list)
}

runCondProbEval <- function(predictand_parts = 4:10, file = "CondProb_2Day_Eval_Results") { 
  
  if(start_offset != 1) {
    file = paste("CondProb_2Day_", start_offset, "Before_EvalResults", sep="")
  }
  
  for(predictand in predictand_parts) {
    errors <- list()
    
    for(predictor_1 in 4:10) {
      for(predictor_2 in 4:10) {
        model_key <- paste(predictand, " <- ", predictor_1, " + ", predictor_2, sep = "")
        writeLines(model_key)
        model <- createCondProbModel(predictand, predictor_1, predictor_2, training_years)
        errors[[model_key]] <- evaluateCondProbModel(model, test_years)
      }
    }
    
    save(errors, file = paste(file, "_", predictand, ".RData", sep=""))
    
    sink(paste(file, "_", predictand, ".csv", sep=""))
    writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
    for(key in names(errors)) {
      writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
    }    
    sink()
    
    sink(paste(file,"_", predictand, "_PartCounts.csv", sep=""))
    for(key in names(errors)) {
      writeLines(key)
      writeLines(paste("predicted -> actual,Count"))
      
      part_counts <- errors[[key]][['partition_counts']]      
      for(part_key in names(part_counts)) {
        writeLines(paste(part_key, part_counts[[part_key]], sep=","))
      }
      writeLines("")
    } 
    sink()
    
    rm(errors)
    gc()
  }    
}

createCondProbApproxModel <- function(npart_predictand, npart_predictor, years) {
  joined <- joinClusters("SOLR", "SOLR", "kmeans", npart_predictand, "kmeans", npart_predictor, start_offset)
  joined <- filterJoined(joined, years)
  
  probs <- list()
  
  predictand_cond_probs <- list()
  predictor_cond_probs <-list()
  predictor_probs <- list()
  
  predictand_values <- sort(unique(joined[,2]))
  predictor_values <- sort(unique(joined[,3]))
  
  # Get probabilities from the data
  predictor_joined <- joinClusters("SOLR", "SOLR", "kmeans", npart_predictor, "kmeans", npart_predictor, start_offset)
  for(i in predictor_values) {
    match <- joined[which(joined[,3] == i),]
    predictor_probs[[as.character(i)]] <- nrow(match) / nrow(joined)
    for(j in predictand_values) {
      predictand_cond_probs[[paste(j , i, sep = ",")]] <- nrow(match[which(match[,2] == j),]) / nrow(match)
    }
    
    predictor_match <- predictor_joined[which(predictor_joined[,3] == i),]
    for(j in predictor_values) {
      predictor_cond_probs[[paste(j , i, sep = ",")]] <- nrow(match[which(predictor_match[,2] == j),]) / nrow(predictor_match)
    }
  }
  rm(predictor_joined)
  
  # Approximate the chain probability
  for(part1 in predictor_values) {
    for(part2 in predictor_values) {
      max_part <- NULL
      max <- NULL
      for(part_predict in predictand_values) {        
        prob <- (predictand_cond_probs[[paste(part_predict, part1, sep = ",")]] * predictor_cond_probs[[paste(part2, part1, sep = ",")]]) / predictor_probs[[as.character(part1)]]
        if(is.null(max) || (!is.na(prob) && max < prob)) {
          max <- prob
          max_part = part_predict
        }
      }
      
      if(is.null(probs[[as.character(part1)]])) {
        probs[[as.character(part1)]] <- list()
      }
      probs[[as.character(part1)]][[as.character(part2)]] <- max_part
    }
  }
  model <- list()
  model$probs <- probs
  model$npart_predictand <- npart_predictand
  model$npart_predictor_1 <- npart_predictor
  model$npart_predictor_2 <- npart_predictor  
  return(model)
}

runCondProbApproxEval <- function(predictand_parts = 4:10, file = "CondProbApprox_Eval_Results") { 
  for(predictand in predictand_parts) {
    errors <- list()
    
    for(predictor in 4:10) {  
        model_key <- paste(predictand, " <- ", predictor, " + ", predictor, sep = "")
        writeLines(model_key)
        model <- createCondProbApproxModel(predictand, predictor, training_years)
        errors[[model_key]] <- evaluateCondProbModel(model, test_years)      
    }
    
    save(errors, file = paste(file, "_", predictand, ".RData", sep=""))
    
    sink(paste(file, "_", predictand, ".csv", sep=""))
    writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
    for(key in names(errors)) {
      writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
    }    
    sink()
    
    sink(paste(file,"_", predictand, "_PartCounts.csv", sep=""))
    for(key in names(errors)) {
      writeLines(key)
      writeLines(paste("predicted -> actual,Count"))
      
      part_counts <- errors[[key]][['partition_counts']]      
      for(part_key in names(part_counts)) {
        writeLines(paste(part_key, part_counts[[part_key]], sep=","))
      }
      writeLines("")
    } 
    sink()
    
    rm(errors)
    gc()
  }    
}

# Main
load("Clusters_SOLR.RData")
args <- commandArgs(TRUE)
if(args[1] == 'empirical') {
  if(length(args) > 1) {
    runCondProbEval(predictand_parts = args[2])    
  } else {
    runCondProbEval()
  }
} else if(args[1] == 'approx') {
  if(length(args) > 1) {
    runCondProbApproxEval(predictand_parts = args[2])    
  } else {
    runCondProbApproxEval()
  }
} else if(args[1] == '1day') {
  if(length(args) > 1) {
    runCondProb1DayEval(predictand_parts = args[2])    
  } else {
    runCondProb1DayEval()
  }
}
