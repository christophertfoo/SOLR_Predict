source('ClusterHelpers.R')

training_years <- 2010:2012
test_years <- 2013

hours <- 7:20

start_offset <- 1
num_offset <- 2

createPredictand <- function(name, cluster_type, num_clusters) {
  predictand <- list()
  predictand$name <- name
  predictand$cluster_type <- cluster_type
  predictand$num_clusters <- num_clusters
  return(predictand)
}

addPredictor <- function(predictors, name, cluster_type, num_clusters, offset) {
  predictor <- list()
  predictor$name <- name
  predictor$cluster_type <- cluster_type
  predictor$num_clusters <- num_clusters
  predictor$offset <- offset
  predictors <- c(predictors, list(predictor))
  return(predictors)
}

# TODO pass in cluster data
createNBC <- function(predictand, predictors, years) {  
  # Get probabilites for the predictand
  predictand_cluster_key <- ""
  if(predictand$cluster_type == "kmeans") {
    predictand_cluster_key <- 'cluster'
  } else if(predictand$cluster_type == "pam") {
    predictand_cluster_key <- 'clustering'
  }
  predictand_probs <- list()
  predictand_clusters <- get(paste(predictand$cluster_type, predictand$name, predictand$num_clusters ,sep="_"))[[predictand_cluster_key]]
  
  predictand_values <- sort(unique(predictand_clusters))
  num_values <- length(predictand_clusters)
  
  for(value in predictand_values) {
    predictand_probs[[as.character(value)]] <- length(which(predictand_clusters == value)) / num_values
  }
  
  # Get probabilities for the predictors
  predictor_probs <- list()  
  for(predictor in predictors) {
    probs <- list()
    joined <- joinClusters(predictand$name, predictor$name, predictand$cluster_type, predictand$num_clusters, predictor$cluster_type, predictor$num_clusters, predictor$offset)

    joined <- filterJoined(joined, years)
    
    predictor_values <- sort(unique(joined[,3]))
    for(value in predictand_values) {
      filtered <- joined[which(joined[,2] == value),]
      num_filtered <- nrow(filtered)
      for(p_value in predictor_values) {
        probs[[paste(p_value, value,sep = "|")]] <- length(which(filtered[,3] == p_value)) / num_filtered
      }
    }
    predictor_probs[[paste(predictor$name, predictor$offset, sep="_")]] <- probs
  }
  
  # Package into a single object
  model <- list()
  model$predictand_values <- predictand_values
  model$predictand_probs <- predictand_probs
  model$predictor_probs  <- predictor_probs
  model$predictors <- predictors
  model$predictand <- predictand
  return(model)
  
}

addNBCPredictArgs <- function(args, name, offset, value) {
  new_arg <- list()
  new_arg$name <- paste(name, offset, sep = "_")
  new_arg$value <- value
  args <- c(args, list(new_arg))
  return(args)
}

nbcPredict <- function(model, args) {
  top_partition <- NULL
  top_probability <- NULL
  
  for(value in model$predictand_values) {
    probability <- model$predictand_probs[[as.character(value)]]
    for(arg in args) {
      probability <- probability * model$predictor_probs[[arg$name]][[paste(arg$value, value, sep="|")]]
    }
    
    if(is.null(top_partition) || top_probability < probability) {
      #       writeLines(paste(value, probability, sep = " => "))
      top_partition <- value
      top_probability <- probability
    }
  }
  return(top_partition)
}

evaluateNBCModel <- function(model, years) {
  
  joined <- list()
  for(predictor in model$predictors) {
    joined[[paste(predictor$name, predictor$offset, sep = "_")]] <- filterJoined(joinClusters(model$predictand$name, predictor$name, model$predictand$cluster_type, model$predictand$num_clusters, predictor$cluster_type, predictor$num_clusters, predictor$offset), years)
  }
  
  all_days <- character(0)
  for(j in joined) {
    all_days <- c(all_days, j$day)
  }
  all_days <- sort(unique(all_days))
  
  filtered <- NULL
  for(j in joined) {
    if(is.null(filtered)) {
      filtered <- all_days %in% j$day
    } else {
      filtered <- filtered & all_days %in% j$day
    }
  }
  filtered <- all_days[which(filtered)]  
  
  
  centers <- NULL
  
  if(model$predictand$cluster_type == "kmeans") {
    centers <- get(paste(model$predictand$cluster_type, model$predictand$name, model$predictand$num_clusters, sep = "_"))$centers
  } else if(model$predictand$cluster_type == "pam") {
    centers <- get(paste(model$predictand$cluster_type, model$predictand$name, model$predictand$num_clusters, sep = "_"))$medoids
  }  
  
  errors <- numeric(0)
  signed <- numeric(0)
  partitions <- list()
  
  for(day in filtered) {
    args <- list()
    expected <- NULL
    for(predictor in model$predictors) {
      predictor_joined <- joined[[paste(predictor$name, predictor$offset, sep = "_")]]
      predictor_joined <- predictor_joined[which(predictor_joined$day == day),]
      if(is.null(expected)) {
        expected <- predictor_joined[, 2]
      }
      
      args <- addNBCPredictArgs(args, predictor$name, predictor$offset, predictor_joined[, 3])
    }
    
    result <- nbcPredict(model, args)    
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

runNBCScan <- function(file = "NBC_2Day_Scan_Results") { 
  args <- commandArgs(TRUE)
  
  part_args <- 4:10
  if(length(args) >= 2) {
    part_args <- args[2]
  }
  for(predictand_parts in part_args) {    
    predictand <- createPredictand('SOLR', 'kmeans', predictand_parts)
    
    predictors <- list()
    
    predictors <- addPredictor(predictors,'RELH', 'kmeans', 4, 1)
    predictors <- addPredictor(predictors,'RELH', 'kmeans', 4, 2)
    
    predictors <- addPredictor(predictors,'TMPF', 'kmeans', 4, 1)    
    predictors <- addPredictor(predictors,'TMPF', 'kmeans', 4, 2)    
    
    predictors <- addPredictor(predictors,'SKNT_N_SKNT_E', 'kmeans', 4, 1)
    predictors <- addPredictor(predictors,'SKNT_N_SKNT_E', 'kmeans', 4, 2)
    
    predictors <- addPredictor(predictors,'SOLR', 'kmeans', 6, 1)
    predictors <- addPredictor(predictors,'SOLR', 'kmeans', 6, 2)
    
    npredictors <- 1:length(predictors)
    if(length(args) >= 3) {
      npredictors <- args[3]
    }
    
    for(i in npredictors) {
      errors <- list()
      partition_combos <- data.frame(4:10)
      if(i > 1) {
        for(p_col in 2:i) {
          partition_combos <- cbind(partition_combos, 4:10)
        }
      }
      partition_combos <- expand.grid(partition_combos)
      combos <- combn(1:length(predictors), i)
      
      for(j in 1:ncol(combos)) {
        selected <- predictors[combos[,j]]
        
        for(part_combo_index in 1:nrow(partition_combos)){
          
          part_combo <- partition_combos[part_combo_index,]
          
          for(feature_index in 1:length(selected)) {
            selected[[feature_index]]$num_clusters <- part_combo[feature_index]
          }
          
          p_string <- character(0)
          for(p in selected) {
            p_string <- c(p_string, paste(p$name, p$num_clusters, p$offset, sep="_"))
          }
          p_string <- paste(p_string, collapse = " + ")
          predictand_string <- paste(predictand$name, predictand$num_clusters, sep="_")        
          model_key <- paste(predictand_string, p_string, sep=" <- ")     
          
          writeLines(model_key)
          
          model <- createNBC(predictand, selected, training_years)          
          results <- evaluateNBCModel(model, test_years)
          errors[[model_key]] <- results
        }
      }
      
      save(errors, file = paste(file, "_", predictand_parts, "_", i, "F.RData", sep=""))
      
      sink(paste(file, "_", predictand_parts, "_", i, "F.csv", sep=""))
      writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
      for(key in names(errors)) {
        writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
      }    
      sink()
      
      sink(paste(file,"_", predictand_parts, "_", i, "F_PartCounts.csv", sep=""))
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
}

runNBCEval <- function(file = "NBC_2Day_Eval_Results") { 
  args <- commandArgs(TRUE)
  
  if(length(args) < 10) {
    writeLines("Usage: eval <num predictand partitions> <RELH_*_1> <RELH_*_2> <TMPF_*_1> <TMPF_*_2> <SKNT_N_SKNT_E_*_1> <SKNT_N_SKNT_E_*_2> <SOLR_*_1> <SOLR_*_2>")
  }
  
  part_args <- 4:10
  if(length(args) >= 2) {
    part_args <- args[2]
  }
  for(predictand_parts in part_args) {  
    errors <- list()
    
    predictand <- createPredictand('SOLR', 'kmeans', predictand_parts)
    
    predictors <- list()
    
    predictors <- addPredictor(predictors,'RELH', 'kmeans', args[3], 1)
    predictors <- addPredictor(predictors,'RELH', 'kmeans', args[4], 2)
    
    predictors <- addPredictor(predictors,'TMPF', 'kmeans', args[5], 1)    
    predictors <- addPredictor(predictors,'TMPF', 'kmeans', args[6], 2)    
    
    predictors <- addPredictor(predictors,'SKNT_N_SKNT_E', 'kmeans', args[7], 1)
    predictors <- addPredictor(predictors,'SKNT_N_SKNT_E', 'kmeans', args[8], 2)
    
    predictors <- addPredictor(predictors,'SOLR', 'kmeans', args[9], 1)
    predictors <- addPredictor(predictors,'SOLR', 'kmeans', args[10], 2)
    
    npredictors <- 1:length(predictors)
    for(i in npredictors) {      
      combos <- combn(1:length(predictors), i)
      for(j in 1:ncol(combos)) {
        selected <- predictors[combos[,j]]        
        
        p_string <- character(0)
        for(p in selected) {
          p_string <- c(p_string, paste(p$name, p$num_clusters, p$offset, sep="_"))
        }
        p_string <- paste(p_string, collapse = " + ")
        predictand_string <- paste(predictand$name, predictand$num_clusters, sep="_")        
        model_key <- paste(predictand_string, p_string, sep=" <- ")     
        
        writeLines(model_key)
        
        model <- createNBC(predictand, selected, training_years)          
        results <- evaluateNBCModel(model, test_years)
        errors[[model_key]] <- results
      }      
    }
    save(errors, file = paste(file, "_", predictand_parts, ".RData", sep=""))
    
    sink(paste(file, "_", predictand_parts, ".csv", sep=""))
    writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
    for(key in names(errors)) {
      writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
    }    
    sink()
    
    sink(paste(file,"_", predictand_parts, "_PartCounts.csv", sep=""))
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

runNBCGreedyEval <- function(file = "NBC_Greedy_Eval_Results") {
  args <- commandArgs(TRUE)
  if(start_offset != 1) {
    file <- paste("NBC_Greedy_Eval_", num_offset,"Day_", start_offset,"Before_Results", sep="")    
  } else {
    file <- paste("NBC_Greedy_Eval_", num_offset,"Day_Results", sep="")
  }
  
  part_args <- 4:10
  if(length(args) >= 2) {
    part_args <- args[2]
  }
  
  features <- c("RELH", "TMPF", "SKNT_N_SKNT_E", "SOLR")
  
  offsets <- numeric(0)
  for(i in 1:num_offset) {
    offsets <- c(offsets, start_offset + i - 1)
  }
  
  for(predictand_parts in part_args) {
    
    predictand <- createPredictand('SOLR', 'kmeans', predictand_parts)
    predictors <- list()
    for(feature in features) {
      for(offset in offsets) {
        
        min_error <- NULL
        min_part <- NULL
        
        for(part in 4:10) {
          test_pred <- list()
          test_pred <- addPredictor(test_pred, feature, 'kmeans', part, offset)
          
          model <- createNBC(predictand, test_pred, training_years)          
          results <- evaluateNBCModel(model, test_years)
          if(is.null(min_error) || is.null(min_part) || min_error >= mean(results[['errors_no_sign']])) {
            min_error <- mean(results[['errors_no_sign']])
            min_part <- part
          }
        }
        writeLines(paste("Selected: ", feature, "_", min_part, "_", offset, sep=""))
        predictors <- addPredictor(predictors, feature, 'kmeans', min_part, offset)
      }
    }
    
    errors <- list()
    npredictors <- 1:length(predictors)
    for(i in npredictors) {      
      combos <- combn(1:length(predictors), i)
      for(j in 1:ncol(combos)) {
        selected <- predictors[combos[,j]]        
        
        p_string <- character(0)
        for(p in selected) {
          p_string <- c(p_string, paste(p$name, p$num_clusters, p$offset, sep="_"))
        }
        p_string <- paste(p_string, collapse = " + ")
        predictand_string <- paste(predictand$name, predictand$num_clusters, sep="_")        
        model_key <- paste(predictand_string, p_string, sep=" <- ")     
        
        writeLines(model_key)
        
        model <- createNBC(predictand, selected, training_years)          
        results <- evaluateNBCModel(model, test_years)
        errors[[model_key]] <- results
      }      
    }
    save(errors, file = paste(file, "_", predictand_parts, ".RData", sep=""))
    
    sink(paste(file, "_", predictand_parts, ".csv", sep=""))
    writeLines("Features,Unsigned Average Absolute Error - Average (W/m^2),Signed Average Absolute Error - Average(W/m^2)")
    for(key in names(errors)) {
      writeLines(paste(key, mean(errors[[key]][['errors_no_sign']]), mean(errors[[key]][['errors_sign']]), sep = ","))
    }    
    sink()
    
    sink(paste(file,"_", predictand_parts, "_PartCounts.csv", sep=""))
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

# MAIN
load("Clusters_SOLR.RData")
load("Clusters_RELH.RData")
load("Clusters_SKNT_N_SKNT_E.RData")
load("Clusters_TMPF.RData")

args <-commandArgs(TRUE)
if(length(args) > 0) {
  if(args[1] == 'scan') {
    runNBCScan()
  } else if(args[1] == 'eval') {
    runNBCEval()
  }
  else if(args[1] == 'greedy') {
    runNBCGreedyEval()
  }
}
