require('prob')

source('ClusterHelpers.R')

load("Data.RData")
load("Clusters.RData")
load("Haar.RData")


getClusterings <- function(data, clustering, type="kmeans") {
  if(type == "kmeans") {
    centroids <- clustering$centers
    clusters <- clustering$cluster
  } else if(type == "pam") {
    centroids <- clustering$medoids
    clusters <- clustering$clustering
  }
  
  matched <- data.frame(day=strptime(as.character(data[,"day"]), '%Y-%m-%d'), cluster=clusters)
  return(matched)  
}

getClusteringOffset <- function(matched, col_name, noffset) {
  day <- 60 * 60 * 24
  
  matches <- numeric(0)
  for(i in 1:nrow(matched)) {
    matches <- c(matches, matched[which(matched[,'day'] - matched[i,'day'] == -(noffset * day))[1], 'cluster'])
  }
  
  matched[[col_name]] <- matches
  return(matched)
}

getProb <- function(matched, target_col, given_col) {
  probabilities <- list()
  for(i in 1:nrow(matched)) {
    if(!is.na(matched[i, given_col]) && !is.na(matched[i, target_col])) {
      given_val <- as.character(matched[i, given_col])
      target_val <- as.character(matched[i, target_col])
      if(is.null(probabilities[[given_val]])) {
        probabilities[[given_val]] <- list()
      }
      if(is.null(probabilities[[given_val]][[target_val]])) {
        probabilities[[given_val]][[target_val]] <- 1
      } else {
        probabilities[[given_val]][[target_val]] <- probabilities[[given_val]][[target_val]] + 1
      }
    }
  }
  
  for(given_val in names(probabilities)) {
    sum <- 0
    for(target_val in names(probabilities[[given_val]])) {
      sum <- sum + probabilities[[given_val]][[target_val]]
    }
    for(target_val in names(probabilities[[given_val]])) {
      probabilities[[given_val]][[target_val]] <- probabilities[[given_val]][[target_val]] / sum
    }
  }
  
  return(probabilities)
}

outputProbHelper <- function(probabilities, col_names, label=NULL, given=NULL) {
  if(is.null(col_names)) {
    for(val in sort(unique(probabilities[['cluster']]))) {
      prob <- Prob(probabilities, cluster == val, given)
      writeLines(paste(paste(label, val, sep=" -> "), prob,sep=","))
    }
  } else {
    col <- col_names[1]
    for(val in sort(unique(probabilities[[col]]))) {
      if(is.null(label)) {
        temp_label <- val
      } else {
        temp_label <- paste(label, val, sep=" -> ")
      }
      
      if(is.null(given)) {
        temp_given <- probabilities
      } else {
        temp_given <- given
      }
      
      if(length(col_names) == 1) {
        temp_cols <- NULL  
      } else {
        temp_cols <- col_names[2:length(col_names)]
      }
      outputProbHelper(probabilities, temp_cols, temp_label, subset(temp_given, get(col) == val))
    }
  }
}

outputProb <- function(matched, col_names, file_name) {
  good_rows <- which(!is.na(matched$cluster))
  for(col in col_names) {
    good_rows <- get('intersect','package:base')(good_rows, which(!is.na(matched[[col]])))
  }
  probabilities <- empirical(matched[good_rows, c('cluster', col_names)])
  sink(file_name)
  row <- paste(paste(paste(col_names, collapse=" -> "), "current cluster", sep=' -> '), "probability", sep=",")
  writeLines(row)
  outputProbHelper(probabilities, col_names)
  sink()
}