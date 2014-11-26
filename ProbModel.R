require('prob')

findClusters <- function(data) {
  return(unique(data[['cluster']]))
}

filterCols <- function(data, cols) {
  log <- T
  for(col in cols) {
    log <- log & !is.na(data[[col]]) & !is.nan(data[[col]])
  }
  return(data[which(log), cols])
}

findBreaks <- function(data, col, num_bins) {
  d <- data[[col]]
  d <- d[which(!is.na(d) & !is.nan(d))]
  r <- range(d)
  i <- (r[2] - r[1]) / (num_bins)
  return(seq(r[1], r[2], i))
}

findBin <- function(value, breaks) {
  return(findInterval(value, breaks))
}

findRange <- function(bin, breaks) {
  nbreaks <- length(breaks)
  if(bin == 0) {
    range <- paste('(, ', breaks[1], sep="")
  } else if(bin > nbreaks) {
    range <- paste('[', breaks[nbreaks], ', )', sep="")
  } else {
    range <- paste('[', breaks[bin], ', ', breaks[bin + 1], ')', sep="")
  }
  return(range)
}

findProbs <- function(model, data, cols, num_bins) {
  for(col in cols) {
    f <- filterCols(data, c('cluster', col))
    b <- findBreaks(data, col, num_bins)
    df <- data.frame(cluster=f[['cluster']])
    model[[col]][['breaks']] <- b
    df[[col]] <- unlist(lapply(f[[col]], findInterval, b))
    model[[col]][['probs']] <- empirical(df)
  }
  return(model)
}

createModel <- function(data, columns, num_bins=10) {
  model <- list()
  return(findProbs(model, data, columns, num_bins))
}

predict <- function(data, model, clusters, verbose=0) {
  results <- vector()
  for(i in 1:nrow(data)) {
    best <- NULL
    best_prob <- -Inf
    if(verbose > 0) {
      writeLines(paste("Row", i))
    }
    for(current in clusters) {
      prob <- 1
      if(verbose > 1) {
        writeLines(paste("  Cluster", current))
      }
      for(col in names(model)) {  
        col_value <- data[i,col]
        if(!is.na(col_value) && !is.nan(col_value)) {
          probs <- model[[col]][['probs']]
          breaks <- model[[col]][['breaks']]
          bin <- findBin(col_value, breaks)
          col_prob <- Prob(probs, get(col) == bin, subset(probs, cluster == current))
          if(verbose > 2) {
            writeLines(paste("   ", col, "Value:", col_value))
            writeLines(paste("    Bin", bin ))
            writeLines(paste("    Col Prob:", col_prob))
          }
          prob <- prob * col_prob
        }
      }
      prob <- prob * Prob(model[[names(model)[1]]][['probs']], cluster == current)
      if(verbose > 1) {
        writeLines(paste("    Probability:", prob))
        writeLines("")
      }
      if(prob > best_prob) {
        best_prob <- prob
        best <- current
      }
    }
    if(verbose > 0) {
      writeLines(paste("  Best Probability:", best_prob))
      writeLines("")
    }
    results <- c(results, best)
    
  }
  return(results)
}

tenFold <- function(data) {
  
}

