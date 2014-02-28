# Finds the correlation coefficient between the two given columns.
#
# Parameters:
#  -index     = The index of the correlation call in the list of scheduled correlation calls.
#  -paramList = The parameters for the call.  Contains xCol (the name of the independent column),
#               yCol (the name of the dependent column), selectedMethod (the name of the correlation
#               method to be used), and data (the data frame).
#
# Returns:
#  -A list containing the results of the correlation 
#   (index of the correlation call, name of the dependent column, correlation coefficient).
runCorrelation <- function(index, paramList) {
  index <- index
  xCol <- paramList$xCol
  yCol <- paramList$yCol
  data <- paramList$data
  selectedMethod <- paramList$selectedMethod
  correlation <- cor(data[[xCol]], data[[yCol]], use="pairwise.complete.obs", method=selectedMethod)
  return(list(index=index, name=xCol, correlation=correlation))  
}

# Combines the results for the correlation into a single data frame.
#
# Parameters:
#  -statusList = The list of function statuses.
#
# Returns:
#  -A data frame containing the correlation results with two columns (names and correlations).
getCorrelationResults <- function(statusList) {
  colNames <- character(0)
  correlations <- numeric(0)
  for(i in 1:length(statusList)) {
    colNames <- c(colNames, statusList[[i]]$result$name)
    correlations <- c(correlations, statusList[[i]]$result$correlation)
  }
  return(data.frame(names=colNames, correlations=correlations))
}

# Finds the correlation coefficients for each column correlated with the given column (dependent) in the
# given data frame.
#
# Parameters:
#  -col            = The name of the column containing the dependent values.
#  -data           = The data frame containing the data.
#  -selectedMethod = The correlation method to be used.
#  -debug          = If the function should be run in debug mode.
#
# Returns:
#  -A data frame containing the correlation coefficients for each column with the given dependent column.
correlate <- function(col, data, selectedMethod="pearson", debug=F) {
  nonNumbers <- c()
  colNames <- names(data)
  for(i in 1:length(colNames)) {
    if(!is.numeric(data[,colNames[i]][0])) {
      nonNumbers <- c(nonNumbers, colNames[i])
    }
  }
  
  numNonNum <- length(nonNumbers)
  
  if(numNonNum > 0) {
    writeLines("Ignoring:")
    for(i in 1:numNonNum){
      writeLines(nonNumbers[i])
    }
  }
  
  validCol <- Filter(function(i){return(!(i %in% nonNumbers || i == col))}, names(data))
  numCols <- length(validCol)
  paramList <- vector(mode="list", length=numCols)
  for(i in 1:numCols) {
    paramList[i] <- list(list(xCol = validCol[i], yCol = col, data = data[,names(data) %in% c(col, validCol[i])], selectedMethod = selectedMethod))
  }
  
  results <- runParallel(paramList=paramList, parallelFunction=runCorrelation, collectFunction=getCorrelationResults, debug=debug)
  return(results[order(abs(results$correlations), decreasing=T),])
}

correlateNoMT <- function(col, data, selectedMethod="pearson", debug=F) {
  ignore <- c("MON", "YEAR", "DAY", "HR", "MIN", "DT", "DT_NUM")
  nonNumbers <- c()
  colNames <- names(data)
  for(i in 1:length(colNames)) {
    if(!is.numeric(data[,colNames[i]][0])) {
      nonNumbers <- c(nonNumbers, colNames[i])
    }
  }
  
  numNonNum <- length(nonNumbers)
  
  if(numNonNum > 0) {
    writeLines("Ignoring:")
    for(i in 1:numNonNum){
      writeLines(nonNumbers[i])
    }
  }
  
  validCol <- Filter(function(i){return(!(i %in% nonNumbers || i %in% ignore || i == col))}, names(data))
  numCols <- length(validCol)
  colNames <- character(0)
  correlations <- numeric(0)
  for(i in 1:numCols) {
    col_data <- data[,names(data) %in% c(col, validCol[i])]
    xCol <- validCol[i]
    yCol <- col
    correlation <- cor(col_data[[xCol]], col_data[[yCol]], use="pairwise.complete.obs", method=selectedMethod)

    colNames <- c(colNames, xCol)
    correlations <- c(correlations,correlation)
  }
  results <- data.frame(names=colNames, correlations=correlations)
  return(results[order(abs(results$correlations), decreasing=T),])
}

# Finds the best columns (i.e. highest correlation coefficient) by taking all columns that have a coefficient above
# the given threshold.
#
# Parameters:
#  -col            = The name of the column that contains the dependent values.
#  -data           = The data frame.
#  -threshold      = The correlation coefficient threshold (all columns with coefficients >= than the threshold are kept).
#  -selectedMethod = The method used to calculate the correlation coefficient.
#  -results        = An optional result set containing the correlation coefficients.
#
# Returns:
#  -A data frame containing all columns with a correlation coefficient above the given threshold.
goodCorrelateThresh <- function(col, data, threshold, selectedMethod="pearson", results=NULL) {
  if(is.null(results)) {
    results <- correlate(col, data, selectedMethod)
  }
  good <- numeric(0)
  goodNames <- character(0)
  for(i in 1:nrow(results)) {
    correlation <- results$correlations[[i]]
    if(is.numeric(correlation) && !is.na(correlation) && abs(correlation) >= threshold) {
      good <- c(good, correlation)
      goodNames <- c(goodNames, as.character(results$names[[i]]))
    }
  }
  
  goodResults <- data.frame(attribute=goodNames, correlation=good)
  return(goodResults)
}

# Finds the best columns (i.e. highest correlation coefficient) by taking the given number of columns from the top.
#
# Parameters:
#  -col            = The name of the column that contains the dependent values.
#  -data           = The data frame.
#  -topThreshold   = The number of columns to take.
#  -selectedMethod = The method used to calculate the correlation coefficient.
#  -results        = An optional result set containing the correlation coefficients.
#
# Returns:
#  -A data frame containing the top columns.
goodCorrelateTop <- function(col, data, topThreshold, selectedMethod="pearson", results=NULL) {
  if(is.null(results)) {
    results <- correlate(col, data, selectedMethod)
  }
  if(nrow(results) < topThreshold) {
    numResults <- nrow(results)
  } else {
    numResults <- topThreshold
  }
  results <- results[order(abs(results$correlations), decreasing=T),]
  return(results[1:numResults,])
}

#
logCorrelations <- function(col, data, filePrefix, getTop=F, numTop=5) {
  writeLines("Pearson:")
  sink(paste(filePrefix, "_pearson.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="pearson"))
  if(getTop) {
    writeLines(paste("Top", numTop, "Fields:"))
    print(goodCorrelateTop(col=col, data=data, selectedMethod="pearson", topThreshold=numTop))
  }
  sink()
  writeLines("Spearman")
  sink(paste(filePrefix, "_spearman.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="spearman"))
  if(getTop) {
    writeLines(paste("Top", numTop, "Fields:"))
    print(goodCorrelateTop(col=col, data=data, selectedMethod="spearman", topThreshold=numTop))
  }
  sink()
  writeLines("Kendall")
  sink(paste(filePrefix, "_kendall.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="kendall"))
  if(getTop) {
    writeLines(paste("Top", numTop, "Fields:"))
    print(goodCorrelateTop(col=col, data=data, selectedMethod="kendall", topThreshold=numTop))
  }
  sink()
  writeLines("Done")
}

writeCorrelationResult <- function(station, method, pentad, data) {
  sink(paste(station, pentad, paste(method, "csv", sep="."), sep="_"))
  pentad <- data[[as.character(pentad)]]
  row <- NULL
  for(i in 0:23) {
    if(is.null(row)) {
      row <- paste(paste("Feature-HR", i, sep=""), paste("Correlation-HR", i, sep=""),sep=",")
    } else {
      row <- paste(row, paste("Feature-HR", i, sep=""), paste("Correlation-HR", i, sep=""),sep=",")
    }
  }
  writeLines(row)
  
  maxRows <- NULL
  for(i in 0:23) {
    count <- nrow(pentad[[as.character(i)]])
    if(!is.null(count)) {
      if(is.null(maxRows) || count > maxRows) {
        maxRows <- count
      }
    }
  }
  if(maxRows > 0) {    
    for(i in 1:maxRows) {
      row <- NULL
      for(j in 0:23) {
        result <- pentad[[as.character(j)]]
        if(is.null(row)) {
          if(is.null(result)) {
            row <- ","
          } else {
            row <- paste(result[i,"names"], result[i, "correlations"], sep=",")
          }
        } else {
          if(is.null(result)) {
            row <- paste(row, ",", sep=",")
          } else {
            row <- paste(row, result[i,"names"], result[i, "correlations"],sep=",")
          }
        }
      }
      writeLines(row)
    }
  }
  sink()
}