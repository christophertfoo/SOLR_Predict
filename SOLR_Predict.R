library("multicore")
library("e1071")
library("rpart")
library("nnet")

numCores <- multicore:::detectCores()

# Gets the names of all CSV files in the current working directory.
#
# Returns:
#   -A vector containing the names of all CSV files in the directory.
getCsv <- function() {
  csvFiles <- c();
  for(i in 1:length(dir())) {
    if(length(grep("csv", dir()[i], fixed=TRUE)) > 0){
      csvFiles <- c(csvFiles, dir()[i]);
    }
  }
  return(csvFiles);
}

# Merges the data for all of the given CSV files.
#
# Parameter:
#   -dataSource = The name of the data source (i.e. location) that should have its CSV files merged and loaded.
#
# Returns:
#   -The merged CSV files as a data frame.
mergeCsv <- function(dataSource) {
  dirFiles <- dir()
  csvFiles <- character(0)
  indexes <- grep(dataSource, dirFiles)
  for(i in 1:length(indexes)) {
    csvFiles <- c(csvFiles, dirFiles[indexes[i]])
  }
  print(csvFiles)
  first <- T;
  
  start <- proc.time()
  for(i in 1:length(csvFiles)) {
    if(first) {
      merged <- read.csv(csvFiles[i]);
      first <- F;
    }
    else {
      merged <- rbind(merged, read.csv(csvFiles[i]));
    }
  }
  print("Reading:")
  print((proc.time() - start))
  merged[["DT"]] <- convertDate(merged)
  merged[["DT_NUM"]] <- as.numeric(merged[["DT"]])
  merged[["TIME"]] <- convertTime(merged)
  merged$DAY <- NULL
  merged$YEAR <- NULL
  merged$HR <- NULL
  merged$MIN <- NULL
  merged$TMZN <- NULL
  merged <- merged[order(merged$DT_NUM, decreasing=F),]
  return(merged);
}

# Filters all rows with 0 or NA in the given column in the given data frame.
#
# Paramters:
#   -data = The data frame to filter.
#   -col  = The column to filter on.
#
# Returns:
#   -A data frame with the filtered rows removed.
filterData <-function(data, col) {
  return(data[data[[col]] > 0 & !is.na(data[[col]]),])
}

# Parallelize this!
convertDate <- function(data) {
  dateStrings <- NULL
  start <- proc.time()
  for(i in 1:nrow(data)) {
    if(length(dateStrings) == 0) {
      dateStrings <- paste(data[["MON"]][i], data[["DAY"]][i], data[["YEAR"]][i], data[["HR"]][i], data[["MIN"]][i])
    }
    else {
      dateStrings <- c(dateStrings, paste(data[["MON"]][i], data[["DAY"]][i], data[["YEAR"]][i], data[["HR"]][i], data[["MIN"]][i]))			
    }	
  }
  print("DateTime Strings:")
  print((proc.time() - start))
  
  start <- proc.time()
  returnValue <- strptime(dateStrings, "%m %d %Y %H %M")
  print("DateTime conversion:")
  print((proc.time() - start))
  return(returnValue)
}

convertTime <- function(data)  {
  timeValues <- NULL
  start <- proc.time()
  for(i in 1:nrow(data)) {
    if(length(timeValues) == 0) {
      timeValues <- data[["HR"]][i] * 60+ data[["MIN"]][i]
    }
    else {
      timeValues <- c(timeValues, data[["HR"]][i] * 60+ data[["MIN"]][i])  		
    }	
  }
  print("Time:") 
  print((proc.time() - start))
  return(timeValues)
}

convertNightDay <- function(data, colName, threshold) {
  nightDay <-logical(0)
  for(i in 1:nrow(data)) {
    if(length(nightDay) == 0) {
      nightDay <- !is.na(data[[colName]][i]) & data[[colName]][i] < threshold
    } 
    else {
      nightDay <- c(nightDay, !is.na(data[[colName]][i]) & data[[colName]][i] < threshold)
    }
  }
  data[["ND"]] <- as.factor(nightDay)
  return(data)
}

offsetRow <- function(numrows, col, data) {
  tempData <- data[(numrows+1):nrow(data),]
  newCol <- tempData[order(tempData$DT_NUM, decreasing=F),col]
  for(i in 1:numrows) {
    newCol <- c(newCol, NA)
  }
  return(newCol)
}

dataOffset <- function(numrows, col, data) {
  ignoreList <- c("MON", "DT", "DT_NUM", "TIME")
  colNames <- names(data)
  for(i in 1:(numrows - 1)) {
    for(j in 1:length(colNames)) {
      if(!(colNames[j] %in% ignoreList)){
        data[[paste(colNames[j],"_",i,sep="")]] <- offsetRow(i, colNames[j], data)
      }
    }
  }
  data[["MON"]] <- offsetRow(numrows, "MON", data)
  data[["DT"]] <- offsetRow(numrows, "DT", data)
  data[["DT_NUM"]] <- offsetRow(numrows, "DT_NUM", data)
  data[["TIME"]] <- offsetRow(numrows, "TIME", data)
  data[[paste(col,"_",numrows,sep="")]] <- offsetRow(numrows, col,data)
  data <- data[1:(nrow(data)-numrows),]
  data <- data[order(data$DT_NUM, decreasing=F),]
  return(data)
}

runCorrelation <- function(index, xCol, yCol, data, selectedMethod) {
  correlation <- cor(data[[xCol]], data[[yCol]], use="pairwise.complete.obs", method=selectedMethod)
  return(list(index=index, name=xCol, correlation=correlation))  
}

getNumStarted <- function(statusList) {
  count <- 0
  for(i in 1:length(statusList)) {
    if(statusList[[i]]$started) {
      count <- count + 1
    }
  }
  return(count)
}

getNumReturned <- function(statusList) {
  count <- 0
  for(i in 1:length(statusList)) {
    if(statusList[[i]]$returned) {
      count <- count + 1
    }
  }
  return(count)
}

getResults <- function(statusList) {
  colNames <- character(0)
  correlations <- numeric(0)
  for(i in 1:length(statusList)) {
    colNames <- c(colNames, statusList[[i]]$result$name)
    correlations <- c(correlations, statusList[[i]]$result$correlation)
  }
  return(data.frame(names=colNames, correlations=correlations))
}

filterColumns <- function(data) {
  colNames <- names(data)
  for(i in 1:length(colNames)) {
    numNA <- 0
    numRows <- nrow(data)
    for(j in 1:numRows) {
      if(is.na(data[[colNames[i]]][j])) {
        numNA <- numNA + 1
      }
    }
    if(numNA / numRows >= 0.3) {
      print(paste("Removing:", colNames[i], paste("(",numNA," NAs)",sep="")))
      data[[colNames[i]]] <- NULL
    }
  }
  return(data)
}

correlate <- function(col, data, selectedMethod="pearson") {
  nonNumbers <- c()
  colNames <- names(data)
  for(i in 1:length(colNames)) {
    if(!is.numeric(data[,colNames[i]][0])) {
      nonNumbers <- c(nonNumbers, colNames[i])
    }
  }
  
  numNonNum <- length(nonNumbers)
  
  if(numNonNum > 0) {
    print("Ignoring:")
    for(i in 1:numNonNum){
      print(nonNumbers[i])
    }
  }
  
  validCol <- Filter(function(i){return(!(i %in% nonNumbers || i == col))}, names(data))
  numCols <- length(validCol)
  maxConcurrent <- numCols
  
  print(paste("Num Columns:", as.character(numCols)))
  
  if(maxConcurrent > numCores) {
    maxConcurrent <- numCores
  }
  numProcessed <- 1
  statusList <- "DUMMY"
  
  for(i in 1:numCols) {
    if(i == 1) {
      statusList <- list(list(started=F,returned=F, result=NULL))
    }  
    else {
      statusList <- c(statusList, list(list(started=F,returned=F,result=NULL)))
    }
  }
  
  nextIndex <- 1
  for(i in 1:maxConcurrent) {
    print(paste("Starting", nextIndex))
    parallel(expr=runCorrelation(nextIndex, validCol[nextIndex], col, data, selectedMethod))
    statusList[[nextIndex]]$started <- T
    nextIndex <- nextIndex + 1
  }
  
  numReturned <- 0
  result <- NULL
  
  launchNew <- function(result){
    numAvailable <- 0
    for(i in 1:length(result)) {
      
      if(is.null(result) || as.character(result[i]) != "NULL") {
        index <- result[[i]]$index
        
        if(statusList[[index]]$returned == F) {
          print(paste("Results for Index", index, "returned"))
          statusList[[index]]$returned <<- T
          statusList[[index]]$result <<- result[[i]]
          numAvailable <- numAvailable + 1
        }
      }
    }
    print("------------------------------------------------------------");
    
    if(numAvailable > 0) {
      for(i in 1:numAvailable) {
        if(nextIndex <= numCols) {
          print(paste("Starting", as.character(nextIndex)))
          parallel(expr=runCorrelation(nextIndex, validCol[nextIndex], col, data, selectedMethod))
          statusList[[nextIndex]]$started <<- T
          nextIndex <<- nextIndex + 1
        }
      }
    }
  }
  
  while(getNumReturned(statusList) < numCols) {
    collect(intermediate=launchNew)
  }
  return(getResults(statusList))
}

goodCorrelateThresh <- function(col, data, threshold, selectedMethod="pearson") {
  results <- correlate(col, data, selectedMethod)
  good <- numeric(0)
  goodNames <- character(0)
  for(i in 1:nrow(results)) {
    correlation <- results$correlations[[i]]
    if(is.numeric(correlation) & !is.na(correlation) & abs(correlation) >= threshold) {
      good <- c(good, correlation)
      goodNames <- c(goodNames, as.character(results$names[[i]]))
    }
  }
  
  goodResults <- data.frame(attribute=goodNames, correlation=good)
  return(goodResults)
}

goodCorrelateTop <- function(col, data, topThreshold, selectedMethod="pearson") {
  results <- correlate(col, data, selectedMethod)
  good <- numeric(0)
  goodNames <- character(0)
  results <- results[order(abs(results$correlations), decreasing=T),]
  return(results[1:topThreshold,])
}

convertTreeResult <- function(result) {
  returnList <- character(0)
  colNames <- colnames(result)
  numCols <- ncol(result)
  for(i in 1:nrow(result)) {
    prediction <- 1
    predictionValue <- result[i, 1]
    for(j in 2:numCols) {
      if(result[i, j] > predictionValue) {
        predictionValue <- result[i, j]
        prediction <- j
      }
    }
    if(length(returnList) == 0) {
      returnList <- colNames[prediction]
    }
    else {
      returnList <- c(returnList, colNames[prediction])
    }
  }
  return(returnList)
}

testTreeModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- convertTreeResult(predict(model, data))
  errors <- 0
  for(i in 1:nrow(data)) {
    if(data[[colName]][i] != predicted[i]) {
      errors <- errors + 1
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  return(list(error_rate=(errors / nrow(data))))
}

testNbModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- predict(model, data)
  errors <- 0
  for(i in 1:nrow(data)) {
    if(data[[colName]][i] != predicted[i]) {
      errors <- errors + 1
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  return(list(error_rate=(errors / nrow(data))))
}

testVectorModelSolr <- function(model, data, colName, verbose=F) {
  predictedVector <- predict(model, data)
  errors <- 0
  errorMargin <- 0
  actualSum <- 0
  for(i in 1:nrow(data)) {
    actual <- data[[colName]][i]
    predicted <- predictedVector[i]
    if(!is.na(actual) & !is.na(predicted) & actual != predicted) {
      errors <- errors + 1
      errorMargin <- errorMargin + abs(actual - predicted)
      actualSum <- actualSum + actual
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  return(list(error_rate=(errors / nrow(data)), error_margin=(errorMargin / errors), error_percent=(errorMargin / actualSum)))
}

testClassModelSolr <- function(model, data, colName, verbose=F) {
  predictedClasses <- predict(model, data)
  errors <- 0
  errorMargin <- 0
  actualSum <- 0
  for(i in 1:nrow(data)) {
    actual <- averageFactor(data[[colName]][i])
    predicted <- averageFactor(predictedClasses[i])
    if(is.na(actual) || is.na(predicted)) {
      print("NA")
    }
    if(!is.na(actual) & !is.na(predicted) & actual != predicted) {
      errors <- errors + 1
      errorMargin <- errorMargin + abs(actual - predicted)
      actualSum <- actualSum + actual
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
  }
  else {
    resultErrorMargin <- (errorMargin / errors)
    resultErrorPercent <- (errorMargin / actualSum)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin= resultErrorMargin, error_percent=resultErrorPercent))
}

makeTreeNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  training <- data[data$MON %in% start:(start + intervalSize - 1),]
  return(rpart(formula=as.formula(modelFormula), data=training))
}

makeNbNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  training <- data[data$MON %in% start:(start + intervalSize - 1),]
  return(naiveBayes(formula=as.formula(modelFormula), data=training))
}

makeLmSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(lm(formula=as.formula(modelFormula), data=training))
}

makeTreeSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(rpart(formula=as.formula(modelFormula), data=training, method="anova"))
}

makeNbSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(naiveBayes(formula=as.formula(modelFormula), data=training))
}

makeAnnSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(nnet(formula=as.formula(modelFormula), data=training, size=10))
}

makeRandomForestSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(randomForest(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

averageTestResults <- function(results, column) {
  total <- 0
  numResults <- length(results)
  for(i in 1:numResults) {
    total <- total + results[[i]][[column]][1]
  }
  return(total / numResults)
}

tenFold <- function(data, makeModelFun, intervalSize, start, testFun, colName, modelFormula, verbose=F) {
  if(nrow(data) < 10) {
    return(NULL)
  }
  folds <- split(data, sample(rep(1:10, nrow(data)/10)))
  overallError <- numeric(0)
  for(i in 1:10) {
    test <- folds[[i]]
    temp <- Filter(function(x){return(!(identical(x, test)))}, folds)
    training <- NULL
    for(j in 1:length(temp)) {
      if(j == 1) {
        training <- temp[[j]]
      }
      else {
        training <- rbind(training, temp[[j]])
      }
    }
    
    model <- makeModelFun(training, intervalSize, start, modelFormula)
    error <- testFun(model, test, colName, verbose)
    overallError <- c(overallError, list(error))
    
    if(verbose) {
      print("-----------------------------------------------------------------")
      print(paste("Fold", i, ":", (error[["error_rate"]] * 100),"% Error Rate"))
    }
  }
  if(verbose) {
    print(paste("Average Error Rate:", (averageTestResults(overallError, "error_rate") * 100), "%"))
  }
  return(overallError)
}

testNightDay <- function(data, intervalSize, verbose=F) {
  nbResults <- vector()
  if(verbose) {
    print("Naive Bayes:")
  }
  for(i in 1:ceiling(12 / intervalSize)) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    nbResults <- c(nbResults, tenFold(subset(data, MON %in% start:end), makeNbNightDay, intervalSize, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", verbose))
  }
  
  dtResults <- vector()
  if(verbose) {
    print("Decision Tree:")
  }
  for(i in 1:ceiling(12 / intervalSize)) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    dtResults <- c(dtResults, tenFold(subset(data, MON %in% start:end), makeTreeNightDay, intervalSize, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", verbose))
  }
  if(verbose) {
    print("-----------------------------------------------------------------------------------------------------------------")
  }
  print(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
  print(paste("Average Error Rate for Decision Tree:", (averageTestResults(dtResults, "error_rate") * 100), "%"))
}

testNightDayThreshold <- function(data, intervalSize, solrColName, maxThreshold, verbose=F) {
  for(i in 1:maxThreshold) {
    print(paste("Threshold", i, ":"))
    testData <- convertNightDay(data, solrColName, i)
    testNightDay(testData, intervalSize, verbose)
  }
}

testSolr <- function(data, intervalSize, colName, modelFunction, numCuts, verbose=F, checkRandomForest=F) {
  testData <- data[!is.na(data[[colName]]) & data[[colName]] > 1,]
  testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
  numIntervals <- ceiling(1440 / intervalSize)
  classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
  
  #   lmResults <- vector()
  #   if(verbose) {
  #     print("Linear Regression:")
  #   }
  #   for(i in 1:numIntervals) {
  #     start <- 1 + ((i - 1) * intervalSize)
  #     end <- start + intervalSize - 1
  #     
  #     lmResults <- c(lmResults, tenFold(subset(testData, TIME %in% start:end), makeLmSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, verbose))
  #   }
  #   
  #   dtResults <- vector()
  #   if(verbose) {
  #     print("Decision Tree:")
  #   }
  #   for(i in 1:numIntervals) {
  #     start <- 1 + ((i - 1) * intervalSize)
  #     end <- start + intervalSize - 1
  #     
  #     dtResults <- c(dtResults, tenFold(subset(testData, TIME %in% start:end), makeTreeSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, verbose))
  #   }
  
  
  
#   nbResults <- vector()
#   if(verbose) {
#     print("Naive Bayes:")
#   }
#   for(i in 1:numIntervals) {
#     start <- 1 + ((i - 1) * intervalSize)
#     end <- start + intervalSize - 1
#     
#     nbResults <- c(nbResults, tenFold(subset(testData, TIME %in% start:end), makeNbSolr, intervalSize, start, testClassModelSolr, "SOLR_D", classModelFunction, verbose))
#   }
  
  annResults <- vector()
  if(verbose) {
    print("ANN:")
  }
  for(i in 1:numIntervals) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    
    annResults <- c(annResults, tenFold(subset(testData, TIME %in% start:end), makeAnnSolr, intervalSize, start, testClassModelSolr, "SOLR_D", classModelFunction, verbose))
  }
  
  if(checkRandomForest) {
    rfResults <- vector()
    if(verbose) {
      print("Random Forest:")
    }
    for(i in 1:numIntervals) {
      start <- 1 + ((i - 1) * intervalSize)
      end <- start + intervalSize - 1
      
      rfResults <- c(rfResults, tenFold(subset(testData, TIME %in% start:end), makeRandomForestSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, verbose))
    }
  }
  #   print(paste("Average Error Rate for Linear Regression:", (averageTestResults(lmResults, "error_rate") * 100), "%"))
  #   print(paste("Average Error Margin for Linear Regression:", (averageTestResults(lmResults, "error_margin"))))
  #   print(paste("Average Error Percentage for Linear Regression:", (averageTestResults(lmResults, "error_percent")), "%"))
  #   print(paste("Average Error Rate for Decision Tree:", (averageTestResults(dtResults, "error_rate") * 100), "%"))
  #   print(paste("Average Error Margin for Decision Tree:", (averageTestResults(dtResults, "error_margin"))))
  #   print(paste("Average Error Percentage for Decision Tree:", (averageTestResults(dtResults, "error_percent")), "%"))
#   print(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
#   print(paste("Average Error Margin for Naive Bayes:", (averageTestResults(nbResults, "error_margin"))))
#   print(paste("Average Error Percentage for Naive Bayes:", (averageTestResults(nbResults, "error_percent")), "%"))
    print(paste("Average Error Rate for ANN:", (averageTestResults(annResults, "error_rate") * 100), "%"))
    print(paste("Average Error Margin for ANN:", (averageTestResults(annResults, "error_margin"))))
    print(paste("Average Error Percentage for ANN:", (averageTestResults(annResults, "error_percent")), "%"))
  if(checkRandomForest) {
    print(paste("Average Error Rate for Random Forest:", (averageTestResults(rfResults, "error_rate") * 100), "%"))
    print(paste("Average Error Margin for Random Forest:", (averageTestResults(rfResults, "error_margin"))))
    print(paste("Average Error Percentage for Random Forest:", (averageTestResults(rfResults, "error_percent")), "%"))
  }
}

mergeDataFrames <- function(destFrame, sourceFrame, sourceName) {
  ignoreList <- c("MON", "DT", "DT_NUM", "TIME", "SINT")
  start <- sourceFrame[["DT_NUM"]][1]
  end <- sourceFrame[["DT_NUM"]][nrow(sourceFrame)]
  inRange <- subset(destFrame, DT_NUM >= start & DT_NUM < end)
  colNames <- names(sourceFrame)
  for(i in 1:length(colNames)) {
    if(!(colNames[i] %in% ignoreList)) {
      inRange[[paste(sourceName,"_",colNames[i], sep="")]] <- approx(x=sourceFrame[["DT_NUM"]], y=sourceFrame[[colNames[i]]], xout=inRange[["DT_NUM"]])$y
    }
  }
  return(inRange)
}

# Writes the given data frame to a CSV file.
#
# Parameters:
#   -data = The data frame to write.
#   -dest = The CSV file to write to.
writeCsv <- function(data, dest) {
  write.csv(data, dest, row.names=F, na="")
}

# Averages a factor representing the value range created by discretizing the data with the cut function.
#
# Parameters:
#   -factor = The factor to average in the form "(#,#]" or "[#,#)".
#
# Returns:
#  -The average of the two numeric values in the factor.
averageFactor <- function(factor) {
  factorString <- as.character(factor)
  factorString <- gsub("(", "", factorString, fixed=T)
  factorString <- gsub("]", "", factorString, fixed=T)
  splitFactor <- strsplit(factorString, ",", fixed=T)
  start <- unlist(splitFactor[[1]])[1]
  end <- unlist(splitFactor[[1]])[2]
  return((as.numeric(start) + as.numeric(end)) / 2)
}