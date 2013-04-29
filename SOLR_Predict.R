library("multicore")
library("e1071")
library("rpart")
library("nnet")
library('randomForest')

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
  writeLines(csvFiles)
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
  writeLines("Reading:")
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
  dateStrings <- vector(mode="character", length=nrow(data))
  start <- proc.time()
  for(i in 1:nrow(data)) {
    dateStrings[i] <- paste(data[["MON"]][i], data[["DAY"]][i], data[["YEAR"]][i], data[["HR"]][i], data[["MIN"]][i])
  }
  writeLines("DateTime Strings:")
  print((proc.time() - start))
  
  start <- proc.time()
  returnValue <- strptime(dateStrings, "%m %d %Y %H %M")
  writeLines("DateTime conversion:")
  print((proc.time() - start))
  return(returnValue)
}

convertTime <- function(data)  {
  timeValues <- vector(mode="numeric", length=nrow(data))
  start <- proc.time()
  for(i in 1:nrow(data)) {
    timeValues[i] <- data[["HR"]][i] * 60+ data[["MIN"]][i]
  }
  writeLines("Time:") 
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
    if(numNA / numRows >= 0.5) {
      writeLines(paste("Removing:", colNames[i], paste("(",numNA," NAs)",sep="")))
      data[[colNames[i]]] <- NULL
    }
  }
  return(data)
}

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
  maxConcurrent <- numCols
  
  writeLines(paste("Num Columns:", as.character(numCols)))
  
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
    if(debug) {
      writeLines(paste("Starting", nextIndex))
    }
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
          if(debug) {
            writeLines(paste("Results for Index", index, "returned"))
          }
          statusList[[index]]$returned <<- T
          statusList[[index]]$result <<- result[[i]]
          numAvailable <- numAvailable + 1
        }
      }
    }
    if(debug) {
      writeLines("------------------------------------------------------------");
    }
    
    if(numAvailable > 0) {
      for(i in 1:numAvailable) {
        if(nextIndex <= numCols) {
          if(debug) {
            writeLines(paste("Starting", as.character(nextIndex)))
          }
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
  if(nrow(results) < topThreshold) {
    numResults <- nrow(results)
  }
  else {
    numResults <- topThreshold
  }
  results <- results[order(abs(results$correlations), decreasing=T),]
  return(results[1:numResults,])
}

logCorrelations <- function(col, data, filePrefix, getTop=F, numTop=5) {
  writeLines("Pearson:")
  sink(paste(filePrefix, "_pearson.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="pearson"))
  if(getTop) {
    writeLines("Top 20 Fields:")
    print(goodCorrelateTop(col=col, data=data, selectedMethod="pearson", topThreshold=numTop))
  }
  sink()
  writeLines("Spearman")
  sink(paste(filePrefix, "_spearman.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="spearman"))
  if(getTop) {
    writeLines("Top 20 Fields:")
    print(goodCorrelateTop(col=col, data=data, selectedMethod="spearman", topThreshold=numTop))
  }
  sink()
  writeLines("Kendall")
  sink(paste(filePrefix, "_kendall.out", sep=""))
  print(correlate(col=col, data=data, selectedMethod="kendall"))
  if(getTop) {
    writeLines("Top 20 Fields:")
    print(goodCorrelateTop(col=col, data=data, selectedMethod="kendall", topThreshold=numTop))
  }
  sink()
  writeLines("Done")
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
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

testNbModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- predict(model, data)
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

testAnnModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- predict(model, data, type="class")
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

testModelNightDayHelper <- function(model, predicted, data, colName, verbose=F) {
  errors <- 0
  falsePositive <- 0
  falseNegative <- 0
  numRows <- nrow(data)
  for(i in 1:numRows) {
    actualValue <- data[[colName]][i]
    if(actualValue != predicted[i]) {
      errors <- errors + 1
      if(as.logical(actualValue)) {
        falseNegative <- falseNegative + 1
      }
      else {
        falsePositive <- falsePositive + 1
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", numRows))
  }
  return(list(error_rate=(errors / numRows), false_positive=(falsePositive / errors), false_negative=(falseNegative / errors)))
}

testVectorModelSolr <- function(model, data, colName, verbose=F) {
  predictedVector <- predict(model, data)
  errors <- 0
  errorMargins <- numeric(0)
  actualSum <- 0
  for(i in 1:nrow(data)) {
    actual <- data[[colName]][i]
    predicted <- predictedVector[i]
    if(!is.na(actual) & !is.na(predicted) & actual != predicted) {
      errors <- errors + 1
      errorMargins <- c(errorMargins, abs(actual - predicted))
      actualSum <- actualSum + actual
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  }
  else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorMargins) / actualSum)
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin=resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd))
}

testClassModelSolr <- function(model, data, colName, verbose=F) {
  predictedClasses <- predict(model, data)
  return(testClassModelSolrHelper(model, predictedClasses, data, colName, verbose))
}

testAnnModelSolr <- function(model, data, colName, verbose=F) {
  predictedClasses <- predict(model, data, type="class")  
  return(testClassModelSolrHelper(model, predictedClasses, data, colName, verbose))
}

testClassModelSolrHelper <- function(model, predictedClasses, data, colName, verbose=F) {
  errors <- 0
  actualSum <- 0
  errorMargins <- numeric(0)
  for(i in 1:nrow(data)) {
    actual <- averageFactor(data[[colName]][i])
    predicted <- averageFactor(predictedClasses[i])
    if(is.na(actual) || is.na(predicted)) {
      print("NA")
    }
    if(!is.na(actual) & !is.na(predicted) & actual != predicted) {
      errors <- errors + 1
      errorMargins <- c(errorMargins, abs(actual - predicted))
      actualSum <- actualSum + actual
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  }
  else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorMargins) / actualSum)
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin= resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd))
}

makeTreeNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
  training <- data
  }
  else {
    training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(rpart(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

makeNbNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
  training <- data
  }
  else {
    training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(naiveBayes(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

makeAnnNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
    training <- data
  }
  else {
  training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(nnet(formula=as.formula(modelFormula), data=training, na.action=na.omit, size=1, trace=FALSE))
}

makeLmSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(lm(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

makeTreeSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(rpart(formula=as.formula(modelFormula), data=training, method="anova", na.action=na.omit))
}

makeNbSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(naiveBayes(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

makeAnnSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(nnet(formula=as.formula(modelFormula), data=training, size=1, na.action=na.omit, trace=FALSE, MaxNWts=5000))
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
      writeLines("-----------------------------------------------------------------")
      writeLines(paste("Fold", i, ":", (error[["error_rate"]] * 100),"% Error Rate"))
    }
  }
  if(verbose) {
    writeLines(paste("Average Error Rate:", (averageTestResults(overallError, "error_rate") * 100), "%"))
  }
  return(overallError)
}

testNightDay <- function(data, intervalSize, checkAnn=F, verbose=F) {
  nbResults <- vector()
  dtResults <- vector()
  annResults <- vector()
  bestResults <- vector()
  
  for(i in 1:ceiling(12 / intervalSize)) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    if(verbose) {
      writeLines(paste("Interval", start, "-", end, ":"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
    intervalResult <- tenFold(subset(data, MON %in% start:end), makeNbNightDay, intervalSize, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", F)
    nbResults <- c(nbResults, intervalResult)
    bestResult <- intervalResult
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    }
    
    intervalResult <- tenFold(subset(data, MON %in% start:end), makeTreeNightDay, intervalSize, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
    dtResults <- c(dtResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines("")
      writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for Decision Tree:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for Decision Tree:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
    
    if(checkAnn) {
      intervalResult <- tenFold(subset(data, MON %in% start:end), makeAnnNightDay, intervalSize, start, testAnnModelNightDay, "ND", "ND ~ TIME + MON", F)
      annResults <- c(annResults, intervalResult)
      
      if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
        bestResult <- intervalResult
      }
      
      if(verbose) {
        writeLines("")
        writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval False Positive % for ANN:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
        writeLines(paste("Interval False Negative % for ANN:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
        writeLines("-----------------------------------------------------------------------------------------------------------------")
      }
    }
    
    bestResults <- c(bestResults, bestResult)
  }
  writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Naive Bayes:", (averageTestResults(nbResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Naive Bayes:", (averageTestResults(nbResults, "false_negative") * 100), "%"))
  writeLines("")
  writeLines(paste("Average Error Rate for Decision Tree:", (averageTestResults(dtResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Decision Tree:", (averageTestResults(dtResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Decision Tree:", (averageTestResults(dtResults, "false_negative") * 100), "%"))
  if(checkAnn) {
    writeLines("")
    writeLines(paste("Average Error Rate for ANN:", (averageTestResults(annResults, "error_rate") * 100), "%"))
    writeLines(paste("Average False Positive % for ANN:", (averageTestResults(annResults, "false_positive") * 100), "%"))
    writeLines(paste("Average False Negative % for ANN:", (averageTestResults(annResults, "false_negative") * 100), "%"))
  }
  writeLines("")
  writeLines(paste("Average Error Rate for Best:", (averageTestResults(bestResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Best:", (averageTestResults(bestResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Best:", (averageTestResults(bestResults, "false_negative") * 100), "%"))
}

testNightDaySeason <- function(data, checkAnn=F, verbose=F) {
  nbResults <- vector()
  dtResults <- vector()
  annResults <- vector()
  bestResults <- vector()
  
  if(verbose) {
    writeLines(paste("Interval Spring:"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  intervalResult <- tenFold(subset(data, MON %in% c(3, 4, 5)), makeNbNightDay, 0, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", F)
  nbResults <- c(nbResults, intervalResult)
  bestResult <- intervalResult
  
  if(verbose) {
    writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
  }
  
  intervalResult <- tenFold(subset(data, MON %in% c(3, 4, 5)), makeTreeNightDay, 0, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
  dtResults <- c(dtResults, intervalResult)
  
  if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
    bestResult <- intervalResult
  }
  
  if(verbose) {
    writeLines("")
    writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Decision Tree:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Decision Tree:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  
  if(checkAnn) {
    intervalResult <- tenFold(subset(data, MON %in% c(3, 4, 5)), makeAnnNightDay, 0, start, testAnnModelNightDay, "ND", "ND ~ TIME + MON", F)
    annResults <- c(annResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines("")
      writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for ANN:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for ANN:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
  }
  
  bestResults <- c(bestResults, bestResult)
  
  if(verbose) {
    writeLines(paste("Interval Summer:"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  intervalResult <- tenFold(subset(data, MON %in% c(6, 7, 8)), makeNbNightDay, 0, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", F)
  nbResults <- c(nbResults, intervalResult)
  bestResult <- intervalResult
  
  if(verbose) {
    writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
  }
  
  intervalResult <- tenFold(subset(data, MON %in% c(6, 7, 8)), makeTreeNightDay, 0, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
  dtResults <- c(dtResults, intervalResult)
  
  if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
    bestResult <- intervalResult
  }
  
  if(verbose) {
    writeLines("")
    writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Decision Tree:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Decision Tree:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  
  if(checkAnn) {
    intervalResult <- tenFold(subset(data, MON %in% c(6, 7, 8)), makeAnnNightDay, 0, start, testAnnModelNightDay, "ND", "ND ~ TIME + MON", F)
    annResults <- c(annResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines("")
      writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for ANN:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for ANN:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
  }
  
  bestResults <- c(bestResults, bestResult)
  
  if(verbose) {
    writeLines(paste("Interval Fall:"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  intervalResult <- tenFold(subset(data, MON %in% c(9, 10, 11)), makeNbNightDay, 0, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", F)
  nbResults <- c(nbResults, intervalResult)
  bestResult <- intervalResult
  
  if(verbose) {
    writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
  }
  
  intervalResult <- tenFold(subset(data, MON %in% c(9, 10, 11)), makeTreeNightDay, 0, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
  dtResults <- c(dtResults, intervalResult)
  
  if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
    bestResult <- intervalResult
  }
  
  if(verbose) {
    writeLines("")
    writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Decision Tree:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Decision Tree:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  
  if(checkAnn) {
    intervalResult <- tenFold(subset(data, MON %in% c(9, 10, 11)), makeAnnNightDay, 0, start, testAnnModelNightDay, "ND", "ND ~ TIME + MON", F)
    annResults <- c(annResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines("")
      writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for ANN:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for ANN:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
  }
  
  bestResults <- c(bestResults, bestResult)
  
  if(verbose) {
    writeLines(paste("Interval Winter:"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  intervalResult <- tenFold(subset(data, MON %in% c(12, 1, 2)), makeNbNightDay, 0, start, testNbModelNightDay, "ND", "ND ~ TIME + MON", F)
  nbResults <- c(nbResults, intervalResult)
  bestResult <- intervalResult
  
  if(verbose) {
    writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
  }
  
  intervalResult <- tenFold(subset(data, MON %in% c(12, 1, 2)), makeTreeNightDay, 0, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
  dtResults <- c(dtResults, intervalResult)
  
  if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
    bestResult <- intervalResult
  }
  
  if(verbose) {
    writeLines("")
    writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
    writeLines(paste("Interval False Positive % for Decision Tree:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
    writeLines(paste("Interval False Negative % for Decision Tree:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  
  if(checkAnn) {
    intervalResult <- tenFold(subset(data, MON %in% c(12, 1, 2)), makeAnnNightDay, 0, start, testAnnModelNightDay, "ND", "ND ~ TIME + MON", F)
    annResults <- c(annResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines("")
      writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for ANN:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for ANN:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
      writeLines("-----------------------------------------------------------------------------------------------------------------")
    }
  }
  
  bestResults <- c(bestResults, bestResult)
  
  writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Naive Bayes:", (averageTestResults(nbResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Naive Bayes:", (averageTestResults(nbResults, "false_negative") * 100), "%"))
  writeLines("")
  writeLines(paste("Average Error Rate for Decision Tree:", (averageTestResults(dtResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Decision Tree:", (averageTestResults(dtResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Decision Tree:", (averageTestResults(dtResults, "false_negative") * 100), "%"))
  if(checkAnn) {
    writeLines("")
    writeLines(paste("Average Error Rate for ANN:", (averageTestResults(annResults, "error_rate") * 100), "%"))
    writeLines(paste("Average False Positive % for ANN:", (averageTestResults(annResults, "false_positive") * 100), "%"))
    writeLines(paste("Average False Negative % for ANN:", (averageTestResults(annResults, "false_negative") * 100), "%"))
  }
  
  writeLines("")
  writeLines(paste("Average Error Rate for Best:", (averageTestResults(bestResults, "error_rate") * 100), "%"))
  writeLines(paste("Average False Positive % for Best:", (averageTestResults(bestResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Best:", (averageTestResults(bestResults, "false_negative") * 100), "%"))
}

testNightDayThreshold <- function(data, solrColName, maxThreshold, verbose=F, checkAnn=F) {
  for(i in 1:maxThreshold) {
    writeLines(paste("Threshold", i, ":"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
    testData <- convertNightDay(data=data, colName=solrColName, threshold=i)
    testNightDay(data=testData, intervalSize=12, verbose=verbose, checkAnn=checkAnn)
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
}

testNightDayIntervals <- function(data, solrColName, verbose=F, checkAnn=F) {
  intervals <- c(12, 6, 4, 3, 2, 1)
  testData <- convertNightDay(data=data, colName=solrColName, threshold=1)
  for(i in 1:length(intervals)) {
    intervalSize <- intervals[i]
    writeLines(paste("Interval Size", intervalSize, ":"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
    testNightDay(data=testData, intervalSize=intervals[i], verbose=verbose, checkAnn=checkAnn)
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  writeLines("Interval Season:")
  writeLines("-----------------------------------------------------------------------------------------------------------------")
  testNightDaySeason(data=testData, checkAnn=checkAnn, verbose=verbose)
  writeLines("-----------------------------------------------------------------------------------------------------------------")
}

testSolr <- function(data, intervalSize, colName, modelFunction, numCuts, verbose=F, checkRandomForest=F, checkAnn=F) {
  testData <- data[!is.na(data[[colName]]) & data[[colName]] > 1,]
  testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
  numIntervals <- ceiling(1440 / intervalSize)
  classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
  
  lmResults <- vector()
  dtResults <- vector()
  nbResults <- vector()
  annResults <- vector()
  rfResults <- vector()
  bestResults <- vector()
  
  for(i in 1:numIntervals) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    if(verbose) {
      writeLines(paste("Interval", start, "-", end,":"))
      writeLines("--------------------------------------------------------------------")
    }
    
    intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeLmSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, F)
    lmResults <- c(lmResults, intervalResult)
    bestResult <- intervalResult
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Linear Regression:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval Error Margin for Linear Regression:", (averageTestResults(intervalResult, "error_margin"))))
      writeLines(paste("Interval Error Std Dev for Linear Regression:", (averageTestResults(intervalResult, "error_sd"))))
      writeLines(paste("Interval Error Percentage for Linear Regression:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      writeLines("")
    }
    
    intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeTreeSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, F)
    dtResults <- c(dtResults, intervalResult)
    
    if(is.null(bestResult) || averageTestResults(results=bestResult, column="error_margin") < averageTestResults(results=intervalResult, column="error_margin")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval Error Margin for Decision Tree:", (averageTestResults(intervalResult, "error_margin"))))
      writeLines(paste("Interval Error Std Dev for Decision Tree:", (averageTestResults(intervalResult, "error_sd"))))
      writeLines(paste("Interval Error Percentage for Decision Tree", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      writeLines("")
    }
    
    intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeNbSolr, intervalSize, start, testClassModelSolr, "SOLR_D", classModelFunction, F)
    nbResults <- c(nbResults, intervalResult)
    
    if(is.null(bestResult) || averageTestResults(results=bestResult, column="error_margin") < averageTestResults(results=intervalResult, column="error_margin")) {
      bestResult <- intervalResult
    }
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval Error Margin for Naive Bayes:", (averageTestResults(intervalResult, "error_margin"))))
      writeLines(paste("Interval Error Std Dev for Naive Bayes:", (averageTestResults(intervalResult, "error_sd"))))
      writeLines(paste("Interval Error Percentage for Naive Bayes:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
    }
    
    if(checkAnn) {
      intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeAnnSolr, intervalSize, start, testAnnModelSolr, "SOLR_D", classModelFunction, F)
      annResults <- c(annResults, intervalResult)
      
      if(is.null(bestResult) || averageTestResults(results=bestResult, column="error_margin") < averageTestResults(results=intervalResult, column="error_margin")) {
        bestResult <- intervalResult
      }
      
      if(verbose) {
        writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval Error Margin for ANN:", (averageTestResults(intervalResult, "error_margin"))))
        writeLines(paste("Interval Error Std Dev for ANN:", (averageTestResults(intervalResult, "error_sd"))))
        writeLines(paste("Interval Error Percentage for ANN:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      }
    }
    
    if(checkRandomForest) {
      intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeRandomForestSolr, intervalSize, start, testVectorModelSolr, colName, modelFunction, F)
      rfResults <- c(rfResults, intervalResult)
      
      if(is.null(bestResult) || averageTestResults(results=bestResult, column="error_margin") < averageTestResults(results=intervalResult, column="error_margin")) {
        bestResult <- intervalResult
      }
      
      if(verbose) {
        writeLines(paste("Interval Error Rate for Random Forest:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval Error Margin for Random Forest:", (averageTestResults(intervalResult, "error_margin"))))
        writeLines(paste("Interval Error Std Dev for Random Forest:", (averageTestResults(intervalResult, "error_sd"))))
        writeLines(paste("Interval Error Percentage for Random Forest:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      }
    }
    
    bestResults <- c(bestResults, bestResult)
    
    if(verbose) {
      writeLines("--------------------------------------------------------------------")
    }
  }
  
  writeLines(paste("Average Error Rate for Linear Regression:", (averageTestResults(lmResults, "error_rate") * 100), "%"))
  writeLines(paste("Average Error Margin for Linear Regression:", (averageTestResults(lmResults, "error_margin"))))
  writeLines(paste("Average Error Std Dev for Linear Regression:", (averageTestResults(lmResults, "error_sd"))))
  writeLines(paste("Average Error Percentage for Linear Regression:", (averageTestResults(lmResults, "error_percent") * 100), "%"))
  writeLines("")
  writeLines(paste("Average Error Rate for Decision Tree:", (averageTestResults(dtResults, "error_rate") * 100), "%"))
  writeLines(paste("Average Error Margin for Decision Tree:", (averageTestResults(dtResults, "error_margin"))))
  writeLines(paste("Average Error Std Dev for Decision Tree:", (averageTestResults(dtResults, "error_sd"))))
  writeLines(paste("Average Error Percentage for Decision Tree:", (averageTestResults(dtResults, "error_percent") * 100), "%"))
  writeLines("")
  writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
  writeLines(paste("Average Error Margin for Naive Bayes:", (averageTestResults(nbResults, "error_margin"))))
  writeLines(paste("Average Error Std Dev for Naive Bayes:", (averageTestResults(nbResults, "error_sd"))))
  writeLines(paste("Average Error Percentage for Naive Bayes:", (averageTestResults(nbResults, "error_percent") * 100), "%"))
  
  if(checkAnn) {
    writeLines("")
    writeLines(paste("Average Error Rate for ANN:", (averageTestResults(annResults, "error_rate") * 100), "%"))
    writeLines(paste("Average Error Margin for ANN:", (averageTestResults(annResults, "error_margin"))))
    writeLines(paste("Average Error Std Dev for ANN:", (averageTestResults(annResults, "error_sd"))))
    writeLines(paste("Average Error Percentage for ANN:", (averageTestResults(annResults, "error_percent") * 100), "%"))
  }
  if(checkRandomForest) {
    writeLines("")
    writeLines(paste("Average Error Rate for Random Forest:", (averageTestResults(rfResults, "error_rate") * 100), "%"))
    writeLines(paste("Average Error Margin for Random Forest:", (averageTestResults(rfResults, "error_margin"))))
    writeLines(paste("Average Error Std Dev for Random Forest:", (averageTestResults(rfResults, "error_sd"))))
    writeLines(paste("Average Error Percentage for Random Forest:", (averageTestResults(rfResults, "error_percent") * 100), "%"))
  }
  writeLines("")
  writeLines(paste("Average Error Rate for Best:", (averageTestResults(bestResults, "error_rate") * 100), "%"))
  writeLines(paste("Average Error Margin for Best:", (averageTestResults(bestResults, "error_margin"))))
  writeLines(paste("Average Error Std Dev for Best:", (averageTestResults(bestResults, "error_sd"))))
  writeLines(paste("Average Error Percentage for Best:", (averageTestResults(bestResults, "error_percent") * 100), "%"))
}

testSolrCuts <- function(data, colName, modelFunction, checkAnn=F) {
  testData <- data[!is.na(data[[colName]]) & data[[colName]] > 1,]
  targetCuts <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
  for(i in 1:length(targetCuts)) {
    numCuts <- targetCuts[i]
    writeLines(paste("Testing", numCuts,"cuts:"))
    writeLines("-------------------------------------------------------------------")
    testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
    classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
    nbResult <- tenFold(testData, makeNbSolr, 1440, 1, testClassModelSolr, "SOLR_D", classModelFunction, F)
    writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResult, "error_rate") * 100), "%"))
    writeLines(paste("Average Error Margin for Naive Bayes:", (averageTestResults(nbResult, "error_margin"))))
    writeLines(paste("Average Error Std Dev for Naive Bayes:", (averageTestResults(nbResult, "error_sd"))))
    writeLines(paste("Average Error Percentage for Naive Bayes:", (averageTestResults(nbResult, "error_percent") * 100), "%"))
    if(checkAnn) {
      annResult <- tenFold(testData, makeAnnSolr, 1440, 1, testAnnModelSolr, "SOLR_D", classModelFunction, F)
      writeLines("")
      writeLines(paste("Average Error Rate for ANN:", (averageTestResults(annResult, "error_rate") * 100), "%"))
      writeLines(paste("Average Error Margin for ANN:", (averageTestResults(annResult, "error_margin"))))
      writeLines(paste("Average Error Std Dev for ANN:", (averageTestResults(annResult, "error_sd"))))
      writeLines(paste("Average Error Percentage for ANN:", (averageTestResults(annResult, "error_percent") * 100), "%"))
    }
    writeLines("-------------------------------------------------------------------")
  }
}

testSolrIntervals <- function(data, colName, modelFunction, checkAnn=F, checkRandomForest=F) {
  intervals <- c(1440, 720, 480, 360, 240, 180, 120, 60)
  for(i in 1:length(intervals)) {
    intervalSize <- intervals[i]
    writeLines(paste("Interval Size", intervalSize,":"))
    writeLines("-------------------------------------------------------------------")
    testSolr(data, intervalSize, colName, modelFunction, 1000, verbose=T, checkAnn=checkAnn, checkRandomForest=checkRandomForest)
    writeLines("-------------------------------------------------------------------")
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

makeFullModelFunction <- function(data, colName, ignoreList=vector()) {
  functionString <- paste(colName, "~")
  first <- T
  for(i in names(data)) {
    if(i != colName & i != "DT" & !(i %in% ignoreList)) {
      if(first) {
        functionString <- paste(functionString, i)
        first <- F
      }
      else {
        functionString <- paste(functionString, "+", i)
      }
    }
  }
  return(functionString)
}