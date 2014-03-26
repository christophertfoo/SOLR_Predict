# Creates a Linear Regression model for predicting the solar irradiance value at the next data point.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in minutes, minimum is 60 minutes).
#  -start        = The starting time of the interval in minutes.
#  -modelFormula = The "formula" used to make the model in the form SOLR_# ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeLmSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(lm(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

# Creates a Decision Tree model for predicting the solar irradiance value at the next data point.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in minutes, minimum is 60 minutes).
#  -start        = The starting time of the interval in minutes.
#  -modelFormula = The "formula" used to make the model in the form SOLR_# ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeTreeSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(rpart(formula=as.formula(modelFormula), data=training, method="anova", na.action=na.omit))
}

# Creates a Naive Bayes model for predicting the solar irradiance value at the next data point.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in minutes, minimum is 60 minutes).
#  -start        = The starting time of the interval in minutes.
#  -modelFormula = The "formula" used to make the model in the form SOLR_# ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeNbSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(naiveBayes(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

# Creates a Artificial Neural Network model for predicting the solar irradiance value at the next data point.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in minutes, minimum is 60 minutes).
#  -start        = The starting time of the interval in minutes.
#  -modelFormula = The "formula" used to make the model in the form SOLR_# ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeAnnSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(nnet(formula=as.formula(modelFormula), data=training, size=1, na.action=na.omit, trace=FALSE, MaxNWts=5000))
}

# Creates a Random Forest model for predicting the solar irradiance value at the next data point.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in minutes, minimum is 60 minutes).
#  -start        = The starting time of the interval in minutes.
#  -modelFormula = The "formula" used to make the model in the form SOLR_# ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeRandomForestSolr <- function(data, intervalSize, start, modelFormula) {
  training <- data[data$TIME %in% start:(start + intervalSize - 1),]
  return(randomForest(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

# Tests a continuous model for predicting the solar irradiance.
#
# Parameters:
#  -model   = The model to be tested.
#  -data    = The data used to perform the prediction which also contains the actual values.
#  -colName = The name of the SOLR column to predict / the actual column.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the testing / error information.
testContinuousModelSolr <- function(model, data, colName, verbose=F) {
  predictedVector <- predict(model, data)
  errors <- 0
  errorMargins <- numeric(0)
  errorPercents <- numeric(0)
  actualSum <- 0
  for(i in 1:nrow(data)) {
    actual <- data[[colName]][i]
    predicted <- predictedVector[i]
    if(!is.na(actual) && !is.na(predicted) && actual != predicted) {
      errors <- errors + 1
      margin <- abs(actual - predicted)
      errorMargins <- c(errorMargins, margin)
      if(actual != 0) {
        errorPercents <- c(errorPercents, abs(margin / actual))
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  } else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorPercents) / errors) * 100
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin=resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd, time=data[["DT"]],actual=data[[colName]], predicted=predictedVector))
}

testContinuousModelSolrFrac <- function(model, data, colName, maxColName, verbose=F) {
  predictedVector <- predict(model, data)
  errors <- 0
  errorMargins <- numeric(0)
  errorPercents <- numeric(0)
  actualSum <- 0
  for(i in 1:nrow(data)) {
    max <- data[[maxColName]][i]
    actual <- data[[colName]][i] * max
    predicted <- predictedVector[i] * max
    if(!is.na(actual) && !is.na(predicted) && actual != predicted) {
      errors <- errors + 1
      margin <- abs(actual - predicted)
      errorMargins <- c(errorMargins, margin)
      if(actual != 0) {
        errorPercents <- c(errorPercents, abs(margin / actual))
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  } else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorPercents) / errors) * 100
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin=resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd, time=data[["DT"]],actual=data[[colName]], predicted=predictedVector))
}

testContinuousModelSolrDeseasonalized <- function(model, data, colName, deseasonalized_signal, deseasonalized_colName, verbose=F) {
  predictedVector <- predict(model, data)
  errors <- 0
  errorMargins <- numeric(0)
  errorPercents <- numeric(0)
  actualSum <- 0
  
  fixedPredicted <- numeric(0)
  fixedActual <- numeric(0)
  
  for(i in 1:nrow(data)) {
    deseasonalized <- deseasonalized_signal[which(deseasonalized_signal$MON == data[["MON"]][i] & deseasonalized_signal$DAY == data[["DAY"]][i] & deseasonalized_signal$HR == data[["HR"]][i] & deseasonalized_signal$MIN == data[["MIN"]][i]), deseasonalized_colName]
    actual <- data[[colName]][i] + deseasonalized
    predicted <- predictedVector[i] + deseasonalized
    fixedActual <- c(fixedActual, actual)
    fixedPredicted <- c(fixedPredicted, predicted)
    if(!is.na(actual) && !is.na(predicted) && actual != predicted) {
      errors <- errors + 1
      margin <- abs(actual - predicted)
      errorMargins <- c(errorMargins, margin)
      if(actual != 0) {
        errorPercents <- c(errorPercents, abs(margin / actual))
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  } else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorPercents) / errors) * 100
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin=resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd, time=data[["DT"]],actual=fixedActual, predicted=fixedPredicted))
}

# Tests a discrete / class-based model for predicting the solar irradiance.
#
# Parameters:
#  -model   = The model to be tested.
#  -data    = The data used to perform the prediction which also contains the actual values.
#  -colName = The name of the SOLR column to predict / the actual column.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the testing / error information.
testClassModelSolr <- function(model, data, colName, verbose=F) {
  predictedClasses <- predict(model, data)
  return(testClassModelSolrHelper(model, predictedClasses, data, colName, verbose))
}

# Tests an Artificial Neural Network model for predicting the solar irradiance.
#
# Parameters:
#  -model   = The model to be tested.
#  -data    = The data used to perform the prediction which also contains the actual values.
#  -colName = The name of the SOLR column to predict / the actual column.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the testing / error information.
testAnnModelSolr <- function(model, data, colName, verbose=F) {
  predictedClasses <- predict(model, data, type="class")  
  return(testClassModelSolrHelper(model, predictedClasses, data, colName, verbose))
}

# A helper function that determines the errors between the predicted classes and the actual
# classes.

# Parameters:
#  -model            = The model to be tested (unused?).
#  -predictedClasses = The results of the prediction.
#  -data             = The data used to perform the prediction which also contains the actual values.
#  -colName          = The name of the SOLR column to predict / the actual column.
#  -verbose          = If the function should print verbose output.
#
# Returns:
#  -A list containing the testing / error information.
testClassModelSolrHelper <- function(model, predictedClasses, data, colName, verbose=F) {
  errors <- 0
  actualSum <- 0
  errorMargins <- numeric(0)
  for(i in 1:nrow(data)) {
    actual <- averageFactor(data[[colName]][i])
    predicted <- averageFactor(predictedClasses[i])
    if(!is.na(actual) && !is.na(predicted) && actual != predicted) {
      errors <- errors + 1
      margin <- abs(actual - predicted)
      errorMargins <- c(errorMargins, margin)
      if(actual != 0) {
        errorPercents <- c(errorPercents, margin / actual)
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", nrow(data)))
  }
  
  if(errors == 0) {
    resultErrorMargin <- 0
    resultErrorPercent <- 0
    resultErrorSd <- 0
  } else {
    resultErrorMargin <- (sum(errorMargins) / errors)
    resultErrorPercent <- (sum(errorPercents) / errors) * 100
    resultErrorSd <- sd(errorMargins)
  }
  
  return(list(error_rate=(errors / nrow(data)), error_margin=resultErrorMargin, error_percent=resultErrorPercent, error_sd=resultErrorSd, time=data[["DT"]],actual=data[[colName]], predicted=predictedVector))
}

# Tests the various models for predicting the solar irradiance values and finds the best combinations of models.
#
# Parameters:
#  -data              = The data used to train / validate the models.
#  -intervalSize      = The size of each interval in minutes (minimum of 60 minutes).
#  -colName           = The name of the column containing the solar irradiance value to predict.
#  -modelFunction     = The "function" used to create each model.
#  -numCuts           = The number of classes to use for class-based models.
#  -verbose           = If verbose output should be printed.
#  -checkNb           = If Naive Bayes models should be generated and tested.
#  -checkRandomForest = If Random Forest models should be generated and tested.
#  -checkAnn          = If Artificial Neural Network models should be generated and tested.
#  -mean              = The mean / average solar irradiance value over the data set.  Used to "deseasonalize" the data if it != 0.
#  -filter            = If NA and obvious Night (<= 0) values should be removed.
#
# Returns:
#  -A list containing the best results (lowest values) in terms of absolute error margin and relative error margin and the model combinations
#   used to generate those best results.
testSolr <- function(data, intervalSize, colName, modelFunction, numCuts, verbose=F, checkNb=F, checkRandomForest=F, checkAnn=F, mean=0, filter=T) {
  if(filter) {
    testData <- data[!is.na(data[[colName]]) & data[[colName]] > 1,]
  } else {
    testData <- data
  }
  
  numIntervals <- ceiling(1440 / intervalSize)
  
  if(mean != 0) {
    testData <- meanNormalize(data=testData, colName=colName, mean=mean)
    colName <- paste(colName, "_NORM", sep="")
    modelFunction <- paste(colName ,"~", strsplit(modelFunction, "~")[[1]][2], sep="")
    testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
    classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
  } else {
    testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
    classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
  }
  
  
  lmResults <- vector()
  dtResults <- vector()
  nbResults <- vector()
  annResults <- vector()
  rfResults <- vector()
  bestResults <- vector()
  bestCombo <- character(0)
  bestRelResults <- vector()
  bestRelCombo <- character(0)
  
  for(i in 1:numIntervals) {
    start <- 1 + ((i - 1) * intervalSize)
    end <- start + intervalSize - 1
    if(verbose) {
      writeLines(paste("Interval", start, "-", end,":"))
      writeLines("--------------------------------------------------------------------")
    }
    
    intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeLmSolr, intervalSize, start, testContinuousModelSolr, colName, modelFunction, F)
    if(!is.null(intervalResult)) {
      lmResults <- c(lmResults, intervalResult)
    }
    if(is.null(intervalResult)) {
      bestResult <- NULL
      bestModel <- "NULL"
      bestRelResult <- NULL
      bestRelModel <- "NULL"
    } else {
      bestResult <- intervalResult
      bestModel <- "Linear Regression"
      bestRelResult <- intervalResult
      bestRelModel <- "Linear Regression"
    }
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Linear Regression:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval Error Margin for Linear Regression:", (averageTestResults(intervalResult, "error_margin"))))
      writeLines(paste("Interval Error Std Dev for Linear Regression:", (averageTestResults(intervalResult, "error_sd"))))
      writeLines(paste("Interval Error Percentage for Linear Regression:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      writeLines("")
    }
    
    intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeTreeSolr, intervalSize, start, testContinuousModelSolr, colName, modelFunction, F)
    if(!is.null(intervalResult)) {
      dtResults <- c(dtResults, intervalResult)
    }
    
    if((is.null(bestResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestResult, column="error_margin") > averageTestResults(results=intervalResult, column="error_margin"))) {
      bestResult <- intervalResult
      bestModel <- "Decision Tree"
    }
    
    if((is.null(bestRelResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestRelResult, column="error_percent") > averageTestResults(results=intervalResult, column="error_percent"))) {
      bestRelResult <- intervalResult
      bestRelModel <- "Decision Tree"
    }
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Decision Tree:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval Error Margin for Decision Tree:", (averageTestResults(intervalResult, "error_margin"))))
      writeLines(paste("Interval Error Std Dev for Decision Tree:", (averageTestResults(intervalResult, "error_sd"))))
      writeLines(paste("Interval Error Percentage for Decision Tree", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      writeLines("")
    }
    
    if(checkNb) {
      intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeNbSolr, intervalSize, start, testClassModelSolr, "SOLR_D", classModelFunction, F)
      nbResults <- c(nbResults, intervalResult)
      
      if((is.null(bestResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestResult, column="error_margin") > averageTestResults(results=intervalResult, column="error_margin"))) {
        bestResult <- intervalResult
        bestModel <- "Naive Bayes"
      }
      
      if((is.null(bestRelResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestRelResult, column="error_percent") > averageTestResults(results=intervalResult, column="error_percent"))) {
        bestRelResult <- intervalResult
        bestRelModel <- "Naive Bayes"
      }
      
      if(verbose) {
        writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval Error Margin for Naive Bayes:", (averageTestResults(intervalResult, "error_margin"))))
        writeLines(paste("Interval Error Std Dev for Naive Bayes:", (averageTestResults(intervalResult, "error_sd"))))
        writeLines(paste("Interval Error Percentage for Naive Bayes:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      }
    }
    
    if(checkAnn) {
      intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeAnnSolr, intervalSize, start, testAnnModelSolr, "SOLR_D", classModelFunction, F)
      annResults <- c(annResults, intervalResult)
      
      if((is.null(bestResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestResult, column="error_margin") > averageTestResults(results=intervalResult, column="error_margin"))) {
        bestResult <- intervalResult
        bestModel <- "ANN"
      }
      
      if((is.null(bestRelResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestRelResult, column="error_percent") > averageTestResults(results=intervalResult, column="error_percent"))) {
        bestRelResult <- intervalResult
        bestRelModel <- "ANN"
      }
      
      if(verbose) {
        writeLines(paste("Interval Error Rate for ANN:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval Error Margin for ANN:", (averageTestResults(intervalResult, "error_margin"))))
        writeLines(paste("Interval Error Std Dev for ANN:", (averageTestResults(intervalResult, "error_sd"))))
        writeLines(paste("Interval Error Percentage for ANN:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      }
    }
    
    if(checkRandomForest) {
      intervalResult <- tenFold(subset(testData, TIME %in% start:end), makeRandomForestSolr, intervalSize, start, testContinuousModelSolr, colName, modelFunction, F)
      rfResults <- c(rfResults, intervalResult)
      
      if((is.null(bestResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestResult, column="error_margin") > averageTestResults(results=intervalResult, column="error_margin"))) {
        bestResult <- intervalResult
        bestModel <- "Random Forest"
      }
      
      if((is.null(bestRelResult) && !is.null(intervalResult)) || (!is.null(intervalResult) && averageTestResults(results=bestRelResult, column="error_percent") > averageTestResults(results=intervalResult, column="error_percent"))) {
        bestRelResult <- intervalResult
        bestRelModel <- "Random Forest"
      }
      
      if(verbose) {
        writeLines(paste("Interval Error Rate for Random Forest:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
        writeLines(paste("Interval Error Margin for Random Forest:", (averageTestResults(intervalResult, "error_margin"))))
        writeLines(paste("Interval Error Std Dev for Random Forest:", (averageTestResults(intervalResult, "error_sd"))))
        writeLines(paste("Interval Error Percentage for Random Forest:", (averageTestResults(intervalResult, "error_percent") * 100), "%"))
      }
    }
    
    bestResults <- c(bestResults, bestResult)
    bestRelResults <- c(bestRelResults, bestRelResult)
    bestCombo <- c(bestCombo, bestModel)
    bestRelCombo <- c(bestRelCombo, bestRelModel)
    
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
  if(checkNb) {
    writeLines("")
    writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResults, "error_rate") * 100), "%"))
    writeLines(paste("Average Error Margin for Naive Bayes:", (averageTestResults(nbResults, "error_margin"))))
    writeLines(paste("Average Error Std Dev for Naive Bayes:", (averageTestResults(nbResults, "error_sd"))))
    writeLines(paste("Average Error Percentage for Naive Bayes:", (averageTestResults(nbResults, "error_percent") * 100), "%"))
  }
  
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
  bestErrorMargin <- averageTestResults(bestResults, "error_margin")
  writeLines(paste("Average Error Margin for Best:", bestErrorMargin))
  writeLines(paste("Average Error Std Dev for Best:", (averageTestResults(bestResults, "error_sd"))))
  bestErrorPercent <- averageTestResults(bestResults, "error_percent") * 100
  writeLines(paste("Average Error Percentage for Best:", bestErrorPercent, "%"))
  bestComboString <- getCombo(bestCombo)
  writeLines(paste("Best Combo:", bestComboString))
  writeLines("")
  writeLines(paste("Average Error Rate for Relative Best:", (averageTestResults(bestRelResults, "error_rate") * 100), "%"))
  bestRelErrorMargin <- averageTestResults(bestRelResults, "error_margin")
  writeLines(paste("Average Error Margin for Relative Best:", bestRelErrorMargin))
  writeLines(paste("Average Error Std Dev for Relative Best:", (averageTestResults(bestRelResults, "error_sd"))))
  bestRelErrorPercent <- averageTestResults(bestRelResults, "error_percent") * 100
  writeLines(paste("Average Error Percentage for Relative Best:", bestRelErrorPercent, "%"))
  bestRelComboString <- getCombo(bestRelCombo)
  writeLines(paste("Relative Best Combo:", bestRelComboString))
  
  return(list(best_margin=list(error_margin=bestErrorMargin, error_percent=bestErrorPercent, combo=bestComboString), best_relative=list(error_margin=bestRelErrorMargin, error_percent=bestRelErrorPercent, combo=bestRelComboString)))
}

# Tests the effects of the number of cuts (i.e. classes for class-based models) on the accuracy
# of the predictions.
#
# Parameters:
#  -data          = The data to be used to train / validate the models.
#  -colName       = The name of the solar irradiance column to predict.
#  -modelFunction = The "function" used to generate each model.
#  -checkAnn      = If Artificial Neural Network models should be tested.
#  -intervalSize  = The size of each interval in minutes.
#
# Returns:
#  -Nothing.  TODO: Return best cut size
testSolrCuts <- function(data, colName, modelFunction, checkAnn=F, intervalSize=1440) {
  testData <- data[!is.na(data[[colName]]) && data[[colName]] > 1,]
  targetCuts <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000)
  for(i in 1:length(targetCuts)) {
    numCuts <- targetCuts[i]
    writeLines(paste("Testing", numCuts,"cuts:"))
    writeLines("-------------------------------------------------------------------")
    testData["SOLR_D"] <- cut(testData[[colName]], numCuts)
    classModelFunction <- paste("SOLR_D ~", strsplit(modelFunction, "~")[[1]][2], sep="")
    nbResult <- tenFold(testData, makeNbSolr, intervalSize, 1, testClassModelSolr, "SOLR_D", classModelFunction, F)
    writeLines(paste("Average Error Rate for Naive Bayes:", (averageTestResults(nbResult, "error_rate") * 100), "%"))
    writeLines(paste("Average Error Margin for Naive Bayes:", (averageTestResults(nbResult, "error_margin"))))
    writeLines(paste("Average Error Std Dev for Naive Bayes:", (averageTestResults(nbResult, "error_sd"))))
    writeLines(paste("Average Error Percentage for Naive Bayes:", (averageTestResults(nbResult, "error_percent") * 100), "%"))
    if(checkAnn) {
      annResult <- tenFold(testData, makeAnnSolr, intervalSize, 1, testAnnModelSolr, "SOLR_D", classModelFunction, F)
      writeLines("")
      writeLines(paste("Average Error Rate for ANN:", (averageTestResults(annResult, "error_rate") * 100), "%"))
      writeLines(paste("Average Error Margin for ANN:", (averageTestResults(annResult, "error_margin"))))
      writeLines(paste("Average Error Std Dev for ANN:", (averageTestResults(annResult, "error_sd"))))
      writeLines(paste("Average Error Percentage for ANN:", (averageTestResults(annResult, "error_percent") * 100), "%"))
    }
    writeLines("-------------------------------------------------------------------")
  }
}

# Tests the effects of the interval size on the accuracy
# of the predictions.
#
# Parameters:
#  -data              = The data to be used to train / validate the models.
#  -colName           = The name of the solar irradiance column to predict.
#  -modelFunction     = The "function" used to generate each model.
#  -checkNb           = If Naive Bayes models should be tested.
#  -checkAnn          = If Artificial Neural Network models should be tested.
#  -checkRandomForest = If Random Forest models should be tested.
#  -numCuts           = The number of cuts / classes to be used by class-based models.
#  -mean              = The mean / average solar irradiance value of the data set.  Used to "deseasonalize" the data if it != 0.
#
# Returns:
#  -A list containing the best interval size for the absolute and relative error margins.
testSolrIntervals <- function(data, colName, modelFunction, checkNb=F, checkAnn=F, checkRandomForest=F, numCuts=1000, mean=0) {
  intervals <- c(1440, 720, 480, 360, 240, 180, 120, 60)
  bestIntervalMargin <- NULL
  bestIntervalRelative <- NULL
  for(i in 1:length(intervals)) {
    intervalSize <- intervals[i]
    writeLines(paste("Interval Size", intervalSize,":"))
    writeLines("-------------------------------------------------------------------")
    result = testSolr(data, intervalSize, colName, modelFunction, numCuts, checkNb = checkNb, checkAnn=checkAnn, checkRandomForest=checkRandomForest, mean=mean)
    if(is.null(bestIntervalMargin) || bestIntervalMargin$error_margin > result$best_margin$error_margin) {
      bestIntervalMargin <- list(interval=intervalSize, error_margin=result$best_margin$error_margin, error_percent=result$best_margin$error_percent, combo=result$best_margin$combo)
    }
    if(is.null(bestIntervalRelative) || bestIntervalRelative$error_percent > result$best_relative$error_percent) {
      bestIntervalRelative <- list(interval=intervalSize, error_margin=result$best_relative$error_margin, error_percent=result$best_relative$error_percent, combo=result$best_relative$combo)
    }
    writeLines("-------------------------------------------------------------------")
  }
  writeLines("Best Margin:")
  writeLines(paste("    Interval:", bestIntervalMargin$interval))
  writeLines(paste("    Error Margin:", bestIntervalMargin$error_margin))
  writeLines(paste("    Error Percent:", bestIntervalMargin$error_percent))
  writeLines(paste("    Models:", bestIntervalMargin$combo))
  writeLines("")
  writeLines("Best Relative:")
  writeLines(paste("    Interval:", bestIntervalRelative$interval))
  writeLines(paste("    Error Margin:", bestIntervalRelative$error_margin))
  writeLines(paste("    Error Percent:", bestIntervalRelative$error_percent))
  writeLines(paste("    Models:", bestIntervalRelative$combo))
  return(list(best_margin=bestIntervalMargin, best_relative=bestIntervalRelative))
}