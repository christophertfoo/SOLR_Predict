# Tests a decision tree model for predicting if the next data point would be
# day or night.
#
# Parameters:
#  -model   = The decision tree model used to perform the prediction.
#  -data    = The data frame containing the data used to test the prediction.
#  -colName = The name of the Night / Day column in the data frame.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the test results.
testTreeModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- convertTreeResult(predict(model, data))
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

# Tests a Naive Bayes model for predicting if the next data point would be
# day or night.
#
# Parameters:
#  -model   = The Naive Bayes model used to perform the prediction.
#  -data    = The data frame containing the data used to test the prediction.
#  -colName = The name of the Night / Day column in the data frame.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the test results.
testNbModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- predict(model, data)
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

# Tests an artificial neural network model for predicting if the next data point would be
# day or night.
#
# Parameters:
#  -model   = The neural network model used to perform the prediction.
#  -data    = The data frame containing the data used to test the prediction.
#  -colName = The name of the Night / Day column in the data frame.
#  -verbose = If the function should print verbose output.
#
# Returns:
#  -A list containing the test results.
testAnnModelNightDay <- function(model, data, colName, verbose=F) {
  predicted <- predict(model, data, type="class")
  return(testModelNightDayHelper(model, predicted, data, colName, verbose))
}

# A helper that determines the accurracy of the predicted Night / Day values.
#
# Parameters:
#  -model     = The model that was tested (unused?).
#  -predicted = The Night / Day values that were predicted by the model.
#  -data      = The data used to generate the predictions.  It also contains the actual Night / Day values.
#  -colName   = The name of the Night / Day column.
#  -verbose   = If the function should print verbose output.
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
      } else {
        falsePositive <- falsePositive + 1
      }
    }
  }
  if(verbose) {
    print(paste("Errors:", errors, "/", numRows))
  }
  return(list(error_rate=(errors / numRows), false_positive=(falsePositive / errors), false_negative=(falseNegative / errors)))
}

# Creates a Decision Tree model for predicting whether the next data point would be during the day or night.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in months).
#  -start        = The starting month of the interval.
#  -modelFormula = The "formula" used to make the model in the form ND ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeTreeNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
    training <- data
  } else {
    training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(rpart(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

# Creates a Naive Bayes moel for predicting whether the next data point would be during the day or night.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in months).
#  -start        = The starting month of the interval.
#  -modelFormula = The "formula" used to make the model in the form ND ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeNbNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
    training <- data
  } else {
    training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(naiveBayes(formula=as.formula(modelFormula), data=training, na.action=na.omit))
}

# Creates an Artificial neural Network model for predicting whether the next data point would be during the day or night.
#
# Parameters:
#  -data         = The training data.
#  -intervalSize = The size of each interval (in months).
#  -start        = The starting month of the interval.
#  -modelFormula = The "formula" used to make the model in the form ND ~ Feature1 + Feature2 + ... which selects which features will be used.
#
# Returns:
#  -The resulting model.
makeAnnNightDay <- function(data, intervalSize, start, modelFormula="ND ~ TIME + MON") {
  if(intervalSize == 0) {
    training <- data
  } else {
    training <- data[data$MON %in% start:(start + intervalSize - 1),]
  }
  return(nnet(formula=as.formula(modelFormula), data=training, na.action=na.omit, size=1, trace=FALSE))
}

# Tests the models for the Night / Day prediction and finds the best combination of models.
#
# Parameters:
#  -data         = The data to be used for training / validation.
#  -intervalSize = The size of each interval.
#  -checkAnn     = If Artificial Neural Network models should be tested.
#  -verbose      = If verbose output should be printed.
#
# Returns:
#  -A list containing the best error rate (i.e. lowest) and the combination of models that
#   generate that error rate.
testNightDay <- function(data, intervalSize, checkAnn=F, verbose=F) {
  nbResults <- vector()
  dtResults <- vector()
  annResults <- vector()
  bestResults <- vector()
  bestCombo <- character(0)
  
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
    bestModel <- "Naive Bayes"
    
    if(verbose) {
      writeLines(paste("Interval Error Rate for Naive Bayes:", (averageTestResults(intervalResult, "error_rate") * 100), "%"))
      writeLines(paste("Interval False Positive % for Naive Bayes:", (averageTestResults(intervalResult, "false_positive") * 100), "%"))
      writeLines(paste("Interval False Negative % for Naive Bayes:", (averageTestResults(intervalResult, "false_negative") * 100), "%"))
    }
    
    intervalResult <- tenFold(subset(data, MON %in% start:end), makeTreeNightDay, intervalSize, start, testTreeModelNightDay, "ND", "ND ~ TIME + MON", F)
    dtResults <- c(dtResults, intervalResult)
    
    if(averageTestResults(intervalResult, "error_rate") < averageTestResults(bestResult, "error_rate")) {
      bestResult <- intervalResult
      bestModel <- "Decision Tree"
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
        bestModel <- "ANN"
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
    bestCombo <- c(bestCombo, bestModel)
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
  bestErrorRate <- (averageTestResults(bestResults, "error_rate") * 100)
  writeLines("")
  writeLines(paste("Average Error Rate for Best:", bestErrorRate, "%"))
  writeLines(paste("Average False Positive % for Best:", (averageTestResults(bestResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Best:", (averageTestResults(bestResults, "false_negative") * 100), "%"))
  bestComboString = getCombo(bestCombo)
  writeLines(paste("Best Combo:", bestComboString))
  return(list(error_rate=bestErrorRate, combo=bestComboString))
}

# Tests the models for Night / Day prediction using seasonal intervals and finds the best
# combination of models.
#
# Parameters:
#  -data     = The data used for training / validation.
#  -checkAnn = If Artificial Neural Network models should be tested.
#  -verbose  = If verbose output should be printed.
#
# Returns:
#  -A list containing the best error rate (i.e. lowest) and the combination of models that
#   generate that error rate.  TODO FIX THIS, it only returns the error rate now.
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
  bestErrorRate <- averageTestResults(bestResults, "error_rate") * 100
  writeLines(paste("Average Error Rate for Best:", bestErrorRate, "%"))
  writeLines(paste("Average False Positive % for Best:", (averageTestResults(bestResults, "false_positive") * 100), "%"))
  writeLines(paste("Average False Negative % for Best:", (averageTestResults(bestResults, "false_negative") * 100), "%"))
  return(bestErrorRate)
}

# Tests the effects of the different thresholds on the accuracy of the Night / Day prediction.
#
# Parameters:
#  -data         = The data to be used for training / validating the models.
#  -solrColName  = The name of the column that contains the current solar irradiance value for each row.
#  -maxThreshold = The largest solar irradiance threshold that should be tested.
#  -verbose      = If verbose output should be printed.
#  -checkAnn     = If Artificial Neural Network models should be trained and tested.
#
# Returns:
#  -The best Night / Day threshold.
testNightDayThreshold <- function(data, solrColName, maxThreshold, verbose=F, checkAnn=F) {
  bestThreshold <- NULL
  for(i in 1:maxThreshold) {
    writeLines(paste("Threshold", i, ":"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
    testData <- convertNightDay(data=data, colName=solrColName, threshold=i)
    result <- testNightDay(data=testData, intervalSize=12, verbose=verbose, checkAnn=checkAnn)
    if(is.null(bestThreshold) || result$error_rate < bestThreshold$error_rate) {
      bestThreshold <- list(threshold=i, error_rate=result$error_rate, combo=result$combo)
    }
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  writeLines(paste("Best Threshold:", bestThreshold$threshold, "(", bestThreshold$error_rate, "% )"))
  writeLines(paste("Models:", bestThreshold$combo))
  return(bestThreshold)
}

# Tests the effects of the different intervals on the accuracy of the Night / Day prediction.
#
# Parameters:
#  -data         = The data to be used for training / validating the models.
#  -solrColName  = The name of the column that contains the current solar irradiance value for each row.
#  -verbose      = If verbose output should be printed.
#  -checkAnn     = If Artificial Neural Network models should be trained and tested.
#
# Returns:
#  -The best Night / Day interval size (in months).
testNightDayIntervals <- function(data, solrColName, verbose=F, checkAnn=F) {
  intervals <- c(12, 6, 4, 3, 2, 1)
  testData <- convertNightDay(data=data, colName=solrColName, threshold=1)
  bestIntervalSize <- NULL
  for(i in 1:length(intervals)) {
    intervalSize <- intervals[i]
    writeLines(paste("Interval Size", intervalSize, ":"))
    writeLines("-----------------------------------------------------------------------------------------------------------------")
    result <- testNightDay(data=testData, intervalSize=intervals[i], verbose=verbose, checkAnn=checkAnn)
    if(is.null(bestIntervalSize) || result$error_rate < bestIntervalSize$error_rate) {
      bestIntervalSize <- list(interval=intervals[i], error_rate=result$error_rate, combo=result$combo)
    }
    writeLines("-----------------------------------------------------------------------------------------------------------------")
  }
  writeLines(paste("Best Interval Size:", bestIntervalSize$interval, "(", bestIntervalSize$error_rate, "% )"))
  writeLines(paste("Models:", bestIntervalSize$combo))
  #   writeLines("Interval Season:")
  #   writeLines("-----------------------------------------------------------------------------------------------------------------")
  #   testNightDaySeason(data=testData, checkAnn=checkAnn, verbose=verbose)
  #   writeLines("-----------------------------------------------------------------------------------------------------------------")
  return(bestIntervalSize)
}
