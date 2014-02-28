#library("multicore")
library("e1071")
library("rpart")
library("nnet")
library('randomForest')

#source('Parallel.R')
source('Correlation.R')
source('Night_Day.R')
source('Solr.R')

writeRawTestResults <- function(name, results, pentad, hour) {
  if(!is.null(results[[as.character(pentad)]][[as.character(hour)]])) {
    sink(name)
    row <- "Time,Actual,Predicted,Error,Error Percent"
    writeLines(row)
    years <- names(results[[as.character(pentad)]][[as.character(hour)]])   
    num_years <- length(years)
    for(j in 1:num_years) {
      result <- results[[as.character(pentad)]][[as.character(hour)]][[years[j]]][["result"]]
      num_predictions <- length(result$predicted)
      for(k in 1:num_predictions) {
        error <- abs(result$predicted[k] - result$actual[k])
        writeLines(paste(result$time[k], result$actual[k], result$predicted[k], error, abs(error / result$actual[k]), sep=","))
      }
    }
    sink()
  }
}

writeTestResults <- function(name, results, pentad) {
  sink(name)
  row <- "Hour, Error Margin, Error Margin Std Dev, Error Percentage"
  writeLines(row)
  
  for(i in 0:23) {
    if(is.null(results[[as.character(pentad)]][[as.character(i)]])) {
      writeLines(paste(i, ",,,"))
    } else {
      years <- names(results[[as.character(pentad)]][[as.character(i)]])   
      num_years <- length(years)
      em <- 0
      stddev <- 0
      ep <- 0
      count <- 0
      for(j in 1:num_years) {
        result <- results[[as.character(pentad)]][[as.character(i)]][[years[j]]][["result"]]
        if(!is.null(result)) {
          count <- count + 1
          em <- em + result$error_margin
          ep <- ep + result$error_percent
          stddev <- stddev + result$error_sd
        }
      }
      if(count > 0) {
        em <- em / count
        ep <- ep / count
        stddev <- stddev / count
        writeLines(paste(i, em, stddev, ep, sep=","))
      } else {
        writeLines(paste(i, ",,,"))
      }
    }
  }
  sink()
}

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

# Loads the data from the given CSV file.
#
# Parameter:
#   -path = The path to the CSV file to be loaded.
#
# Returns:
#   -The contents of the CSV file as a data frame.
loadCsv <- function(path) {
  writeLines(paste("Reading: ", path))
  start <- proc.time()

  data <- read.csv(path);

  writeLines("Reading:")
  print((proc.time() - start))
  data[["DT"]] <- convertDate(data)
  data[["DT_NUM"]] <- as.numeric(data[["DT"]])
  data[["TIME"]] <- convertTime(data)
  data$TMZN <- NULL
  data$RELH_ERROR <- NULL
  data$RELH_REL_ERROR <- NULL
  data <- data[order(data$DT_NUM, decreasing=F),]
  return(data);
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
    } else {
      merged <- rbind(merged, read.csv(csvFiles[i]));
    }
  }
  writeLines("Reading:")
  print((proc.time() - start))
  merged[["DT"]] <- convertDate(merged)
  merged[["DT_NUM"]] <- as.numeric(merged[["DT"]])
  merged[["TIME"]] <- convertTime(merged)
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


# Removes columns with too many NA / missing values.
#
# Parameters:
#  -data   = The data frame to be filtered.
#  -cutoff = The cutoff percentage for the number of NAs that can exist before it is removed.
#            Ex.  A cutoff of 0.5 will remove a column with more than 50% NA values.
#
# Returns:
#  -The data frame with the columns that contain too many NA values removed.
filterColumns <- function(data, cutoff=0.5) {
  colNames <- names(data)
  for(i in 1:length(colNames)) {
    numNA <- 0
    numRows <- nrow(data)
    for(j in 1:numRows) {
      if(is.na(data[[colNames[i]]][j])) {
        numNA <- numNA + 1
        if(numNA / numRows > cutoff) {
          writeLines(paste("Removing:", colNames[i]))#, paste("(",numNA," NAs)",sep="")))
          data[[colNames[i]]] <- NULL
          break
        }
      }
    }
  }
  return(data)
}

# Normalizes the given column in the data frame by subtracting the mean.
#
# Parameters:
#  -data    = The data frame to be normalized.
#  -colName = The name of the column to be normalzied.
#  -mean    = The mean value of the column.
#
# Returns:
#  -The normalized data.
meanNormalize <- function(data, colName, mean) {
  normalized <- vector(mode="numeric", length=nrow(data))
  for(i in 1:nrow(data)) {
    normalized[i] <- data[[colName]][i] - mean
  }
  data[[paste(colName, "_NORM", sep="")]] <- normalized
  return(data)
}

# Converts the date and time fields of the given data frame into a single numeric
# value.
#
# Parameters:
#  -data = The data frame to convert the date of.
#
# Returns:
#   -A vector containing the converted date values.
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

# Converts the time columns of the given data frame into a single value.
#
# Parameters:
#  -data = The data frame to convert the time of.
#
# Returns:
#  -A vector containing the converted values.
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

# Determines whether each in the given data frame is during the day or night and
# adds a new column to the data frame containing this determination.  A row is
# during the night if it's SOLR value is less then a given threshold.
#
# Parameters:
#  -data      = The data frame to check.
#  -colName   = The name of the SOLR column that will be used to perform the
#               thresholding.
#  -threshold = The threshold that will be used to determine if a given row is during
#               the day or night.
#
# Returns:
#  -The modified data frame with a new ND column that contains whether the given row
#   is during the day or night.
convertNightDay <- function(data, colName, threshold) {
  nightDay <-logical(0)
  for(i in 1:nrow(data)) {
    if(length(nightDay) == 0) {
      nightDay <- !is.na(data[[colName]][i]) && data[[colName]][i] < threshold
    } else {
      nightDay <- c(nightDay, !is.na(data[[colName]][i]) && data[[colName]][i] < threshold)
    }
  }
  data[["ND"]] <- as.factor(nightDay)
  return(data)
}

# Merges the sourceFrame into the destFrame.
#
# Parameters:
#  -destFrame   = The frame that the other data frame should be merged into.
#  -sourceFrame = The frame to be merged.
#  -sourceName  = The name of the source / station of the sourceFrame.
#
# Returns:
#  -The merged data frames.
mergeDataFrames <- function(destFrame, sourceFrame, sourceName) {
  ignoreList <- c("MON", "YEAR", "DT", "DT_NUM", "TIME", "SINT", "PENTAD", "DAY", "HR", "MIN", "RELH_ERROR", "RELH_REL_ERROR")
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

offsetDay <- function(numdays, col, data) {
  newCol <- numeric(0)
  for(i in 1:nrow(data)) {
    writeLines(paste(i, "/", nrow(data)))
    offset <- data[data$DT == (data[["DT"]][i] - (3600 * 24 * numdays)), col]
    if(length(offset) == 0) {
      newCol <- c(newCol, NA)
    } else {
      newCol <- c(newCol, offset[1])
    }
  }
  data[[paste(col,"_DAY_", numdays, sep="")]] <- newCol
  return(data)
}

# Offsets a given column in the given data frame up the given number of rows and returns
# the offset column.
#
# NOTE:  It "offsets" it by grabbing the values past the offset and then padding the
#        columns with NAs.
#
# Parameters:
#  -numrows = The number of row to offset the column by.
#  -col     = The name of the column to offset.
#  -data    = The data frame containing the column to offset.
#
# Returns:
#  -The offset column.
offsetCol <- function(numrows, col, data) {
  tempData <- data[(numrows+1):nrow(data),]
  newCol <- tempData[order(tempData$DT_NUM, decreasing=F),col]
  for(i in 1:numrows) {
    newCol <- c(newCol, NA)
  }
  return(newCol)
}

# Offsets the given column of the given data frame by numrows rows to associate the column
# with the data from numrows previous rows.
#
# NOTE: Each row that was offset will have a suffix of _# where # is the number of times it
#       was offset.  Ex. If SOLR_4 is the highest suffix (i.e. the SOLR value of the row that we
#       are trying to associate the previous data to or the "result" row), SOLR_3 would be the 
#       SOLR value of row previous to our "result" row, SOLR_2 would be 2 rows previous, SOLR_1 would
#       be 3 rows previous, and SOLR would be 4 rows previous.
#
# Parameters:
#  -numrows = The number of rows previous to offset (i.e. the number of previous rows to associate with each
#             row).
#  -col     = The name of the column that will be associated with the previous data.
#  -data    = The data frame containing the data to be offset.
#
# Returns:
#  -The offset data frame with the given column associated with the given number of previous rows.
dataOffset <- function(numrows, col, data) {
  ignoreList <- c("YEAR", "MON", "DAY", "HR", "MIN", "DT", "DT_NUM", "TIME")
  colNames <- names(data)
  
  # Offset other columns numrows - 1 times
  for(i in 1:(numrows - 1)) {
    for(j in 1:length(colNames)) {
      if(!(colNames[j] %in% ignoreList)){
        data[[paste(colNames[j],"_",i,sep="")]] <- offsetCol(i, colNames[j], data)      
      }
    }
  }
  
  # Set the invariant columns (i.e. date / time)
  data[["YEAR"]] <- offsetCol(numrows, "YEAR", data)
  data[["MON"]] <- offsetCol(numrows, "MON", data)
  data[["DAY"]] <- offsetCol(numrows, "DAY", data)
  data[["HR"]] <- offsetCol(numrows, "HR", data)
  data[["MIN"]] <- offsetCol(numrows, "MIN", data)
  data[["DT"]] <- offsetCol(numrows, "DT", data)
  data[["DT_NUM"]] <- offsetCol(numrows, "DT_NUM", data)
  data[["TIME"]] <- offsetCol(numrows, "TIME", data)
  
  # Offset the specified column numrows times
  data[[paste(col,"_",numrows,sep="")]] <- offsetCol(numrows, col, data)
  if(col == "SOLR_FRAC") {
    data[[paste("SOLR_MAX_",numrows,sep="")]] <- offsetCol(numrows, "SOLR_MAX", data)
  }
  
  # Truncate and sort the data frame
  data <- data[1:(nrow(data)-numrows),]
  data <- data[order(data$DT_NUM, decreasing=F),]
  return(data)
}

# Converts the result from a tree model prediction to the same form as the other
# results.
#
# Parameters:
#  -result = The results of a predict call.
#
# Returns:
#  -The converted result set.
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
    } else {
      returnList <- c(returnList, colNames[prediction])
    }
  }
  return(returnList)
}

# Averages the specified column of the given results vector.
#
# Parameters:
#  -results = The vector containing the results.
#  -column  = The name of the column to average.
#
# Returns:
#  -The average of the specified column.
averageTestResults <- function(results, column) {
  total <- 0
  numResults <- length(results)
  for(i in 1:numResults) {
    total <- total + results[[i]][[column]][1]
  }
  return(total / numResults)
}

# Runs a single fold in the ten-fold validation for a single model type.
#
# Parameters:
#  -index     = The index of the function call in the list of scheduled function calls.
#  -paramList = The parameters for the function call.
#
# Returns:
#  -Returns a list containing the index of the call and results of the call.
tenFoldFunc <- function(index, paramList) {
  training <- paramList$training
  test <- paramList$test
  intervalSize <- paramList$intervalSize
  start <- paramList$start
  modelFormula <- paramList$modelFormula
  colName <- paramList$colName
  verbose <- paramList$verbose
  makeModelFun <- paramList$makeModelFun
  testFun <- paramList$testFun
  
  model <- makeModelFun(training, intervalSize, start, modelFormula)
  error <- testFun(model, test, colName, verbose)
  
  if(verbose) {
    writeLines("-----------------------------------------------------------------")
    writeLines(paste("Fold", index, ":", (error[["error_rate"]] * 100),"% Error Rate"))
  }
  
  return(list(index=index, error = error))
}

# Collects the results of the ten-fold validation.
#
# Parameters:
#  -statusList = The list of function statuses.
#
# Returns:
#  -The collected / combined results.
tenFoldResults <- function(statusList) {
  numResults <- length(statusList)
  results <- vector(mode="list", length=numResults)
  for(i in 1:numResults) {
    results[i] <- list(statusList[[i]]$result$error)
  }
  return(results)
}

# Runs ten-fold validation for a given model type.
#
# Parameters:
#  -data         = The data to be used for the training / validation.
#  -makeModelFun = The function used to create and train the model.
#  -intervalSize = The size of the interval that the model covers.
#  -start        = The start of the interval to be tested.
#  -testFun      = The function that will be used to run the test for each fold.
#  -colName      = The name of the column that the model will predict.
#  -modelFormula = The "formula" used to build the model.
#  -verbose      = If the function should print verbose output.
#
# Returns:
#  -The results of the ten-fold validation.
tenFold <- function(data, makeModelFun, intervalSize, start, testFun, colName, modelFormula, verbose=F) {
  if(nrow(data) < 10) {
    return(NULL)
  }
  paramLists <- vector(mode="list", length=10)
  folds <- split(data, sample(rep(1:10, nrow(data)/10)))
  overallError <- numeric(0)
  for(i in 1:10) {
    test <- folds[[i]]
    temp <- Filter(function(x){return(!(identical(x, test)))}, folds)
    training <- NULL
    for(j in 1:length(temp)) {
      if(j == 1) {
        training <- temp[[j]]
      } else {
        training <- rbind(training, temp[[j]])
      }
    }
    
    paramLists[i] <- list(list(makeModelFun = makeModelFun, testFun = testFun, test = test, training = training, intervalSize = intervalSize, start = start, colName = colName, modelFormula = modelFormula, verbose = verbose))   
  }
  overallError <- runParallel(paramList=paramLists, parallelFunction=tenFoldFunc, collectFunction=tenFoldResults)
  
  if(verbose) {
    writeLines(paste("Average Error Rate:", (averageTestResults(overallError, "error_rate") * 100), "%"))
  }
  return(overallError)
}

# Creates a string describing the combination of models in the given list.
#
# Parameters:
#  -comboList = A list containing the models that yeilded the best results for each interval.
#
# Returns:
#  -The string representation of the models.
getCombo <- function(comboList) {
  numCombo <- length(comboList)
  comboString <- comboList[1]
  if(numCombo > 1) {
    for(i in 2:numCombo) {
      comboString <- paste(comboString, "-", comboList[i])
    }
  }
  return(comboString)
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

# Creates a model function for predicting the given column using all of the other columns.
#
# Parameters:
#  -data       = The data frame containing the data that can be used to perform the prediction.
#  -colName    = The name of the column to predict.
#  -ignoreList = The names of the columns to be ignored.
#
# Returns:
#  -The model function string.
makeFullModelFunction <- function(data, colName, ignoreList=vector()) {
  functionString <- paste(colName, "~")
  first <- T
  for(i in names(data)) {
    if(i != colName && i != "DT" && !(i %in% ignoreList)) {
      if(first) {
        functionString <- paste(functionString, i)
        first <- F
      } else {
        functionString <- paste(functionString, "+", i)
      }
    }
  }
  return(functionString)
}

# Creates a model function using the top attributes.
#
# Parameters:
#  -data               = The data frame containing the data that can be used to perform the prediction.
#  -colName            = The name of the column to predict.
#  -topThreshold       = The number of attributes to select.
#  -selectedMethod     = The correlation coefficient that will be used to rank the attributes.
#  -ignoreList         = The names of the columns to be ignored.
#  -correlationResults = A list of pre-computed correlation coefficients.
#
# Returns:
#  -The model function string.
makeTopFunction <- function(data, colName, topThreshold, selectedMethod="pearson", ignoreList=vector(), correlationResults=NULL) {
  first <- T
  results <- goodCorrelateTop(data=data, col=colName, topThreshold=topThreshold, selectedMethod=selectedMethod, results=correlationResults)[["names"]]
  functionString <- paste("`", colName, "`", " ~ ", sep="")
  for(i in results) {
    if(i != colName && i != "DT" && !(i %in% ignoreList)) {
      if(first) {
        functionString <- paste(" ", functionString, "`", as.character(i), "`", sep="")
        first <- F
      } else {
        functionString <- paste(functionString, " + ", "`", as.character(i), "`", sep="")
      }
    }
  }
  return(functionString)
}

# Creates a model function using all attributes that have a correlation coefficient above a given threshold.
#
# Parameters:
#  -data               = The data frame containing the data that can be used to perform the prediction.
#  -colName            = The name of the column to predict.
#  -threshold          = The lowest correlation coefficient that will be kept.
#  -selectedMethod     = The correlation coefficient that will be used to rank the attributes.
#  -ignoreList         = The names of the columns to be ignored.
#  -correlationResults = A list of pre-computed correlation coefficients.
#
# Returns:
#  -The model function string.
makeThreshFunction <- function(data, colName, threshold, selectedMethod="pearson", ignoreList=vector(), correlationResults=NULL) {
  first <- T
  results <- goodCorrelateThresh(data=data, col=colName, threshold=threshold, selectedMethod=selectedMethod, results=correlationResults)[["names"]]
  functionString <- paste(colName, "~")
  for(i in results) {
    if(i != colName && i != "DT" && !(i %in% ignoreList)) {
      if(first) {
        functionString <- paste(functionString, as.character(i))
        first <- F
      } else {
        functionString <- paste(functionString, "+", as.character(i))
      }
    }
  }
  return(functionString)
}

# Generates the models for Night / Day prediction.
#
# Parameters:
#  -comboString = The string describing the best model combination.
#  -colName     = The name of the column to predict.
#  -data        = The data used to train the models.
#  -ndThreshold = The threshold between Night / Day (i.e. the minimum Day solar irradiance value.)
#
# Returns:
#  -The Night / Day models.
generateNDModels <- function(comboString, colName, data, ndThreshold=1) {
  modelFormula <- "ND ~ TIME + MON"
  models <- vector()
  modelStrings <- strsplit(comboString, "-")[[1]]
  start <- 1
  intervalSize <- 12 / length(modelStrings)
  end <- intervalSize
  data <- convertNightDay(data=data, colName=colName, threshold=ndThreshold)
  for(i in 1:length(modelStrings)) {
    string <- gsub("(^ +)|( +$)", "", modelStrings[i])

    if(string == "NULL") {
      models <- c(models, list(list(start=start, end=end, model=NULL)))
    } else if (string == "Naive Bayes") {
      models <- c(models, list(list(start=start, end=end, model=naiveBayes(formula=as.formula(modelFormula), data=data[data$MON >= start & data$MON <= end,], na.action=na.omit), type="Naive Bayes")))
    } else if(string == "Decision Tree") {
      models <- c(models, list(list(start=start, end=end, model=rpart(formula=as.formula(modelFormula), data=data[data$MON >= start & data$MON <= end,], na.action=na.omit), type="Decision Tree")))
    } else if(string == "ANN") {
      models <- c(models, list(list(start=start, end=end, model=nnet(formula=as.formula(modelFormula), data=data[data$MON >= start & data$MON <= end,], na.action=na.omit, size=1, trace=FALSE), type="ANN")))
    }
    start  = start + intervalSize
    end = end + intervalSize
  }
  return(models)
}

# Generates the models for solar irradiance prediction.
#
# Parameters:
#  -comboString  = The string describing the best model combination.
#  -colName      = The name of the column to predict.
#  -data         = The data used to train the models.
#  -modelFormula = The model formula used to generate the models.
#  -numCuts      = The number of cuts / classes to use for class-based models.
#
# Returns:
#  -The solar irradiance models.
generateSolrModels <- function(comboString, colName, data, modelFormula, numCuts=1000) {
  data <- data[!is.na(data[[colName]]) & data[[colName]] > 1,]
  models <- vector()
  modelStrings <- strsplit(comboString, "-")[[1]]
  start <- 1
  intervalSize <- 1440 / length(modelStrings)
  end <- intervalSize
  data["SOLR_D"] <- cut(data[[colName]], numCuts)
  classModelFunction <- paste("SOLR_D ~", strsplit(modelFormula, "~")[[1]][2], sep="")
  
  for(i in 1:length(modelStrings)) {
    string <- gsub("(^ +)|( +$)", "", modelStrings[i])
    
    if(string == "NULL") {
      models <- c(models, list(list(start=start, end=end, model=NULL, type="NULL")))
    } else if(string == "Linear Regression") {
      models <- c(models, list(list(start=start, end=end, model=lm(formula=as.formula(modelFormula), data=data[data$TIME >= start & data$TIME <= end,], na.action=na.omit), type="Linear Regression")))
    } else if (string == "Naive Bayes") {
      models <- c(models, list(list(start=start, end=end, model=naiveBayes(formula=as.formula(classModelFunction), data=data[data$TIME >= start & data$TIME <= end,], na.action=na.omit), type="Naive Bayes")))
    } else if(string == "Decision Tree") {
      models <- c(models, list(list(start=start, end=end, model=rpart(formula=as.formula(modelFormula), data=data[data$TIME >= start & data$TIME <= end,], na.action=na.omit, method="anova"), type="Decision Tree")))
    } else if(string == "ANN") {
      models <- c(models, list(list(start=start, end=end, model=nnet(formula=as.formula(classModelFunction), data=data[data$TIME >= start & data$TIME <= end,], na.action=na.omit, size=1, trace=FALSE, MaxNWts=5000), type="ANN")))
    } else if(string == "Random Forest") {
      models <- c(models, list(list(start=start, end=end, model=randomForest(formula=as.formula(modelFormula), data=data[data$TIME >= start & data$TIME <= end,], na.action=na.omit), type="Random Forest")))
    }
    start  = start + intervalSize
    end = end + intervalSize
  }
  return(models)
}

# Creates the overall model (Night / Day and SOLR).
#
# Parameters:
#  -target         = The station that the model would predict values for.
#  -neighbors      = The other stations to pull data from.
#  -offset         = The number of rows to offset (the number of previous rows to consider).
#  -numFeatures    = The number of features to use in the prediction.
#  -colName        = The name of the column to predict for in the original data frame.
#  -selectedMethod = The name of the correlation coefficient method that will be used to rank the features.
#
# Returns:
#  -The model.
createModel <- function(target, neighbors, offset, numFeatures, colName="SOLR", selectedMethod="pearson") {
  targetData <- filterColumns(mergeCsv(target))
  neighborData <- list()
  for(i in 1:length(neighbors)) {
    neighborData[[i]] <- filterColumns(mergeCsv(neighbors[i]))
  }
  merged <- mergeDataFrames(destFrame=targetData, sourceFrame=neighborData[[1]], sourceName=neighbors[1])
  if(length(neighbors) > 1) {
    for(i in 2:length(neighbors)){
      merged <- mergeDataFrames(destFrame=merged, sourceFrame=neighborData[[i]], sourceName=neighbors[i])
    }
  }
  rm(neighborData)
  
  solrColName <- paste("SOLR_", offset, sep="")
  
  merged <- dataOffset(data=merged, col="SOLR", numrows=offset)
  
  bestNightDay <- testNightDayIntervals(data=merged, solrColName=solrColName, checkAnn=T)
  
  modelFun <- makeTopFunction(data=merged, colName=solrColName, topThreshold=numFeatures, selectedMethod=selectedMethod)
  bestSolr <- testSolrIntervals(data=merged, colName=solrColName, modelFunction=modelFun, numCuts=1000)
  
  return(createModelHelper(ndCombo=bestNightDay$combo, solrCombo=bestSolr$best_margin$combo, data=merged, colName=solrColName, modelFun=modelFun))
  
}

# Creates the overall model (Night / Day and SOLR).
#
# Parameters:
#  -data           = The merged data frame to be used in training the model.
#  -colName        = The name of the column to predict for in the original data frame.
#  -modelFun       = The model function that will be used to generate the models.
#  -numFeatures    = The number of features to use in the prediction.
#  -selectedMethod = The name of the correlation coefficient method that will be used to rank the features.
#
# Returns:
#  -The model.
createModelNoLoad <- function(data, colName, modelFun=NULL, numFeatures=10, selectedMethod="pearson") {
  bestNightDay <- testNightDayIntervals(data=data, solrColName=colName, checkAnn=T)
  if(is.null(modelFun)) {
    modelFun <- makeTopFunction(data=data, colName=colName, topThreshold=numFeatures, selectedMethod=selectedMethod)
  }
  bestSolr <- testSolrIntervals(data=data, colName=colName, modelFunction=modelFun, numCuts=1000)
  
  return(createModelHelper(ndCombo=bestNightDay$combo, solrCombo=bestSolr$best_margin$combo, data=data, colName=colName, modelFun=modelFun))
}

# Creates and formats the model.
#
# Parameters:
#  -ndCombo   = The string describing the best model combination for the Night / Day prediction.
#  -solrCombo = The string describing the best model combination for the solar irradiance prediction.
#  -data      = The training data.
#  -colName   = The name of the column to predict.
#  -modelFun  = The model function used to create the solar irradiance models.
#
# Returns:
#  -The created model.
createModelHelper <- function(ndCombo, solrCombo, data, colName, modelFun) {
  return(list(ndModel=generateNDModels(comboString=ndCombo, data=data, colName=colName, ndThreshold=2), solrModel=generateSolrModels(comboString=solrCombo, colName=colName, data=data, modelFormula=modelFun, numCuts=1000)))
}

# Predicts the next solar irradiance value for each of the rows in the given data set.
#
# Parameters:
#  -model = The model to be used in the prediction.
#  -data  = The data set containing the data that will be used for the prediction.
#
# Returns:
#  -The predicted values.
predictSolr <- function(model, data) {
  predictions <- vector(mode="numeric", length=nrow(data))
  for(i in 1:nrow(data)) {
    night <- predictNightDay(model, data[i,])
    if(night) {
      predictions[i] <- 0
    } else {
      predictions[i] <- predictSolrHelper(model, data[i,])
    }
  }
  return(predictions)
}

# Predicts the next Night / Day value for each row in the given data set.
#
# Parameters:
#  -model = The model to be used in the prediction.
#  -data  = The data set containing the data that will be used for the prediction.
#
# Returns:
#  -The predicted values.
predictNightDay <- function(model, data) {
  month <- data$MON 
  
  # Implement better (binary?) search later
  for(i in 1:length(model$ndModel)) {
    if(month >= model$ndModel[[i]]$start && month <= model$ndModel[[i]]$end) {
      return(as.logical(predict(model$ndModel[[i]]$model, data, type="class")))
    }
  }
}

# Runs the second layer of the solar irradiance prediction for a row of data.
#
# Parameters:
#  -model = The model to be used in the prediction.
#  -data  = The data set containing the data that will be used for the prediction.
#
# Returns:
#  -The predicted value.
predictSolrHelper <- function(model, data) {
  time <- data$TIME[1]

  for(i in 1:length(model$solrModel)) {
    if(time >= model$solrModel[[i]]$start && time <= model$solrModel[[i]]$end) {
      type <- model$solrModel[[i]]$type
      if(type == "NULL") {
        return(0)
      } else if(type == "Linear Regression" || type == "Decision Tree" || type == "Random Forest") {
        return(as.numeric(predict(model$solrModel[[i]]$model, data)[1]))
      } else {
        return(averageFactor(predict(model$solrModel[[i]]$model, data, type="class")[1]))
      }
    }
  }
}

# Tests the solar irradiance predictions.
#
# Parameters:
#  -data     = The data to be used in the test (for training and validation).
#  -colName  = The name of the column to predict.
#  -modelFun = The model function used to generate the models.
#
# Returns:
#  -Nothing.
testPredictSolr <- function(data, colName, modelFun) {
  cutoff <- ceiling(nrow(data) * 0.75)
  
  training <- data[1:cutoff,]
  test <- data[(cutoff+1):(nrow(data)),]
  model <- createModelNoLoad(data=training, colName=colName, modelFun=modelFun)
  results <- predictSolr(model=model, data=test)
  
  errorMargin <- 0
  actualSum <- 0
  errors <- 0
  for(i in 1:nrow(test)) {
    predicted <- results[i] 
    actual <- test[[colName]][i]

    if(!is.na(actual) && !is.na(predicted) && predicted != actual) {
      errorMargin <- errorMargin + abs(predicted - actual)
      actualSum <- actualSum + actual
      errors <- errors + 1
    }
  }
  writeLines(paste("Error Margin:", errorMargin / errors))
  writeLines(paste("Error Percent:", (errorMargin / actualSum * 100), "%"))
}

