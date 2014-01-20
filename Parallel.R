numCores <- multicore:::detectCores()

# Find the number of function calls that have been started.
#
# Parameters:
#  -statusList = The list of statuses for the scheduled function call.
#
# Returns:
#  -The number of function calls that were started (some may have completed).
getNumStarted <- function(statusList) {
  count <- 0
  for(i in 1:length(statusList)) {
    if(statusList[[i]]$started) {
      count <- count + 1
    }
  }
  return(count)
}

# Find the number of function calls that have returned.
#
# Parameters:
#  -statusList = The list of statuses for the scheduled function call.
#
# Returns:
#  -The number of function calls that returned (i.e. finished).
getNumReturned <- function(statusList) {
  count <- 0
  for(i in 1:length(statusList)) {
    if(statusList[[i]]$returned) {
      count <- count + 1
    }
  }
  return(count)
}

# Runs multiple calls of a given function in parallel (multi-processed).
#
# NOTE: The function must take two parameters: index (index of the function call) and paramList (a list of parameters that the function call can use)
#
# Parameters:
#  -paramListVector  = A vector in list mode that contains the paramLists that will be passed to each function call.
#  -parallelFunction = The function that will be called.
#  -collectFunction  = The function that is used to collect / combine the results of the parallel function calls.
#  -debug            = If it should be run in debug mode.
#
# Returns:
#  -The results of each function call combined by the collectFunction.
runParallel <- function(paramListVector, parallelFunction, collectFunction, debug=F) {
  maxConcurrent <- length(paramListVector)
  if(maxConcurrent > numCores) {
    maxConcurrent <- numCores
  }
  
  numJobs <- length(paramListVector)
  
  statusList <- vector(mode="list", length=numJobs)
  
  for(i in 1:numJobs) {
    statusList[i]  <- list(list(started=F,returned=F, result=NULL, paramList=paramListVector[[i]]))
  }
  
  nextIndex <- 1
  for(i in 1:maxConcurrent) {
    if(debug) {
      writeLines(paste("Starting", nextIndex))
    }
    parallel(expr=parallelFunction(index=nextIndex, paramList=statusList[[nextIndex]]$paramList))
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
        if(nextIndex <= numJobs) {
          if(debug) {
            writeLines(paste("Starting", as.character(nextIndex)))
          }
          parallel(expr=parallelFunction(index=nextIndex, paramList=statusList[[nextIndex]]$paramList))
          statusList[[nextIndex]]$started <<- T
          nextIndex <<- nextIndex + 1
        }
      }
    }
  } # End launchNew
  
  while(getNumReturned(statusList) < length(statusList)) {
    collect(intermediate=launchNew)
  }
  return(collectFunction(statusList))
}