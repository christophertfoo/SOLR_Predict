source('SOLR_Predict.R')

load("Data.RData")
solr_col <- "SOLR"

hours <- as.character(7:20)

wrf <- loadCsv("WRF.csv")
wrf <- wrf[which(wrf[["YEAR"]] == 2013),]

all_errors <- numeric(0)
errors <- list()

for(i in 1:nrow(wrf)) {
  matches <- merged[which(merged$DT_NUM == wrf[i, "DT_NUM"]),]
  if(nrow(matches) > 0) {
    hr <- as.character(wrf[i, "HR"])
    if(is.null(errors[[hr]])) {
      errors[[hr]] <- numeric(0)
    }
    writeLines(paste("WRF: ", wrf[i, "SOLR"], " -> Actual: ", matches[1, solr_col], sep=""))
    error <- abs(wrf[i, "SOLR"] - matches[1, solr_col])
    errors[[hr]] <- c(errors[[hr]], error)
    if(hr %in% hours) {
      all_errors <- c(all_errors, error)
    }
  }
}

sink("WRF_Results.csv")
writeLines("Hour,Average Absolute Error (W/m^2)")
for(hr in sort(names(errors))) {
  writeLines(paste(hr, mean(errors[[hr]]), sep=","))
}
writeLines(paste("Average", mean(all_errors), sep=","))
sink()