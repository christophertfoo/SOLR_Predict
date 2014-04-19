if(!exists("merged")) {
  if(file.exists("Data.RData")) {
    writeLines("Loading Data.RData")
    load("Data.RData")
    source('SOLR_Predict.R')
  } else {
    source("Load_Data.R")
  }
}

# 1. Hash of days -> index in vector
#
#    Vectors -> 0-23 w/ solr & day label
#

day_map <- new.env()

day <- character()

# 2. For each row
#      If day exists
#         Plug SOLR into correct hour for day
#      Else
#         Add new row w/ day label, plug in SOLR for found hour, add NA for all others
#

num_rows <- nrow(merged)

ignore_list <- c("YEAR", "MON", "DAY", "HR", "TMZN")

columns <- setdiff(names(merged), ignore_list)
vector_names <- character()

for(i in 1:num_rows) {
  writeLines(paste(i,"/",num_rows))
  row <- merged[i,]
  date <- sprintf('%04d-%02d-%02d', row$YEAR, row$MON, row$DAY)
  if(is.null(day_map[[date]])) {
    day <- c(day, date)
    day_map[[date]] <- length(day)
    for(hour in 0:23) {
      for(col in columns) {
        vector_name <- paste(col,hour,sep="_")
        if(!exists(vector_name)) {
          vector_names <- c(vector_names, vector_name)
          assign(vector_name, numeric())
        }
        if(hour == row$HR) {
          if(col == "DT") {
            assign(vector_name, c(get(vector_name), list(row[[col]])))     
          } else {
            assign(vector_name, c(get(vector_name), row[[col]]))     
          }   
        } else {
          assign(vector_name, c(get(vector_name), NA))
        }
      }
    }
  } else {
    for(col in columns) {
      vector_name <- paste(col,row$HR,sep="_")
      temp <- get(vector_name)
      if(col == "DT") {
        temp[day_map[[date]]] <- list(row[[col]])
      } else {
        temp[day_map[[date]]] <- row[[col]]
      }
      assign(vector_name, temp)
    }
  }
}
# 3. Make data frame and return
vectorized <- data.frame(day=day)
for(vector in vector_names) {
  vectorized[[vector]] <- get(vector)
}

save(vectorized, file="Vectorized.RData")