if(file.exists("Data.RData")) {
  writeLines("Loading Data.RData")
  load("Data.RData")
} else {
  source("Load_Data.R")
}

windward <- list(AS839, KFWH1, KTAH1, MOKH1, OFRH1, PHNG)
leeward <- list(D3665, MKRH1, PHJR, PLHH1, WNVH1)
marine_corp <- list(PHNG)
honolulu_airport <- list(PHNL)
scbh1 <- list(SCBH1)

direction_counts <- function(data, colName="DRCT") {
  counts <- list()
  for(frame in data) {
    if(colName %in% names(frame)) {
      directions <- frame[[colName]]
      for(i in 1:nrow(frame)) {
        direction <- directions[i] - 90
        if(direction < 0) {
          direction <- direction + 360
        }
        if(direction == 360) {
          direction <- 0
        }
        direction <- round(direction)
        if(is.null(counts[[as.character(direction)]])) {
          counts[[as.character(direction)]] <- 1
        } else {
          counts[[as.character(direction)]] <- counts[[as.character(direction)]] + 1
        }
      }
    }
  }
  return(counts)
}

write_counts <- function(counts, destination) {
  sink(destination)
  for(i in 0:359) {
    if(is.null(counts[[as.character(i)]])) {
      writeLines(paste(i,0,sep=","))
    } else {
      writeLines(paste(i,counts[[as.character(i)]],sep=","))
    }
  }
  sink()
}

write_binned_counts <- function(counts, destination) {
  sink(destination)
  for(i in 0:35) {
    min <- (i * 10) - 4
    max <- (i * 10) + 5
    sum <- 0
    for(j in min:max) {
      if(j < 0) {
        j <- j + 360
      }
      if(!is.null(counts[[as.character(j)]])) {
        sum <- sum + counts[[as.character(j)]]
      }
    }
    
    writeLines(paste(i,sum,sep=","))
  }
  sink()
}

windward_counts <- direction_counts(data=windward)
write_counts(windward_counts, "windward_counts.csv")
write_binned_counts(windward_counts, "windward_binned_counts.csv")

leeward_counts <- direction_counts(data=leeward)
write_counts(leeward_counts, "leeward_counts.csv")
write_binned_counts(leeward_counts, "leeward_binned_counts.csv")

marine_corp_counts <- direction_counts(data=marine_corp)
write_counts(marine_corp_counts, "marine_corp_counts.csv")
write_binned_counts(marine_corp_counts, "marine_corp_binned_counts.csv")

honolulu_airport_counts <- direction_counts(data=honolulu_airport)
write_counts(honolulu_airport_counts, "honolulu_airport_counts.csv")
write_binned_counts(honolulu_airport_counts, "honolulu_airport_binned_counts.csv")

scbh1_counts <- direction_counts(data=scbh1)
write_counts(scbh1_counts, "scbh1_counts.csv")
write_binned_counts(scbh1_counts, "scbh1_binned_counts.csv")