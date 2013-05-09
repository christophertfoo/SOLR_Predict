source('SOLR_Predict.R')

currentWd <- getwd()

# Load Schofield Data
setwd("Schofield_Data")
SCBH1 <- filterColumns(mergeCsv("SCBH1"))
PHHI <- filterColumns(mergeCsv("PHHI"))

# Ignore SCEH1 for now due to missing data
# SCEH1 <- filterColumns(mergeCsv("SCEH1"))
SCSH1 <- filterColumns(mergeCsv("SCSH1"))

SCBH1_Base <- mergeDataFrames(SCBH1, PHHI, "PHHI")
#SCBH1_Base <- mergeDataFrames(SCBH1_Base, SCEH1, "SCEH1")
SCBH1_Base <- mergeDataFrames(SCBH1_Base, SCSH1, "SCSH1")
SCBH1_Base <- dataOffset(4, "SOLR", SCBH1_Base)

# SCEH1_Base <- mergeDataFrames(SCEH1, PHHI, "PHHI")
# SCEH1_Base <- mergeDataFrames(SCEH1_Base, SCBH1, "SCBH1")
# SCEH1_Base <- mergeDataFrames(SCEH1_Base, SCSH1, "SCSH1")
# SCEH1_Base <- dataOffset(4, "SOLR", SCEH1_Base)

SCSH1_Base <- mergeDataFrames(SCSH1, PHHI, "PHHI")
#SCSH1_Base <- mergeDataFrames(SCSH1_Base, SCEH1, "SCEH1")
SCSH1_Base <- mergeDataFrames(SCSH1_Base, SCBH1, "SCBH1")
SCSH1_Base <- dataOffset(4, "SOLR", SCSH1_Base)

rm(PHHI)
gc()

# Load Palehua Data
setwd("..")
setwd("Palehua_Data")
Data_51204 <- filterColumns(mergeCsv("51204"))
D3665 <- filterColumns(mergeCsv("D3665"))
HOFH1 <- filterColumns(mergeCsv("HOFH1"))
PHJR <- filterColumns(mergeCsv("PHJR"))
PLHH1 <- filterColumns(mergeCsv("PLHH1"))
WWFH1 <- filterColumns(mergeCsv("WWFH1"))

PLHH1_Base <- mergeDataFrames(PLHH1, Data_51204, "D51204")
PLHH1_Base <- mergeDataFrames(PLHH1_Base, D3665, "D3665")
PLHH1_Base <- mergeDataFrames(PLHH1_Base, HOFH1, "HOFH1")
PLHH1_Base <- mergeDataFrames(PLHH1_Base, PHJR, "PHJR")
PLHH1_Base <- mergeDataFrames(PLHH1_Base, WWFH1, "WWFH1")
PLHH1_Base <- dataOffset(4, "SOLR", PLHH1_Base)

rm(Data_51204)
rm(D3665)
rm(HOFH1)
rm(PHJR)
rm(WWFH1)
gc()

setwd(currentWd)