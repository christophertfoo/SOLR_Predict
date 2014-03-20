if(file.exists("Data.RData")) {
  writeLines("Loading Data.RData")
  load("Data.RData")
} else {
  source("Load_Data.R")
}

source('SOLR_Predict.R')

low_wind_threshold <- 3
wind_column <- "SKNT"
drct_column <- "DRCT"
offset_wind_column <- "SKNT_6"
offset_drct_column <- "DRCT_6"

merged_temp <- merged
offset_solr_temp <- dataOffset(6, c("SOLR", "SKNT", "DRCT", "SKNT_E", "SKNT_N"), merged_temp)
offset_solr_frac_temp <- dataOffset(6, c("SOLR_FRAC", "SKNT", "DRCT", "SKNT_E", "SKNT_N"), merged_temp)

writeLines("Low Wind")
merged <- merged_temp[merged_temp[[wind_column]] < low_wind_threshold,]
offset_solr <- offset_solr_temp[offset_solr_temp[[offset_wind_column]] < low_wind_threshold,]
offset_solr_frac <- offset_solr_frac_temp[offset_solr_frac_temp[[offset_wind_column]] < low_wind_threshold,]
save(merged, offset_solr, offset_solr_frac, file="Data_Low_Wind.RData")
rm(merged)
rm(offset_solr)
rm(offset_solr_frac)
gc()

writeLines("Quadrant 1")
merged <- merged_temp[merged_temp[[wind_column]] >= low_wind_threshold & (merged_temp[[drct_column]] >= 90 & merged_temp[[drct_column]] < 180),]
offset_solr <- offset_solr_temp[offset_solr_temp[[offset_wind_column]] >= low_wind_threshold & (offset_solr_temp[[offset_drct_column]] >= 90 & offset_solr_temp[[offset_drct_column]] < 180),]
offset_solr_frac <- offset_solr_frac_temp[offset_solr_frac_temp[[offset_wind_column]] < low_wind_threshold & (offset_solr_frac_temp[[offset_drct_column]] >= 90 & offset_solr_frac_temp[[offset_drct_column]] < 180),]
save(merged, offset_solr, offset_solr_frac, file="Data_Quadrant_1.RData")
rm(merged)
rm(offset_solr)
rm(offset_solr_frac)
gc()

writeLines("Quadrant 4")
merged <- merged_temp[merged_temp[[wind_column]] >= low_wind_threshold & (merged_temp[[drct_column]] >= 0 & merged_temp[[drct_column]] < 90),]
offset_solr <- offset_solr_temp[offset_solr_temp[[offset_wind_column]] >= low_wind_threshold & (offset_solr_temp[[offset_drct_column]] >= 0 & offset_solr_temp[[offset_drct_column]] < 90),]
offset_solr_frac <- offset_solr_frac_temp[offset_solr_frac_temp[[offset_wind_column]] < low_wind_threshold & (offset_solr_frac_temp[[offset_drct_column]] >= 0 & offset_solr_frac_temp[[offset_drct_column]] < 90),]
save(merged, offset_solr, offset_solr_frac, file="Data_Quadrant_4.RData")
rm(merged)
rm(offset_solr)
rm(offset_solr_frac)
gc()

writeLines("Quadrant 3")
merged <- merged_temp[merged_temp[[wind_column]] >= low_wind_threshold & (merged_temp[[drct_column]] >= -90 & merged_temp[[drct_column]] < 0),]
offset_solr <- offset_solr_temp[offset_solr_temp[[offset_wind_column]] >= low_wind_threshold & (offset_solr_temp[[offset_drct_column]] >= -90 & offset_solr_temp[[offset_drct_column]] < 0),]
offset_solr_frac <- offset_solr_frac_temp[offset_solr_frac_temp[[offset_wind_column]] < low_wind_threshold & (offset_solr_frac_temp[[offset_drct_column]] >= -90 & offset_solr_frac_temp[[offset_drct_column]] < 0),]
save(merged, offset_solr, offset_solr_frac, file="Data_Quadrant_3.RData")
rm(merged)
rm(offset_solr)
rm(offset_solr_frac)
gc()

writeLines("Quadrant 2")
merged <- merged_temp[merged_temp[[wind_column]] >= low_wind_threshold & ((merged_temp[[drct_column]] >= -180 & merged_temp[[drct_column]] < -90) | merged_temp[[drct_column]] == 180),]
offset_solr <- offset_solr_temp[offset_solr_temp[[offset_wind_column]] >= low_wind_threshold & ((offset_solr_temp[[offset_drct_column]] >= -180 & offset_solr_temp[[offset_drct_column]] < -90) | offset_solr_temp[[offset_drct_column]] == 180),]
offset_solr_frac <- offset_solr_frac_temp[offset_solr_frac_temp[[offset_wind_column]] < low_wind_threshold & ((offset_solr_frac_temp[[offset_drct_column]] >= -180 & offset_solr_frac_temp[[offset_drct_column]] < -90) | offset_solr_frac_temp[[offset_drct_column]] == 180),]
save(merged, offset_solr, offset_solr_frac, file="Data_Quadrant_2.RData")
rm(merged)
rm(offset_solr)
rm(offset_solr_frac)
gc()