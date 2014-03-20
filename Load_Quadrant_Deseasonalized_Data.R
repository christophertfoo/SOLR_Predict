if(file.exists("Deseasonalized.RData")) {
  writeLines("Loading Deseasonalized.RData")
  load("Deseasonalized.RData")
} else {
  source("Deseasonalize.R")
}

source('SOLR_Predict.R')

low_wind_threshold <- 3
wind_column <- "SKNT"
drct_column <- "DRCT"
offset_wind_column <- "SKNT_6"
offset_drct_column <- "DRCT_6"

deseasonalized_temp <- deseasonalized
deseasonalized_offset_temp <- dataOffset(6, c("SOLR", "SKNT", "DRCT", "SKNT_E", "SKNT_N"), deseasonalized_temp)

writeLines("Low Wind")
deseasonalized <- deseasonalized_temp[deseasonalized_temp[[wind_column]] < low_wind_threshold,]
deseasonalized_offset <- deseasonalized_offset_temp[deseasonalized_offset_temp[[offset_wind_column]] < low_wind_threshold,]
save(deseasonalized, deseasonalized_offset, deseasonalized_signal, file="Data_Deseasonalized_Low_Wind.RData")
rm(deseasonalized)
rm(deseasonalized_offset)
gc()

writeLines("Quadrant 1")
deseasonalized <- deseasonalized_temp[deseasonalized_temp[[wind_column]] >= low_wind_threshold & (deseasonalized_temp[[drct_column]] >= 90 & deseasonalized_temp[[drct_column]] < 180),]
deseasonalized_offset <- deseasonalized_offset_temp[deseasonalized_offset_temp[[offset_wind_column]] >= low_wind_threshold & (deseasonalized_offset_temp[[offset_drct_column]] >= 90 & deseasonalized_offset_temp[[offset_drct_column]] < 180),]
save(deseasonalized, deseasonalized_offset, deseasonalized_signal, file="Data_Deseasonalized_Quadrant_1.RData")
rm(deseasonalized)
rm(deseasonalized_offset)
gc()

writeLines("Quadrant 4")
deseasonalized <- deseasonalized_temp[deseasonalized_temp[[wind_column]] >= low_wind_threshold & (deseasonalized_temp[[drct_column]] >= 0 & deseasonalized_temp[[drct_column]] < 90),]
deseasonalized_offset <- deseasonalized_offset_temp[deseasonalized_offset_temp[[offset_wind_column]] >= low_wind_threshold & (deseasonalized_offset_temp[[offset_drct_column]] >= 0 & deseasonalized_offset_temp[[offset_drct_column]] < 90),]
save(deseasonalized, deseasonalized_offset, deseasonalized_signal, file="Data_Deseasonalized_Quadrant_4.RData")
rm(deseasonalized)
rm(deseasonalized_offset)
gc()

writeLines("Quadrant 3")
deseasonalized <- deseasonalized_temp[deseasonalized_temp[[wind_column]] >= low_wind_threshold & (deseasonalized_temp[[drct_column]] >= -90 & deseasonalized_temp[[drct_column]] < 0),]
deseasonalized_offset <- deseasonalized_offset_temp[deseasonalized_offset_temp[[offset_wind_column]] >= low_wind_threshold & (deseasonalized_offset_temp[[offset_drct_column]] >= -90 & deseasonalized_offset_temp[[offset_drct_column]] < 0),]
save(deseasonalized, deseasonalized_offset, deseasonalized_signal, file="Data_Deseasonalized_Quadrant_3.RData")
rm(deseasonalized)
rm(deseasonalized_offset)
gc()

writeLines("Quadrant 2")
deseasonalized <- deseasonalized_temp[deseasonalized_temp[[wind_column]] >= low_wind_threshold & ((deseasonalized_temp[[drct_column]] >= -180 & deseasonalized_temp[[drct_column]] < -90) | deseasonalized_temp[[drct_column]] == 180),]
deseasonalized_offset <- deseasonalized_offset_temp[deseasonalized_offset_temp[[offset_wind_column]] >= low_wind_threshold & ((deseasonalized_offset_temp[[offset_drct_column]] >= -180 & deseasonalized_offset_temp[[offset_drct_column]] < -90) | deseasonalized_offset_temp[[offset_drct_column]] == 180),]
save(deseasonalized, deseasonalized_offset, deseasonalized_signal, file="Data_Deseasonalized_Quadrant_2.RData")
rm(deseasonalized)
rm(deseasonalized_offset)
gc()