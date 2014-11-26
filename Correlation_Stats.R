cor_file <- 'Solr_Deseasonalized_Correlations.RData'
output_file <- 'Correlation_Stats_5.csv'

num_features <- 5
method <- 'P'


load(cor_file)

correlations <- get(grep(paste(method, "_", sep=""), ls(), value=T))

feat_counts <- list()

num_models <- 0

for(group in names(correlations)) {
  for(hour in names(correlations[[group]])) {
    writeLines(paste("Group:", group, ", Hour:", hour))
    num_models <- num_models + 1
    
    inst <- correlations[[group]][[hour]]
    inst <- inst[order(-abs(inst$correlations)),]
    inst <- inst[1:num_features,]
    
    for(i in 1:nrow(inst)) {
      name <- as.character(inst[i, 'names'])
      correlation <- inst[i, 'correlations']
      if(!is.na(correlation)) {
        if(is.null(feat_counts[[name]])) {
          feat_counts[[name]] <- list()
          feat_counts[[name]][['count']] <- 1
          feat_counts[[name]][['correlations']] <- correlation
        } else {
          feat_counts[[name]][['count']] <- feat_counts[[name]][['count']] + 1
          feat_counts[[name]][['correlations']] <- c(feat_counts[[name]][['correlations']], correlation)
        }
      }
    }
  }
}

features <- character(0)
counts <- numeric(0)
percents <- numeric(0)
avg_correlations <- numeric(0)

for(feature in names(feat_counts)) {
  features <- c(features, feature)
  counts <- c(counts, feat_counts[[feature]][['count']])
  percents <- c(percents, (feat_counts[[feature]][['count']] / num_models) * 100)
  avg_correlations <- c(avg_correlations, mean(feat_counts[[feature]][['correlations']]))
}

frame <- data.frame(Feature=features, Count=counts, Percent=percents, AvgCorrelation=avg_correlations)
write.csv(frame[order(-frame$Percent),], row.names=F, file=output_file)