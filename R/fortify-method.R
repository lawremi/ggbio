setGeneric("fortify")

setMethod("fortify", c("ANY", "eSet"), function(model = NULL, data){
  object <- data
  pdata <- phenoData(object)
  df <- as(object, "data.frame")
  df$sampleNames <- row.names(df)
  df.m <- melt(df, id.vars =  c(varLabels(pdata), "sampleNames"))
  df.m
})

setMethod("fortify", c("ANY", "GRanges"), function(model = NULL, data){
  df <- as.data.frame(data)
  df$midpoint <- (df$start+df$end)/2
  df
})

