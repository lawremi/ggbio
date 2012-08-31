setGeneric("fortify")

setMethod("fortify", c("eSet", "missing"), function(model, data){
  object <- model
  pdata <- phenoData(object)
  df <- as(object, "data.frame")
  df$sampleNames <- row.names(df)
  df.m <- melt(df, id.vars =  c(varLabels(pdata), "sampleNames"))
  df.m
})

setMethod("fortify", c("GRanges", "missing"), function(model, data){
  names(model) <- NULL
  df <- as.data.frame(model)
  df$midpoint <- (df$start+df$end)/2
  df
})

