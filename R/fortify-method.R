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
  vals <- values(model)
  if(length(vals)){
    idx <- !unlist(lapply(vals@listData, function(x) is(x, "List") & !is(x, "DNAStringSet")))
    if(sum(!idx))
      warning(colnames(vals)[!idx], " column has been dropped for the reason that the corecion of class List is not supported")
    df <- as.data.frame(model[,idx])
  }else{
    df <- as.data.frame(model)
  }
  df$midpoint <- (df$start+df$end)/2
  df
})

