setGeneric("ggplot")

setMethod("ggplot", "GRanges", function(data, ...){
  df <- fortify(data)
  g <- ggplot(df, ...)
  g$.data <- data
  g <- ggbio(g)
  g
})

setMethod("ggplot", "GRangesList", function(data, ...){
  df <- fortify(data)
  g <- ggplot(df, ...)
  g$.data <- data
  g <- ggbio(g)
  g
})

setMethod("ggplot", "IRanges", function(data, ...){
  df <- fortify(data)
  g <- ggplot(df, ...)
  g$.data <- data
  g <- ggbio(g)
  g
})


setMethod("ggplot", "GappedAlignments", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
})


setMethod("ggplot", "BamFile", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
})

setMethod("ggplot", "character", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})


setMethod("ggplot", "TranscriptDb", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
})


setMethod("ggplot", "BSgenome", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "Rle", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "RleList", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "ExpressionSet", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "Seqinfo", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "VCF", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

setMethod("ggplot", "SummarizedExperiment", function(data, ...){
  g <- ggplot(...)
  g$.data <- data
  g <- ggbio(g)
  g
  
})

