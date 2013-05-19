setGeneric("ggplot")
setMethod("ggplot", "GRanges", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)
  g
})

setMethod("ggplot", "GRangesList", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)  
  g
})

setMethod("ggplot", "IRanges", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)    
  g
})

setMethod("ggplot", "Seqinfo", function(data, ...){
  df <- mold(data)  
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)    
  g
})


setMethod("ggplot", "matrix", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)      
  g
})


setMethod("ggplot", "Views", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)        
  g
})

## to here
setMethod("ggplot", "ExpressionSet", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)          
  g
})

setMethod("ggplot", "SummarizedExperiment", function(data, assay.id = 1, ...){
  df <- mold(data, assay.id = assay.id)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)            
  g
})

setMethod("ggplot", "VCF", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)              
  g
})

setMethod("ggplot", "Rle", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- GGbio(g, data = data)                
  g
})

setMethod("ggplot", "RleList", function(data, ...){
  df <- mold(data)
  g <- ggplot2::ggplot(df, ...)
  g <- ggbio(g)
  g <- GGbio(g, data = data)                
  g
})


setMethod("ggplot", "GAlignments", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)                  
  g
})


setMethod("ggplot", "BamFile", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)                    
  g
})

setMethod("ggplot", "character", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)                      
  g
})


setMethod("ggplot", "TranscriptDb", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)                        
  g
})


setMethod("ggplot", "BSgenome", function(data, ...){
  g <- ggplot2::ggplot(...)
  g <- GGbio(g, data = data)                          
  g
  
})

## setMethod("ggplot", "ANY", function(data, ...){
##   g <- ggplot2::ggplot(data = data, ...)
##   g <- GGbio(g, data = data)
##   g
## })

