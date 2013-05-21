setClass("Plot", contains = "Tracked")

## abstract, so different methods could dispatch on diiferent types of graphics
## instance
setClass("ggplotPlot", contains = c("gg", "ggplot", "Plot"))
setClass("latticePlot", contains = c("trellis","Plot"))
setRefClass("ggbioPlot", contains = c("GGbio", "Plot"))
setClass("test", contains = c("gg", "ggplot", "Plot"))

## Generic function to get subclas instance of 'Plot' class
setGeneric("Plot", function(x, ...) standardGeneric("Plot"))
setMethod("Plot", "gg", function(x){
  idx <- names(attributes(x)) %in% c("fixed", "labeled", "bgColor", "hasAxis", "mutable", "height")
  if(length(idx)){
    lst <- attributes(x)[idx]
    obj <- do.call("new", c("ggplotPlot", list(x), lst))
  }else{
    obj <- new("ggplotPlot", x)    
  }
  obj
})

## lattice doesn't now how to update itself yet, so mutalbe = FALSE
setMethod("Plot", "trellis", function(x, mutable = FALSE){
  idx <- names(attributes(x)) %in% c("fixed", "labeled", "bgColor", "hasAxis", "mutable", "height")
  if(length(idx)){
    lst <- attributes(x)[idx]
    lst$mutable <- mutable
    obj <- do.call("new", c("latticePlot", list(x), lst))
  }else{
    obj <- new("latticePlot", x, mutable = mutable)    
  }
  obj
})

setMethod("Plot", "GGbio", function(x){
  idx <- names(attributes(x)) %in% c("fixed", "labeled", "bgColor", "hasAxis", "mutable", "height")
  if(length(idx)){
    lst <- attributes(x)[idx]
    obj <- do.call("new", c("ggbioPlot", list(x), lst))
  }else{
    obj <- new("ggbioPlot", x)    
  }
  obj
})



## compare to grobList, plotList return a list of original plot
## supported grobs only
setClass("PlotList", prototype = prototype(elementType = "Plot"),
         contains = "list")

setValidity("PlotList", .validList)

## validate via constructor
PlotList <- function(...){
  items <- list(...)
  items <- reduceListOfPlots(items)
  items <- lapply(items, Plot)
  new("PlotList", items)
}

## original list of plots
plotList <- function(...){
  items <- list(...)
  items <- reduceListOfPlots(items)
}




