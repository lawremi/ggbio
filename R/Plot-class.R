setClass("Plot", contains = "Tracked")

## abstract, so different methods could dispatch on diiferent types of graphics
## instance
setClass("ggplotPlot", contains = c("gg", "ggplot", "Plot"))
setClass("latticePlot", contains = c("trellis","Plot"))
setRefClass("ggbioPlot", contains = c("GGbio", "Plot"))


## Generic function to get subclas instance of 'Plot' class
setGeneric("Plot", function(x, ...) standardGeneric("Plot"))
setMethod("Plot", "gg", function(x){
  new("ggplotPlot", x)
})

## lattice doesn't now how to update itself yet, so mutalbe = FALSE
setMethod("Plot", "trellis", function(x, mutable = FALSE){
  new("latticePlot", x, mutable = mutable)
})

setMethod("Plot", "GGbio", function(x){
  new("ggbioPlot", x)
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

