setClass("Plot", contains = "Tracked")

## abstract, so different methods could dispatch on diiferent types of graphics
## instance
setClass("ggplotPlot", contains = c("gg", "ggplot", "Plot"))
setClass("latticePlot", contains = c("trellis","Plot"))
setClass("ggbioPlot", contains = c("GGbio", "Plot"))
setClass("ideogramPlot", contains = c("Ideogram", "Plot"))


## Generic function to get subclas instance of 'Plot' class
setGeneric("Plot", function(x, ...) standardGeneric("Plot"))
setMethod("Plot", "gg", function(x){
  x <- ggbio(x)
  obj <- Plot(x)
  obj
})

## lattice doesn't now how to update itself yet, so mutalbe = FALSE
setMethod("Plot", "trellis", function(x, mutable = FALSE){
  idx <- names(attributes(x)) %in% c("fixed", "labeled", "bgColor", "hasAxis", "mutable", "height")
  if(sum(idx)){
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
  if(sum(idx)){
    lst <- attributes(x)[idx]
    obj <- do.call("new", c("ggbioPlot", list(x), lst))
  }else{
    obj <- new("ggbioPlot", x)
  }
  if("geom" %in% names(attributes(x))){
      attr(obj, "geom") <- attr(x, "geom")
  }
  obj
})

## be careful with Ideogram object
setMethod("Plot", "Ideogram", function(x){
    res <- new("ideogramPlot", x)
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




## add tracks + plot
setMethod("c", "PlotList",  function(x, ...){
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        do.call(PlotList, unlist(args, recursive = FALSE))
})

## if raw data, generate plot
genPlots <- function(dots){
  lapply(dots, function(x){
    isPlot <- any(sapply(.supportedPlots, function(c){
      extends(class(x), c)
    }))
    if(!isPlot){
      res <- autoplot(x)
    }else{
      res <- x
    }
    res
  })
}




setMethod("[", c("PlotList", "numeric", "missing"),
          function(x, i, j, ...){
              i <- as.integer(i)
              nms <- names(x)
              x <- initialize(x, x@.Data[i])
              names(x) <- nms[i]
              x
          })


