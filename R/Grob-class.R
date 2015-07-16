.supportedPlots <- c("gg", "trellis", "GGbio")
isSupportedPlots <- function(x){
  sapply(x, function(z){
    any(sapply(.supportedPlots, function(c){
      extends(class(z), c)
    }))
  })
}

setClass("Grob", contains = "VIRTUAL")
## setClass("ggplotGrob", contains = c("gtable", "grob", "Grob"))
## setClass("latticeGrob", contains = c("lattice", "grob", "Grob"))

## Grob creat instance of sub-class
setGeneric("Grob", function(x, ...) standardGeneric("Grob"))
setMethod("Grob", "gg", function(x){
  ## new("ggplotGrob", ggplotGrob(x))
  ggplotGrob(x)
})
setMethod("Grob", "gtable", function(x){
  ## new("ggplotGrob", x)
  x
})
setMethod("Grob", "trellis", function(x){
  ## new("latticeGrob", latticeGrob(x))
  gridExtra:::latticeGrob(x)
})
setMethod("Grob", "lattice", function(x){
  x
})

setMethod("Grob", "GGbio", function(x){
   ## new("ggplotGrob", ggplotGrob(x@ggplot))
   ggplotGrob(x@ggplot)
})


## setClass("GrobList", prototype = prototype(elementType = "Grob"),
##          contains = "list")

.validList <- function(object){
  if(all(sapply(object, is, object@elementType)))
    return(TRUE)
  else
    paste("Class must be", object@elementType)
}
## setValidity("GrobList", .validList)

## ## constructor for class 'grobList'
GrobList <- function(...){
  items <- list(...)
  items <- listOfGrobs(items)
  ## new("GrobList", items)
  items
}

reduceListOfPlots <- function(x){
  firstElementIsListOfGrobs <-
    length(x) == 1 && is.list(x[[1L]]) && !extends(class(x[[1]]), "gg")
  if (firstElementIsListOfGrobs)
    x <- x[[1]]
  x
}

## this return a list of 'grobs' from list of valided 'plots' which could have grobs returned
listOfGrobs <- function(x) {
  x <- reduceListOfPlots(x)
  lapply(x, Grob)
}
