.supportedPlots <- c("gg", "trellis", "GGbio")
isSupportedPlots <- function(x){
  sapply(x, function(z){
    any(sapply(.supportedPlots, function(c){
      extends(class(z), c)
    }))
  })
}

setClass("Grob", contains = "VIRTUAL")
setClass("ggplotGrob", contains = c("gtable", "grob", "Grob"))
setClass("latticeGrob", contains = c("lattice", "grob", "Grob"))

## Grob creat instance of sub-class
setGeneric("Grob", function(x, ...) standardGeneric("Grob"))
setMethod("Grob", "gg", function(x){
  new("ggplotGrob", ggplotGrob(x))
})
setMethod("Grob", "gtable", function(x){
  new("ggplotGrob", x)  

})
setMethod("Grob", "trellis", function(x){
  new("latticeGrob", latticeGrob(x))
})

setMethod("Grob", "lattice", function(x){
   new("latticeGrob", x)
})

setMethod("Grob", "GGbio", function(x){
   new("ggplotGrob", ggplotGrob(x$ggplot))
})


setClass("GrobList", prototype = prototype(elementType = "Grob"),
         contains = "list")

.validList <- function(x){
  if(all(sapply(x, is, x@elementType)))
    return(TRUE)
  else
    paste("Class must be", x@elementType)
}
setValidity("GrobList", .validList)

## constructor for class 'grobList'
GrobList <- function(...){
  items <- list(...)
  items <- listOfGrobs(items)
  new("GrobList", items)
}

reduceListOfPlots <- function(x){
  firstElementIsListOfGrobs <-
    length(x) == 1 && is.list(x[[1L]]) 
  if (firstElementIsListOfGrobs)
    x <- x[[1]]
  idx <- isSupportedPlots(x)
  if(!all(idx)){
    warning("only class ", paste(.supportedPlots, collapse = "|"), " are suppoted for binding in tracks.")
    warning("plot ", paste(which(!idx), collpase = " and "), " is dropped")
  }
  if(length(idx))
    return(x[idx])
  else
    return(NULL)
}

## this return a list of 'grobs' from list of valided 'plots' which could have grobs returned
listOfGrobs <- function(x) {
  x <- reduceListOfPlots(x)
  lapply(x, Grob)
}
