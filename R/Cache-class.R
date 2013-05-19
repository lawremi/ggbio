setClass("Cache", 
         slots = c(cached = "logical",
                   cached_xlim = "numericORNULL",
                   cached_ylim = "numericORNULL"),
         prototype = prototype(
           cached = FALSE
           ))

Cache <- function(cached = FALSE, cached_xlim = NULL, cached_ylim = NULL){
  new("Cache", cached = cached, cached_xlim = cached_xlim, cached_ylim = cached_ylim)
}

setGeneric("cached", function(x, ...) standardGeneric("cached"))
setMethod("cached", "Cache", function(x){
  x@cached
})
setGeneric("cached<-", function(x, value) standardGeneric("cached<-"))
setReplaceMethod("cached", c("Cache", "logical"), function(x, value){
  x@cached <- value
  x
})


setGeneric("cached_xlim", function(x, ...) standardGeneric("cached_xlim"))
setMethod("cached_xlim", "Cache", function(x){
  x@cached_xlim 
})
setGeneric("cached_xlim<-", function(x, value) standardGeneric("cached_xlim<-"))
setReplaceMethod("cached_xlim", c("Cache", "numeric"), function(x, value){
  if(length(value) == 1)
    value <- rep(value, 2)
  if(length(value) > 1)
    value <- range(value)
  x@cached_xlim <- value
  x
})


setGeneric("cached_ylim", function(x, ...) standardGeneric("cached_ylim"))
setMethod("cached_ylim", "Cache", function(x){
  x@cached_ylim
})
setGeneric("cached_ylim<-", function(x, value) standardGeneric("cached_ylim<-"))
setReplaceMethod("cached_ylim", c("Cache", "numeric"), function(x, value){
  if(length(value) == 1)
    value <- rep(value, 2)
  if(length(value) > 1)
    value <- range(value)
  x@cached_ylim<- value
  x
})

