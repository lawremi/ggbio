setClass("Cache",
            slots = list(
              cached = "logical",
              cached_xlim = "numericORNULL",
              cached_ylim = "numericORNULL",
              cached_which = "GRangesORNULL",
              cached_item = "list"
              ),
         prototype = list(cached = TRUE,
                          cached_xlim = 1,
                          cached_ylim = NULL,
                          cached_which = NULL,
             cached_item = list()
             ))

## cached always equal TRUE
## only for 'fetchable' object, set it to FALSE
Cache <- function(..., cached = TRUE, cached_xlim = NULL, cached_ylim = NULL,
                  cached_which = NULL, cached_item = list()){
  new("Cache", cached = cached,
      cached_xlim = cached_xlim,
      cached_ylim = cached_ylim,
      cached_which = cached_which,
      cached_item = cached_item, ...)
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

setGeneric("cached_item", function(x, ...) standardGeneric("cached_item"))
setMethod("cached_item", "Cache", function(x){
  x@cached_item
})
setGeneric("cached_item<-", function(x, value) standardGeneric("cached_item<-"))
setReplaceMethod("cached_item", c("Cache", "list"), function(x, value){
  x@cached_item <- value
  x
})

setGeneric("addItem", function(x, ...) standardGeneric("addItem"))
setMethod("addItem", c("Cache"), function(x, ...){
  x@cached_item <- c(x@cached_item, list(...))
  x
})



setGeneric("cached_which", function(x, ...) standardGeneric("cached_which"))
setMethod("cached_which", "Cache", function(x){
  x@cached_which
})
setGeneric("cached_which<-", function(x, value) standardGeneric("cached_which<-"))
setReplaceMethod("cached_which", c("Cache", "GRanges"), function(x, value){
  x@cached_which<- value
  x
})


setGeneric("addWhich", function(x, value, ...) standardGeneric("addWhich"))
setMethod("addWhich", c("Cache", "GRanges"), function(x, value){
  if(is.null(x@cached_which)){
    x@cached_which <- value
  }else{
    x@cached_which <- c(x@cached_which, value)
  }
  x
})

## cacheSet cache item and which at the same time, make sure the lengths equals
setGeneric("cacheSet", function(x, value, ...) standardGeneric("cacheSet"))
setMethod("cacheSet", c("Cache", "GRanges"), function(x, value){
  x <- addItem(x, x)
  x <- addWhich(x, value)
  x
})






