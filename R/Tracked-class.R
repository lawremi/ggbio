## TODO:ideogram need to be fixed
setClass("Tracked", slots = c(mutable = "logical", fixed = "logical",
                      labeled = "logical", hasAxis = "logical",
                      bgColor = "character", height = "numericORunit"),
         prototype = list(mutable = TRUE, fixed = FALSE, labeled = TRUE,
           hasAxis = FALSE, bgColor = "white", height = unit(1, "null")))


Tracked <- function(mutable = TRUE, fixed = FALSE, labeled = TRUE,
                    hasAxis = FALSE, bgColor = "white", height = unit(1, "null")){
  new("Tracked", mutable = mutable, fixed = fixed, labeled = labeled,
      hasAxis = hasAxis, bgColor = bgColor, height = height)
}

## background color
setGeneric("bgColor",  function(x, ...) standardGeneric("bgColor"))
setGeneric("bgColor<-",  function(x, value,  ...) standardGeneric("bgColor<-"))
setMethod("bgColor", "Tracked", function(x){
  x@bgColor
})
setReplaceMethod("bgColor", c("Tracked", "character"), function(x, value){
  x@bgColor <- value
  x
})


## fixed
setMethod("fixed", "Tracked", function(x){
  x@fixed
})
setReplaceMethod("fixed", c("Tracked", "logical"), function(x, value){
  x@fixed <- value
  x
})


## labeled
setGeneric("labeled",  function(x, ...) standardGeneric("labeled"))
setGeneric("labeled<-",  function(x, value,  ...) standardGeneric("labeled<-"))
setMethod("labeled", "Tracked", function(x){
  x@labeled
})
setReplaceMethod("labeled", c("Tracked", "logical"), function(x, value){
  x@labeled <- value
  x
})

setMethod("labeled", "Ideogram", function(x){
  x@labeled
})
setReplaceMethod("labeled", c("Ideogram", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
})

setMethod("labeled", "gtable", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("gtable", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
}
)

setOldClass("text")
setMethod("labeled", "text", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setMethod("labeled", "gTree", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

## mutable
setGeneric("mutable",  function(x, ...) standardGeneric("mutable"))
setGeneric("mutable<-",  function(x, value,  ...) standardGeneric("mutable<-"))
setMethod("mutable", "Tracked", function(x){
  x@mutable
})
setReplaceMethod("mutable", c("Tracked", "logical"), function(x, value){
  x@mutable <- value
  x
})

## hasAxis
setGeneric("hasAxis",  function(x, ...) standardGeneric("hasAxis"))
setGeneric("hasAxis<-",  function(x, value,  ...) standardGeneric("hasAxis<-"))
setMethod("hasAxis", "Tracked", function(x){
  x@hasAxis
})
setReplaceMethod("hasAxis", c("Tracked", "logical"), function(x, value){
  x@hasAxis <- value
  x
})

## height of tracked object
setGeneric("height",  function(x, ...) standardGeneric("height"))
setGeneric("height<-",  function(x, value,  ...) standardGeneric("height<-"))
setMethod("height", "Tracked", function(x){
  ht <- x@height
  if(is.numeric(ht)  && !is.unit(ht)){
    ht <- unit(mt, "null")
  }
  ht
})
setReplaceMethod("height", c("Tracked", "numericORunit"), function(x, value){
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  x@height <- value
  x
})



