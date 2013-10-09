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
setGeneric("bgColor",  function(x) standardGeneric("bgColor"))
setGeneric("bgColor<-",  function(x, value) standardGeneric("bgColor<-"))
setMethod("bgColor", "Tracked", function(x){
  x@bgColor
})
setReplaceMethod("bgColor", c("Tracked", "character"), function(x, value){
  x@bgColor <- value
  x
})

setMethod("bgColor", "gg", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})

setReplaceMethod("bgColor", c("gg", "character"), function(x, value){
  attr(x, "bgcolor") <- value
  x
})

setMethod("bgColor", "GGbio", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})

setReplaceMethod("bgColor", c("GGbio", "character"), function(x, value){
  attr(x, "bgcolor") <- value
  x
})

setMethod("bgColor", "gtable", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})

setReplaceMethod("bgColor", c("gtable", "character"), function(x, value){
  attr(x, "bgcolor") <- value
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

setMethod("fixed", "gg", function(x){
  res <- attr(x, "fixed")
  if(is.null(res))
    return(FALSE)
  else
    return(res)
})

setReplaceMethod("fixed", c("gg", "logical"), function(x, value){
  attr(x, "fixed") <- value
  x
})

setMethod("fixed", "GGbio", function(x){
  res <- attr(x, "fixed")
  if(is.null(res))
    return(FALSE)
  else
    return(res)
})

setReplaceMethod("fixed", c("GGbio", "logical"), function(x, value){
  attr(x, "fixed") <- value
  x
})


## labeled
setGeneric("labeled",  function(x) standardGeneric("labeled"))
setGeneric("labeled<-",  function(x, value) standardGeneric("labeled<-"))
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
})

setMethod("labeled", "gg", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("gg", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
})

setMethod("labeled", "GGbio", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("GGbio", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
})



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
setGeneric("mutable",  function(x) standardGeneric("mutable"))
setGeneric("mutable<-",  function(x, value) standardGeneric("mutable<-"))
setMethod("mutable", "Tracked", function(x){
  x@mutable
})
setReplaceMethod("mutable", c("Tracked", "logical"), function(x, value){
  x@mutable <- value
  x
})
setMethod("mutable", "gg", function(x){
  mt <- attr(x, "mutable")
  if(is.null(mt))
    return(TRUE)
  else
    return(mt)
})

setReplaceMethod("mutable", c("gg", "logical"), function(x, value){
  attr(x, "mutable") <- value
  x
})

setMethod("mutable", "GGbio", function(x){
  mt <- attr(x, "mutable")
  if(is.null(mt))
    return(TRUE)
  else
    return(mt)
})

setReplaceMethod("mutable", c("GGbio", "logical"), function(x, value){
  attr(x, "mutable") <- value
  x
})


## hasAxis
setGeneric("hasAxis",  function(x) standardGeneric("hasAxis"))
setGeneric("hasAxis<-",  function(x, value) standardGeneric("hasAxis<-"))
setMethod("hasAxis", "Tracked", function(x){
  x@hasAxis
})
setReplaceMethod("hasAxis", c("Tracked", "logical"), function(x, value){
  x@hasAxis <- value
  x
})
setMethod("hasAxis", "gg", function(x){
  mt <- attr(x, "hasAxis")
  if(is.null(mt))
    return(FALSE)
  else
    return(mt)
})

setReplaceMethod("hasAxis", c("gg", "logical"), function(x, value){
  attr(x, "hasAxis") <- value
  x
})

setMethod("hasAxis", "GGbio", function(x){
  mt <- attr(x, "hasAxis")
  if(is.null(mt))
    return(FALSE)
  else
    return(mt)
})

setReplaceMethod("hasAxis", c("GGbio", "logical"), function(x, value){
  attr(x, "hasAxis") <- value
  x
})


## height of tracked object
setGeneric("height",  function(x) standardGeneric("height"))
setGeneric("height<-",  function(x, value) standardGeneric("height<-"))
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
setMethod("height", "gg", function(x){
  ht <- attr(x, "height")
  if(is.null(ht))
    return(unit(1, "null"))
  else if(is.numeric(ht)  && !is.unit(ht)){
    return(unit(mt, "null"))
  }else if(is.unit(ht)){
    return(ht)
  }else{
    stop("height attribute must be numeric or ")
  }
})

setReplaceMethod("height", c("gg", "numericORunit"), function(x, value){
  if(length(value) != 1)
    stop("height value can only be of length 1.")
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  attr(x, "height") <- value
  x
})

setMethod("height", "GGbio", function(x){
  ht <- attr(x, "height")
  if(is.null(ht))
    return(unit(1, "null"))
  else if(is.numeric(ht)  && !is.unit(ht)){
    return(unit(mt, "null"))
  }else if(is.unit(ht)){
    return(ht)
  }else{
    stop("height attribute must be numeric or ")
  }
})

setReplaceMethod("height", c("GGbio", "numericORunit"), function(x, value){
  if(length(value) != 1)
    stop("height value can only be of length 1.")
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  attr(x, "height") <- value
  x
})





