setOldClass("ggplot")
setOldClass("gg")
setOldClass("uneval")
setClass("ggbio", contains = c("gg", "ggplot"))

ggbio <- function(...){
  new("ggbio", ...)  
}

setMethod("+", c("ggbio"), function(e1, e2){
  e2name <- deparse(substitute(e2))  
  if(is.call(e2)){
    args <- as.list(e2)
    if(!is.null(e1$.data))
      args$data <- e1$.data
    object <- do.call(as.character(args[[1]]), args[-1])
    e1 <- mapToGG(e1, object)
  }else{
    object <- e2
  }
  ggplot2:::add_ggplot(e1, object, e2name)
})
?popViewport
## search for proto class, the data and mapping
mapToGG <- function(p, object){
    protos <- returnProto(object) 
    p$mapping <- protos[[1]]$mapping
    p$data <- protos[[1]]$data
    p
}

returnProto <- function(object){
  rapply(object, function(x) x, "proto", how = "unlist")  
}

