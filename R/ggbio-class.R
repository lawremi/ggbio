setOldClass("ggplot")
setOldClass("gg")
setClass("ggbio", contains = c("gg", "ggplot"))
ggbio <- function(x){
  new("ggbio", x)  
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
  res <- ggplot2:::add_ggplot(e1, object, e2name)
  res <- ggbio(res)
  res
})

setStat <- function(x){
  attr(x, "isStat") <- TRUE
  x
}
isStat <- function(x){
  res <- attr(x, "isStat")
  if(is.null(res))
    res <- FALSE
  res
}

## search for proto class, the data and mapping

mapToGG <- function(p, object){
    protos <- returnProto(object)
    if(isStat(object) == TRUE){
      p$mapping <- protos[[1]]$mapping
      p$data <- protos[[1]]$data
    }
    p
}

returnProto <- function(object){
  rapply(object, function(x) x, "proto", how = "unlist")  
}

