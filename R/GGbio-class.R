## For a class GGbio, all calls are stacked over each other
GGbio.gen <- setRefClass("GGbio",
                         fields = list(
                           data = "ANY", #raw data
                           ggplot = "ggORNULL", #ggplot object
                           cmd = "list",
                           args = "list"),
                         contains = "Cache")

GGbio <- function(ggplot = NULL, ...){
  GGbio.gen$new(ggplot = ggplot, ...)
}


## alias
ggbio <- GGbio

## combine command if layout_circle presents
.layout_circle.geoms <- c(c("point","line", "link", "ribbon","rect", "bar",
                                       "segment","hist", "scale",  "heatmap", 
                                "ideogram", "text"))

## FIXME, need geom_* for something like ideogram
.combineNames <- function(obj){
  cmd <- obj$cmd
  .nms <- names(cmd)
  idx <- grepl("layout_circle", .nms)
  if(sum(idx)){
    ## did the trick here, which combine the geom/stat/and layout together
    .geom <- grep("geom_", .nms, value = TRUE)
    idx.geom <- grep("geom_", .nms)
    if(length(.geom)){
    .geom <- stringr::str_match(.geom, "geom_([a-z]+)")[, 2]
    if(length(.geom) > 1){
      warning("multiple geoms detected, last one used")
      .geom <- tail(.geom, 1)
      idx.geom <- which(idx.geom)
      idx.geom <- tail(idx.geom, 1)
    }else if(length(.geom) == 1){
      if(!.geom %in% .layout_circle.geoms)
        stop(.geom, "is not supported for layout_circle yet.")
    }else{
      .geom <- NULL
    }}else{
      .geom <- NULL
    }
    .stat <- grep("stat_", .nms, value = TRUE)
    idx.stat  <- grep("stat_", .nms)    
    if(length(.stat)){
    .stat <- str_match(.stat, "stat_([a-z]+)")[, 2]
    if(length(.stat) > 1){
      warning("multiple stats detected, last one used")
      .stat <- tail(.stat, 1)
      idx.stat <- which(idx.stat)
      idx.stat <- tail(idx.stat, 1)
    }else if(length(.stat) == 1){
      if(!.stat %in% .layout_circle.stats)
        stop(.stat, "is not supported for layout_circle yet.")
    }else{
      .stat <- NULL
    }}else{
      .stat <- NULL
    }
    ## to costruct the new call
    if(length(.geom)){
      args <- c(obj$args[[idx.geom]],
                list(geom = .geom))
    ## layout_circle(geom = geom, stat =  stat)
    obj$cmd <- list(do.call(layout_circle, args))
      obj$args <- list()
    }
  }
  obj$ggplot <- obj$ggplot + list(obj$cmd)
  obj
}


setMethod("show", "GGbio", function(object){
  ## this will parse layout first, define our own order here
  object <- .combineNames(object)
  ## then print
  print(object$ggplot)
})

setMethod("+", c("GGbio"), function(e1, e2){
  mc <- match.call()
  .nm <- as.character(mc)[3]
  .tmp <- list(e2)
  names(.tmp) <- .nm
  .args <-   as.list(as.list(mc)$e2)
  .args <- list(.args[-1])
  names(.args) <- .nm
  if(!is.null(e1$data) & is.null(.args$data))
    .args[[1]]$data <- e1$data
  e1$cmd <- c(e1$cmd, .tmp)
  e1$args <- c(e1$args, .args)
  e1 
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
