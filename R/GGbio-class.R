## For a class GGbio, all calls are stacked over each other
GGbio.gen <- setRefClass("GGbio",
                         fields = list(
                           data = "ANY", #raw data
                           ggplot = "ggORNULL", #ggplot object
                           cmd = "list",
                           fetchable = "logical"),
                         contains = "Cache")

GGbio <- function(ggplot = NULL, fetchable = FALSE, ...){
  GGbio.gen$new(ggplot = ggplot, fetchable = fetchable, ...)
}

## alias
ggbio <- GGbio

## combine command if layout_circle presents
.layout_circle.geoms <- c("point","line", "link",
                          "ribbon","rect", "bar",
                          "segment","hist", "scale",
                          "heatmap", "ideogram", "text")

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
      .args <- argList(obj$cmd)
      args <- c(.args[[idx.geom]],
                list(geom = .geom))
      if(!is.null(obj$data) & is.null(args$data))
        args$data <- obj$data
      obj$cmd <- do.call(layout_circle, args)
    }
    obj$ggplot <- ggplot() + obj$cmd
  }
  obj
}


setMethod("show", "GGbio", function(object){
  ## this will parse layout first, define our own order here
  object <- .combineNames(object)
  ## then print
  print(object$ggplot)
})

argList <- function(cmd){
  lapply(cmd, function(x){
    args <- as.list(x)[-1]
  })
}

setMethod("+", c("GGbio"), function(e1, e2){
  if(!is(e2, "xlim")){
    e2name <- deparse(substitute(e2))  
    ## special if
    .tmp <- list(e2)
    names(.tmp) <- e2name
    e1$cmd <- c(e1$cmd, .tmp)
    ## get data from object
    if(is.call(e2)){
      args <- as.list(e2)
      if(!is.null(e1$data) & is.null(args$data))
        args$data <- e1$data
      object <- do.call(as.character(args[[1]]), args[-1])
      e1$ggplot <- mapToGG(e1$ggplot, object)
    }else{
      object <- e2
    }
    res <- ggplot2:::add_ggplot(e1$ggplot, object, e2name)
    e1$ggplot <- res
    return(e1)
  }else{
    if(!e1$fetchable){
      e1$ggplot <- e1$ggplot + e2
    }else{
      grl <- cached_which(e1)
      current.which <- grl[length(grl)]
      chr.default <- as.character(seqnames(current.which))
      new.which <- getGrFromXlim(e2, chr.default)
      idx <- needCache(e1, new.which)
      if(!length(idx)){
        ## so need to update cache
        ## point is to re-run the cmd with new which
        e1 <- replaceArg(e1, list(which = new.which))
        ## browser()
        ## FIXME:
        if(TRUE){
          e1 <-eval(e1$cmd[[1]])
        }else{
          e1$ggplot <- e1$ggplot + e1$cmd
        }
        ## how to update which at this point?
      }else{
        ## use cached p
        id <- idx[1]                      #use either one
        e1$ggplot <- e1@cached_item[[id]]$ggplot + e2
      }
    }
    return(e1)
  }
})

## replace *single* arg

replaceArg <- function(p, args){
  .cmd <- lapply(p$cmd, function(cm){
    .args <- as.list(cm)
    if(names(args) %in% names(.args)){
      .args[names(args)] <- args[[1]]
    }
    as.call(.args)
    ## do.call(as.character(.fun), .arg)
  })
  p$cmd <- .cmd
  p
}

needCache <- function(p, new.which){
  current.which <- cached_which(p)
  idx <- subjectHits(findOverlaps(new.which, current.which, type = "within"))
}

getGrFromXlim <- function(xlim, chr.default = NULL){
  .xlim <- xlim$limits$x
  if("chr" %in% names(attributes(xlim))){
    return(GRanges(attr(xlim, "chr"), IRanges(.xlim[1], .xlim[2])))
  }else{
    if(!length(chr.default))
      stop("no seqname found")
    return(GRanges(chr.default, IRanges(.xlim[1], .xlim[2])))    
  }
}


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


