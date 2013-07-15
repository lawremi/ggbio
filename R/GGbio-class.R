## For a class GGbio, all calls are stacked over each other
GGbio.gen <- setClass("GGbio",
                         slots = list(
                           data = "ANY",
                           ggplot = "ggORNULL", #ggplot object
                           cmd = "list",
                           fetchable = "logical",
                           blank = "logical"),
                      contains = "Cache")



GGbio <- function(ggplot = NULL, data = NULL, fetchable = FALSE, blank = FALSE,...){
  new("GGbio", ggplot = ggplot, data = data, fetchable = fetchable, blank = blank, ...)
}
## alias
ggbio <- GGbio


## combine command if circle presents
.circle.geoms <- c("point","line", "link",
                          "ribbon","rect", "bar",
                          "segment","hist", "scale",
                          "heatmap", "ideogram", "text")

## FIXME, need geom_* for something like ideogram
.combineNames <- function(obj){
    cmd <- obj@cmd
  .nms <- names(cmd)
  idx <- "layout_circle" == .nms
  if(sum(idx) && sum(idx)!=length(cmd)){
    ## did the trick here, which combine the geom/stat/and layout together
    .geom <- grep("geom_", .nms, value = TRUE)
    idx.geom <- grep("geom_", .nms)
    if(length(.geom)){
        .geom <- gsub("geom_", "", .geom)        
    ## .geom <- stringr::str_match(.geom, "geom_([a-z]+)")[, 2]
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
        .stat <- gsub("stat_", "", .stat)
      ## .stat <- str_match(.stat, "stat_([a-z]+)")[, 2]
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
      .args <- argList(obj@cmd)
      args <- c(.args[[idx.geom]],
                list(geom = .geom))
      if(!is.null(obj@data) & is.null(args$data))
        args$data <- obj@data
      obj@cmd <- do.call(circle, args)
    }
    p <- ggplot2::ggplot() + obj@cmd
    obj@ggplot <- p
  }
  obj
}


setMethod("show", "GGbio", function(object){
  object <- .combineNames(object)
  if(object@blank)
    warning("No region(with chromosome name) specified, so no data found")
  print(object@ggplot)
})

argList <- function(cmd){
  lapply(cmd, function(x){
    args <- as.list(x)[-1]
  })
}

setMethod("+", c("GGbio"), function(e1, e2){
    if(is(e2, "circle")){
        ## compute radius
        args <- e2
        r.addon <- .radius(e2)
        r <- attr(e1, "radius")
        if(is.null(r)){
            attr(e1, "radius") <- 0
        }
        t.addon <- .trackWidth(e2)
        if(is.null(r.addon)){
            r.cur <-  attr(e1, "radius") +  t.addon
            attr(e1, "radius") <- r.attr <- r.cur
        }else{
            r.cur <- r.addon
            attr(e1, "radius") <- r.attr <- max(attr(e1, "radius"),  r.cur + t.addon)
        }
        args$radius <- r.cur
        args$trackWidth <- t.addon
        e1 <- e1 + do.call(layout_circle, args)
        attr(e1, "radius") <- r.attr
        return(e1)
    }
  if(!is(e2, "xlim")){
    args <- as.list(match.call()$e2)
    e2name <-  deparse(args[[1]])
    .tmp <- list(args)
    names(.tmp) <- e2name
    e1@cmd <- c(e1@cmd, .tmp)
    ## get data from object
    if(is.call(e2)){
        ## args <- as.list(e2)
        if(!is.null(e1@data) & is.null(args$data))
            args$data <- e1@data
        object <- do.call(as.character(args[[1]]), args[-1])
        e1@ggplot <- mapToGG(e1@ggplot, object)
    }else{
      object <- e2
    }
    res <- ggplot2:::add_ggplot(e1@ggplot, object, e2name)
    e1@ggplot <- res
    return(e1)
  }else{
    if(!e1@fetchable){
      e1@ggplot <- e1@ggplot + e2
    }else{
      grl <- cached_which(e1)
      new.which <- getGrFromXlim(e2, chr.default)
      if(length(grl)){
        current.which <- grl[length(grl)]
        chr.default <- as.character(seqnames(current.which))
        idx <- needCache(e1, new.which)
        if(!length(idx)){
          ## so need to update cache
          ## point is to re-run the cmd with new which
          e1 <- replaceArg(e1, list(which = new.which))
          e1 <-eval(e1@cmd[[1]])
        }else{
          ## use cached p
          id <- idx[1]                      #use either one
          e1@ggplot <- e1@cached_item[[id]]@ggplot + e2
        }
      }else{
        e1 <- replaceArg(e1, list(which = new.which))
        e1 <-eval(e1@cmd[[1]])
        ## no cached copy and no current range
      }
    }
    return(e1)
  }
})

## replace *single* arg
replaceArg <- function(p, args){
  .cmd <- lapply(p@cmd, function(cm){
    .args <- as.list(cm)
    .args[names(args)] <- args[[1]]
    as.call(.args)
    ## do.call(as.character(.fun), .arg)
  })
  p@cmd <- .cmd
  p
}

needCache <- function(p, new.which){
  current.which <- cached_which(p)
  idx <- subjectHits(findOverlaps(new.which, current.which, type = "within"))
}

getGrFromXlim <- function(xlim, chr.default = NULL){
  .xlim <- xlim$limits$x
  if("ori" %in% names(attributes(xlim))){
    return(attr(xlim, "ori"))
  }else if("chr" %in% names(attributes(xlim))){
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


