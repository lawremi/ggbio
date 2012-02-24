newDataAfterFacetByGr <- function(gr, which, id.name){
  gr <- sort(gr)
  which <- sort(which)
  ## suppose which is a GRanges
  res <- lapply(seq_len(length(which)),function(i){
    res <- subsetByOverlaps(gr, which[i])
    values(res)$.bioviz.facetid <- i
    res
  })
  do.call(c, res)
}

getLimits <- function(obj){
  x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
  ## x
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  ## y
  if(!is.null(obj$mapping$y) && length(obj$data))
    y <- eval(obj$mapping$y, obj$data)
  
  if(!is.null(obj$mapping$xmin) && length(obj$data))
    xmin <- eval(obj$mapping$xmin, obj$data)

  
  if(!is.null(obj$mapping$ymin) && length(obj$data))
    ymin <- eval(obj$mapping$ymin, obj$data)
  
  if(!is.null(obj$mapping$xmax) && length(obj$data))
    xmax <- eval(obj$mapping$xmax, obj$data)
  
  if(!is.null(obj$mapping$ymax) && length(obj$data))
    ymax <- eval(obj$mapping$ymax, obj$data)
  
  if(!is.null(obj$mapping$xend) && length(obj$data))
    xend <- eval(obj$mapping$xend, obj$data)
  
  if(!is.null(obj$mapping$yend) && length(obj$data))
    yend <- eval(obj$mapping$yend, obj$data)
  else
    yend <- NULL

  ## if(length(obj$layer)>1){

  l.res <- getLimitsFromLayer(obj)
  ## }else{
  ##   l.res <- NULL
  ## }
  res <- list(xlim = c(min(c(l.res$xmin, x, xmin)),
                max(c(l.res$xmax, x, xmax, xend))),
              ylim = c(min(c(l.res$ymin, y, ymin)),
                max(c(l.res$ymax, y, ymax, yend))))
  if(length(obj$coordinates$limits$x) == 2)
    res$xlim <- obj$coordinates$limits$x
  
  if(length(obj$coordinates$limits$y) == 2)
    res$ylim <- obj$coordinates$limits$y

  res

}

getLimitsFromLayer <- function(obj){
  layers <- obj$layer
  lst <- lapply(layers, function(layer){
    if(length(obj$data) | length(layer$data)){
      
    if(length(layer$data))
      dt <- layer$data
    else
      dt <- obj$data
    if(!is.null(layer$mapping)){
    if(!is.null(layer$mapping$x))
      x <- eval(layer$mapping$x, dt)
    else
      x <- NULL
    
    if(!is.null(layer$mapping$y))
      y <- eval(layer$mapping$y, dt)
    else
      y <- NULL
    
    if(!is.null(layer$mapping$xmin))
      xmin <- eval(layer$mapping$xmin, dt)
    else
      xmin <- NULL
    
    if(!is.null(layer$mapping$ymin))
      ymin <- eval(layer$mapping$ymin, dt)
    else
      ymin <- NULL
    
    if(!is.null(layer$mapping$xmax))
      xmax <- eval(layer$mapping$xmax, dt)
    else
      xmax <- NULL
    
    if(!is.null(layer$mapping$ymax))
      ymax <- eval(layer$mapping$ymax, dt)
    else
      ymax <- NULL
    
    if(!is.null(layer$mapping$xend))
      xend <- eval(layer$mapping$xend, dt)
    else
      xend <- NULL
    
    if(!is.null(layer$mapping$yend))
      yend <- eval(layer$mapping$yend, dt)
    else
      yend <- NULL
    
    res <- data.frame(xmin = min(c(x, xmin)),
                      xmax = max(c(x, xmax, xend)),
                      ymin = min(c(y, ymin)),
                      ymax = max(c(y, ymax, yend)))
  }else{
    res <- NULL
  }
  }else{
    res <- NULL
  }
  })
  lst <- lst[!is.null(lst)]
  res <- do.call("rbind", lst)
  res
}

getGap <- function(data, group.name, facets = NULL){
  if(!length(facets))
    facets <- as.formula(~seqnames)
  allvars <- all.vars(as.formula(facets))
  allvars.extra <- allvars[!allvars %in% c(".", "seqnames")]


  if(!".levels" %in% colnames(values(data)))
    stop(".levels is not in data")
  grl <- splitByFacets(data, facets)  
  ## res <- split(data, seqnames(data))
  
  grl <- lapply(grl, function(dt){
    res <- split(dt, values(dt)[,group.name])
    gps.lst <- lapply(res, function(x){
      if(length(x) > 1){
        gps <- gaps(ranges(x))
        if(length(gps)){
          seqs <- unique(as.character(seqnames(x)))
          ir <- gps
          gr <- GRanges(seqs, ir)
          values(gr)$.levels <- unique(values(x)$.levels)
          values(gr)[,allvars.extra] <- rep(unique(values(x)[, allvars.extra]),
                                            length(gr))
          
          gr
        }else{
          NULL
        }}else{
          NULL
        }
    })
    idx <- which(!unlist(lapply(gps.lst, is.null)))
    gps <- do.call(c, gps.lst[idx])
    ## if(!is.null(gps)){
    ##   res.e <- try(gps <- do.call("c", unname(gps)))
    ##   if(inherits(res.e, "try-error")) browser()
    ## }
  })
  res <- unlist(do.call(GRangesList, do.call(c, grl)))
  values(res)$type <- "gaps"
  res <- resize(res, width = width(res) + 2, fix = "center")
}

## suppose we have freq?
getModelRange <- function(data, group.name){
  seqs <- unique(as.character(seqnames(data)))
  ir <- unlist(range(ranges(split(data, values(data)[,group.name]),
                            ignore.strand = TRUE)))
  ## freqs <- values(data)$freq[match(names(ir), values(data)[,group.name])]
  .lvs <- values(data)$.levels[match(names(ir), values(data)[,group.name])]
  ## with levels
  gr <- GRanges(seqs, ir, .levels = .lvs)
  values(gr)$.label <- names(gr)
  gr
}



## going to be moved to biovizBase
setGeneric("getYLab", function(obj,...) standardGeneric("getYLab"))
setMethod("getYLab", "TranscriptDb", function(obj){
  md <- metadata(obj)
  ds <- md[md[,1] == "Data source",2]
  lb <- character()
  if(ds == "UCSC"){
    lb <- md[md[,1] == "UCSC Table",2]
  }
  if(ds == "BioMart"){
    lb <- md[md[,1] == "BioMart Database",2]
  }
  lb
})



gr2newLinear <- function(obj, space.skip = 0.1){
  obj <- sort(obj)
  seqs.l <- seqlengths(obj)
  if(all(!is.na(seqs.l))){
    chr.l <- seqs.l
    seqs.suml <- sum(as.numeric(seqs.l))
  }else{
    ## if no seqlengths are found, use obj range
    chr.l <- max(end(split(obj, as.character(seqnames(obj)))))
    seqs.suml <- sum(as.numeric(chr.l))
  }
  chr.l.back <- chr.l
  space.skip <- space.skip * seqs.suml
  skps <- space.skip * ((1:length(seqs.l)))
  names(skps) <- names(seqlengths(obj))
  nms <- names(chr.l)
  chr.l <- cumsum(as.numeric(chr.l))
  chr.l2 <- c(0, chr.l[-length(chr.l)])
  names(chr.l2) <- nms
  sts.new <- start(obj) + skps[as.character(seqnames(obj))] +
    chr.l2[as.character(seqnames(obj))]
  values(obj)$.biovizBase.start <- sts.new
  ed.new <- end(obj) + skps[as.character(seqnames(obj))] +
    chr.l2[as.character(seqnames(obj))]
  values(obj)$.biovizBase.end <- ed.new
  values(obj)$.biovizBase.mid <- (sts.new + ed.new) /2
  max.chr <- rev(names(chr.l2))[1]
  x.max <- chr.l.back[max.chr] + skps[max.chr] + chr.l2[max.chr]
  ed.new <- end(obj) + skps[as.character(seqnames(obj))] +
    chr.l2[as.character(seqnames(obj))]
  metadata(obj)$view.max <- x.max
  obj
}

## then need a transformation to circlular view
## data is a GRanges object
gr2circle <- function(obj, x = ".biovizBase.mid", y,
                      radius = 10, width = 10, direction = c("clockwise", "anticlockwise")){

  if(!length(y)){
    values(obj)$.biovizBase.equal.y <- 1
    y <- as.character(".biovizBase.equal.y")
    ## stop("y is missing")
  }else if(!is.character(y) && !is.numeric(y)){
    stop("y must be character or numeric")
  }

  temp.x <- values(obj)[,x]
  
  if(is.character(y))
    temp.y <- values(obj)[,y]
  if(is.numeric(y)){
    temp.y <- y
  }
  
  temp.y.r <- expand_range(range(temp.y), mul = 0.05)
  temp.y <- (temp.y - min(temp.y.r))/diff(temp.y.r) * width + radius
  
  ## always from 1 to max
  direction <- match.arg(direction)
  if(direction == "clockwise")
    dir <- 1
  else
    dir <- -1
  ## need to use max of "ideo"
  x.max <- metadata(obj)$view.max
  angle.unit <- pi* 2/ x.max
  x.new <- temp.y * sin(angle.unit * temp.x * dir)
  y.new <- temp.y * cos(angle.unit * temp.x * dir)
  values(obj)$.biovizBase.x <- x.new
  values(obj)$.biovizBase.y <- y.new
  values(obj)$.biovizBase.angle <- angle.unit * temp.x * dir
  obj
}

## interpolate segment or rectangle
## rectangle only use stepping as y


rectInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction,
                      n = 5){

data.back <- data
  inter.fun <- function(x, y) approx(x, y, n = n)
    ## need to consider the space
  if(length(y)){
    temp.y <- as.character(values(data)[,as.character(y)])
    if(is.character(temp.y)){
      temp.y <- as.numeric(as.factor((temp.y)))
    }
    if(!all(check.integer(temp.y)))
      stop("geom(bar) require specified 'y', must be integer which indicates the levels")
    values(data)$.levels <- temp.y
  }else{
      values(data)$.levels <- disjointBins(data)
  }
  ## if(!length(y)){  
  ##   values(data)$.levels <- disjointBins(data)
  ## }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    if(df[i,"width"] > 1){
      res.x <- as.integer(inter.fun(c(df[i,"start"], df[i,"end"]), c(0, 0))$x)
    }else{
      res.x <- c(df[i, "start"], df[i, "start"])
    }
    res <- data.frame(.biovizBase.new.x = c(res.x, rev(res.x)))
    N <- nrow(res)
    res$.biovizBase.group <- i
    ## if(!length(y)){      
    res$.biovizBase.level <- c(rep(df[i, ".levels"] - 0.4, N/2),
                               rep(df[i, ".levels"] + 0.4, N/2))
  ## }
    df.extra <- do.call("rbind", lapply(1:N, function(k) df[i,]))
    res <- cbind(res, df.extra)
    res$.int.id <- seq_len(nrow(res))
    res
  })
  res <- do.call("rbind", lst)
  ## if(!length(y))
  .y <- ".biovizBase.level"
  ## else
    ## .y <- y;
  res.gr <- GRanges(res$seqnames, IRanges(start = res$.biovizBase.new.x,
                                          width = 1),
                    strand = res$strand)
  values(res.gr) <- subset(res, select = -c(start, end, width, strand,seqnames))
  seqlengths(res.gr) <- seqlengths(data)
## res.gr[seqnames(res.gr) == "chr22"]
## data.back[seqnames(data.back) == "chr22"]
## res[seqnames(res) == "chr22"]
## res.bk[seqnames(res.bk) == "chr22"]
## debug(gr2circle)
## res.bk <- gr2circle(res.bk, y = 1, radius = radius, width = trackWidth,
##                    direction = direction)  

## res.bk <- gr2newLinear(data.back, space.skip)
  res <- gr2newLinear(res.gr, space.skip)
  res <- gr2circle(res, y = .y, radius = radius, width = trackWidth,
                   direction = direction)  
}

barInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction){
  if(!length(y)){
    temp.y <- values(data)[,as.character(y)]
    if(!all(check.integer(temp.y)))
      stop("geom(bar) require specified 'y', must be integer which indicates the levels")
    values(y)$.levels <- temp.y
  }else{
      values(data)$.levels <- disjointBins(data)
  }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- rep(df[i, "start"], 2)
    res <- data.frame(.biovizBase.new.x = res.x)
    N <- nrow(res)
    res$.biovizBase.group <- i
    res$.biovizBase.level <- c(df[i, ".levels"] - 0.4,
                               df[i, ".levels"] + 0.4)
    df.extra <- rbind(df[i, ], df[i, ])
    res <- cbind(res, df.extra)
    res
  })
  res <- do.call("rbind", lst)
  .y <- ".biovizBase.level"
  res.gr <- GRanges(res$seqnames, IRanges(start = res$.biovizBase.new.x,
                                          width = 1),
                    strand = res$strand)
  values(res.gr) <- subset(res, select = -c(start, end, width, strand,seqnames))
  seqlengths(res.gr) <- seqlengths(data)
  res <- gr2newLinear(res.gr, space.skip)
  res <- gr2circle(res, y = .y, radius = radius, width = trackWidth,
                   direction = direction)  
}

## ok, segment allow user to use a flexible y
segInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction,
                      n = 5){
  ## do the linear interpolatoin first
  inter.fun <- function(x, y) approx(x, y, n = n)  
  if(!length(y)){
    values(data)$.levels <- disjointBins(ranges(data))
  }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- as.integer(inter.fun(c(df[i,"start"], df[i,"end"]), c(0, 0))$x)
    res <- data.frame(.biovizBase.new.x = res.x)
    res$.biovizBase.group <- i
    if(!length(y)){          
    res$.biovizBase.level <- df[i, ".levels"]
  }
    N <- nrow(res)
    df.extra <- do.call("rbind", lapply(1:N, function(k) df[i,]))
    res <- cbind(res, df.extra)
  })
  res <- do.call("rbind", lst)
  if(!length(y))
    .y <- ".biovizBase.level"
  else
    .y <- as.character(y)

  res.gr <- GRanges(res$seqnames, IRanges(start = res$.biovizBase.new.x,
                                          width = 1),
                    strand = res$strand)
  values(res.gr) <- subset(res, select = -c(start, end, width, strand,seqnames))
  seqlengths(res.gr) <- seqlengths(data)
  res <- gr2newLinear(res.gr, space.skip)
  res <- gr2circle(res, y = .y, radius = radius, width = trackWidth,
                   direction = direction)
}

## for a special GRanges
linkInter <- function(data, linked.to, space.skip = 0.1, trackWidth = 10, radius = 10,
                      link.fun = function(x, y, n = 100) bezier(x, y, evaluatio = n),
                      direction = direction){
  if(missing(linked.to))
    stop("linked.to must be provided and be a GRanges")
  obj <- gr2newLinear(data, space.skip)
  obj <- gr2circle(obj, y = 0, radius = radius, width = trackWidth,
                   direction = direction)
  df <- as.data.frame(obj)
  linktodata <- values(data)[,linked.to]
  values(linktodata)$.biovizBase.idx <- 1:length(linktodata)
  ## missing y
  linktodata <- gr2newLinear(linktodata, space.skip)
  ## keep order
  linktodata <- linktodata[order(values(linktodata)$.biovizBase.idx)]
  linktodata <- gr2circle(linktodata, radius = radius,
                          y = 0,
                          width = trackWidth,
                          direction = direction)
  linkdf <- as.data.frame(linktodata)
  extra.df <- subset(df, select = -c(.biovizBase.start, .biovizBase.x, .biovizBase.y))
  linkdf2 <- data.frame(from.x = df$.biovizBase.x,
                        from.y = df$.biovizBase.y,
                        to.x = linkdf$.biovizBase.x,
                        to.y = linkdf$.biovizBase.y)
  linkdf2 <- cbind(linkdf2, extra.df)
  lst <- lapply(1:nrow(linkdf2), function(i){
    ndf <- link.fun(c(linkdf2[i, "from.x"],0,linkdf2[i,"to.x"]),
                    c(linkdf2[i, "from.y"],0,linkdf2[i,"to.y"]))
    ndf <- as.data.frame(do.call("cbind", ndf))
    N <- nrow(ndf)
    ndf$.biovizBase.group <- i
    ## need to past other meta data
    extra.df <- subset(linkdf2[i,], select = -c(from.x, from.y, to.x, to.y))
    extra.df2 <- do.call("rbind", lapply(1:N, function(i) extra.df))
    ndf <- cbind(ndf, extra.df2)
  })
  res <- do.call("rbind", lst)
}




ddplyFun <- function(data, .fun, ..., window = 1e5){
  grl <- split(data, seqnames(data))
  idx <- elementLengths(grl) > 0
  grl <- endoapply(grl[idx], function(gr){
    ## let's make it more works for multiple seqnames
    bks <- seq(1, max(end(gr)), by = window)
    N <- length(bks)
    seqn <- unique(seqnames(gr))
    if(length(seq) > 1)
      stop("Multiple seqnames found")
    if(!N%%2){
      qgr <- GRanges(seqn, IRanges(start = bks[1:(N-1)],
                                   end = bks[2:N]))
    }else{
      bks <- c(bks, max(end(gr)))
      N <- length(bks)
      qgr <- GRanges(seqn, IRanges(start = bks[1:(N-1)],
                                   end = bks[2:N]))
    }
    if(max(end(qgr)) < max(end(gr))){
      qgr <- c(qgr, GRanges(seqn, IRanges(max(end(qgr))+ 1,
                                          max(end(gr)))))
    }
    of <- findOverlaps(gr, qgr, select = "first")
    idx <- !is.na(of)
    df <- as.data.frame(gr[idx])
    df$of <- of[idx]
    resdf <- ddply(df, .(of), .fun, ...)
    idx <- as.numeric(resdf$of)
    ## idx <- idx[!is.na(idx)]
    res <- qgr[idx]
    values(res) <- as.data.frame(resdf[,2])
    res
  })
  res <- unlist(grl[!is.null(grl)])
  names(res) <- NULL
  sort(res)
}



transformDfToGr <- function(data, seqnames = NULL, start = NULL, end = NULL,
                           width = NULL, strand = NULL){


  ## this losing seqinfo....
  colnms <- colnames(data)
  if(is.null(seqnames)){
    if("seqnames" %in% colnms)
      seqnames <- "seqnames"
    else
      stop("Please sepicify which column represent the seqnames")
  }

  if(is.null(start)){
    if("start" %in% colnms)
      start <- "start"
    else
      stop("Please sepicify which column represent the start")
  }

  if(is.null(end)){
    if("end" %in% colnms)
      end <- "end"
    else if(is.null(width)){
      if("width" %in% colnms)
        width <- "width"
      else
        stop("Must provide end or width column names")
    }}
  
  if(is.null(width)){
    if("width" %in% colnms)
        width <- "width"
  }

  if(!is.null(end))
    gr <- GRanges(data[,seqnames], IRanges(start = data[,start], end = data[,end]))
  else
    gr <- GRanges(data[,seqnames], IRanges(start = data[,start], width = data[,width]))

  if(is.null(strand)){
    if("strand" %in% colnms){
      strand <- "strand"
      strand(gr) <- data[,strand]
    }
  }
  values(gr) <- data[,!colnames(data) %in% c(start, end, width, seqnames, strand)]
  gr
}



setMethod("disjointBins", "GRanges", function(x){
  res <- split(x, seqnames(x))
  res <- unlist(lapply(res, function(x){
    disjointBins(ranges(x))
  }))
  unname(res)
})

check.integer <- function(x){
  sapply(x, function(x)
    !length(grep("[^[:digit:]]", as.character(x))))
}

getIdeoGR <- function(gr){
  if(!is(gr, "GenomicRanges"))
    stop("require GenomicRanges")
  if(all(is.na(seqlengths(gr)))){
    message("geom(ideogram) need valid seqlengths information for accurate mapping,
                 now use reduced information as ideogram... ")
    res <- reduce(gr, ignore = TRUE)
    start(res) <- 1
  }else{
    res <- as(seqinfo(gr), "GenomicRanges")
  }
}

## get major/mionr scale, y value, major text?
getScale <- function(gr, unit = NULL, n = 100, type = c("M", "B", "sci")){
  type <- match.arg(type)
  if(is.null(unit)){
    unit <- sum(as.numeric(width(gr)))/n
    unit <- 10^floor(log10(unit))
  }
  ## not like normal scale
  res <- split(gr, seqnames(gr))
  grl <- endoapply(res, function(gr){
    st <- 1
    ed <- end(gr)
    major.pos <- seq(st, ed, by = 5*unit)
    minor.pos <- seq(st, ed, by = unit)
    minor.pos <- setdiff(minor.pos, major.pos)
    if(type == "M"){
      texts <- c(paste(as.character(round(major.pos/1e6, digit = 1)), "M",
                       sep = ""),
               rep("", length(minor.pos)))
    }
    if(type == "B"){
      texts <- c(paste(as.character(round(major.pos/1e9, digit = 1)), "B",
                       sep = ""),
               rep("", length(minor.pos)))
    }
    if(type == "sci"){
    texts <- c(as.character(format(major.pos, scientific = TRUE, digit = 1)),
               rep("", length(minor.pos)))
    }
    GRanges(seqnames(gr),
            IRanges(start = c(major.pos, minor.pos),
                    width = 1),
            type = c(rep("major", length(major.pos)), rep("minor", length(minor.pos))),
            scale.y = c(rep(3, length(major.pos)),
                         rep(1, length(minor.pos))),
            text.major = texts)
  })
  res <- unlist(grl)
  seqlengths(res) <- seqlengths(gr)
  res
}



parseArgsForAes <- function(args){
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    if(length(idx))
      return(eval(args[[idx]]))
    else
      return(list())
  }else{
    return(list())
  }
}

parseArgsForNonAes <- function(args){
  lst <- unlist(lapply(args, function(x) class(eval(x)) != "uneval"))
  args[lst]
}

getNR <- function(x, type = c("NUSE", "RLE"),range = 0, ...){
require(affyPLM)
compute.nuse <- function(which) {
  nuse <- apply(x@weights[[1]][which, ], 2, sum)
  1/sqrt(nuse)
}

type <- match.arg(type)
model <- x@model.description$modelsettings$model

if (type == "NUSE") {
  if (x@model.description$R.model$which.parameter.types[3] == 
      1 & x@model.description$R.model$which.parameter.types[1] == 
      0) {
    grp.rma.se1.median <- apply(se(x), 1, median, 
                                na.rm = TRUE)
    res <- grp.rma.rel.se1.mtx <- sweep(se(x), 1, grp.rma.se1.median, 
                                        FUN = "/")

  }
  else {
    which <- indexProbesProcessed(x)
    ses <- matrix(0, length(which), 4)
    if (x@model.description$R.model$response.variable == 
        1) {
      for (i in 1:length(which)) ses[i, ] <- compute.nuse(which[[i]])
    }
    else {
      stop("Sorry I can't currently impute NUSE values for this PLMset object")
    }
    grp.rma.se1.median <- apply(ses, 1, median)
    res <- grp.rma.rel.se1.mtx <- sweep(ses, 1, grp.rma.se1.median, 
                                        FUN = "/")
    
  }
}
if(type == "RLE"){
 if (x@model.description$R.model$which.parameter.types[3] == 
            1) {
            medianchip <- apply(coefs(x), 1, median)
            res <- sweep(coefs(x), 1, medianchip, FUN = "-")
        }
        else {
            stop("It doesn't appear that a model with sample effects was used.")
        }  
}
res
}



getFormalNames <- function(..., move.dots = TRUE){
  res <- lapply(list(...), function(fun){
    if(is.function(fun)){
      res <- names(formals(fun))
      if(move.dots)
        res <- res[ res != "..."]
      res
    }else{
      stop("arguments passed to getFormalNames must be functions")
    }
  })
  res <- unlist(res)
  res <- res[!duplicated(res)]
  res
}


subsetArgsByFormals <- function(args, ..., move.dots = TRUE){
  .formals <- getFormalNames(..., move.dots = move.dots)
  res <- args[names(args) %in% .formals]
  res
}

getGeomFun <- function(geom){
  match.fun(paste("geom_", geom, sep = ""))
}
getStatFun <- function(stat){
  match.fun(paste("stat_", stat, sep = ""))
}
getDrawFunFromGeomStat <- function(geom, stat){
  ## how about allways start from geom??
  if(!is.null(stat)){
    .fun <- getStatFun(stat)      
  }else{
    .fun <- getGeomFun(geom)
  }
  .fun
}

flatGrl <- function(object, indName = ".grl.name"){
  idx <- togroup(object)
  indName <- ".grl.name"
  gr <- stack(object, indName)
  values(gr) <-   cbind(values(gr), values(object)[idx,,drop = FALSE])
  gr
}
