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


  if(!"stepping" %in% colnames(values(data)))
    stop("stepping is not in data")
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
          values(gr)$stepping <- unique(values(x)$stepping)
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
  })

  grl <- grl[!unlist(lapply(grl, is.null))]
  ## grl
  if(length(grl)){
    ## res <- unlist(do.call(GRangesList, grl))a
    res <- unlist(do.call(GRangesList, do.call(c, grl)))
    values(res)$type <- "gaps"
    res <- resize(res, width = width(res) + 2, fix = "center")
  }else{
    res <- GRanges()
  }
  res
}

## suppose we have freq?
getModelRange <- function(data, group.name){
  seqs <- unique(as.character(seqnames(data)))
  ir <- unlist(range(ranges(split(data, values(data)[,group.name]),
                            ignore.strand = TRUE)))
  ## freqs <- values(data)$freq[match(names(ir), values(data)[,group.name])]
  .lvs <- values(data)$stepping[match(names(ir), values(data)[,group.name])]
  ## with levels
  gr <- GRanges(seqs, ir, stepping = .lvs)
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
                      radius = 10, width = 10, direction = c("clockwise", "anticlockwise"),
                      mul = 0.05){

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
  
  temp.y.r <- expand_range(range(temp.y), mul = mul)
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
                      n = 5, mul = 0.05){

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
    values(data)$stepping <- temp.y
  }else{
      values(data)$stepping <- disjointBins(data)
  }
  ## if(!length(y)){  
  ##   values(data)$stepping <- disjointBins(data)
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
    res$.biovizBase.level <- c(rep(df[i, "stepping"] - 0.4, N/2),
                               rep(df[i, "stepping"] + 0.4, N/2))
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
                   direction = direction, mul = mul)  
}

barInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction){
  if(!length(y)){
    temp.y <- values(data)[,as.character(y)]
    if(!all(check.integer(temp.y)))
      stop("geom(bar) require specified 'y', must be integer which indicates the levels")
    values(y)$stepping <- temp.y
  }else{
      values(data)$stepping <- disjointBins(data)
  }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- rep(df[i, "start"], 2)
    res <- data.frame(.biovizBase.new.x = res.x)
    N <- nrow(res)
    res$.biovizBase.group <- i
    res$.biovizBase.level <- c(df[i, "stepping"] - 0.4,
                               df[i, "stepping"] + 0.4)
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
    values(data)$stepping <- disjointBins(ranges(data))
  }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- as.integer(inter.fun(c(df[i,"start"], df[i,"end"]), c(0, 0))$x)
    res <- data.frame(.biovizBase.new.x = res.x)
    res$.biovizBase.group <- i
    if(!length(y)){          
    res$.biovizBase.level <- df[i, "stepping"]
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
                      link.fun = function(x, y, n = 100) bezier(x, y, evaluation = n),
                      direction = direction){
  if(missing(linked.to))
    stop("linked.to must be provided and be a GRanges")
  ## trace id
  N <- length(data)
  values(data)$.biovizBase.idx <- 1:N
  values(values(data)[, linked.to])$.biovizBase.idx <- 1:N
  obj <- gr2newLinear(data, space.skip)
  obj <- gr2circle(obj, y = 0, radius = radius, width = trackWidth,
                   direction = direction)
  obj <- obj[order(values(obj)$.biovizBase.idx)]  
  df <- as.data.frame(obj)
  linktodata <- values(data)[,linked.to]
  ## values(linktodata)$.biovizBase.idx <- 1:length(linktodata)
  ## missing y
  linktodata <- gr2newLinear(linktodata, space.skip)
  ## keep order
  linktodata <- gr2circle(linktodata, radius = radius,
                          y = 0,
                          width = trackWidth,
                          direction = direction)
  linktodata <- linktodata[order(values(linktodata)$.biovizBase.idx)]  
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
    resdf <- plyr::ddply(df, .(of), .fun, ...)
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
    warning("geom(ideogram) need valid seqlengths information for accurate mapping,
                 now use reduced information as ideogram... ")
    res <- reduce(gr, ignore = TRUE)
    start(res) <- 1
    res
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
      texts <- c(paste(as.character(round(major.pos/1e6, digits = 1)), "M",
                       sep = ""),
               rep("", length(minor.pos)))
    }
    if(type == "B"){
      texts <- c(paste(as.character(round(major.pos/1e9, digits = 1)), "B",
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
  aes.lst <- unlist(lapply(args, function(x){
    class(eval(x, parent.frame())) == "uneval"
  }))
  if(length(aes.lst)){
    idx <- base::which(aes.lst)
    if(length(idx))
      res <- eval(args[[idx]])
    else
      res <- list()
  }else{
    res <- list()
  }
  idx <- ggplot2:::is_calculated_aes(res)
  res[idx] <- ggplot2:::strip_dots(res[idx])
  res
}

parseArgsForNonAes <- function(args){
  lst <- unlist(lapply(args, function(x) class(eval(x, parent.frame())) != "uneval"))
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

flatGrl <- function(object, indName = "grl_name"){
  idx <- togroup(object)
  gr <- stack(object, indName)
  values(gr) <-   cbind(values(gr), values(object)[idx,,drop = FALSE])
  gr
}


.changeStrandColor <- function(p, args, fill = TRUE){
  strandColor <- getOption("biovizBase")$strandColor
  isStrand.color <- FALSE
  isStrand.fill <- FALSE
  ## default with no color
  idx <- c("color", "colour") %in% names(args)
  if((any(idx))){
    nms <- c("color", "colour")[idx][1]
    if(as.character(args[[nms]]) == "strand")
      isStrand.color <- TRUE
  }
  if(("fill" %in% names(args))){
    if(as.character(args$fill) == "strand")
      isStrand.fill <- TRUE
  }
  if(isStrand.color)
    p <- c(list(p), list(scale_color_manual(values = strandColor)))
  if(fill){
    if(isStrand.fill)
      p <- c(p, list(scale_fill_manual(values = strandColor)))
  }
  p
}

## functions downbelow from workshop by Michael Lawrence
## will be implmented by into his package, so I will replace this function later
splicefun <- function(files, txdb, which, id,  xlim, txdb.chr.pre = character(), weight = 1){
  elementGaps <- function(x) {
    x_flat <- unlist(x, use.names = FALSE)
    egaps <- gaps(ranges(x))
    first_segment <- start(PartitioningByWidth(x))
    sn <- seqnames(x_flat)[first_segment][togroup(egaps)]
    strand <- strand(x_flat)[first_segment][togroup(egaps)]
    relist(GRanges(sn, unlist(egaps, use.names = FALSE), 
                   strand, seqlengths = seqlengths(x)), 
           egaps)
  }

  pairReadRanges <- function(reads) {
    pairs <- split(unlist(reads, use.names=FALSE),
                   factor(names(reads)[togroup(reads)], 
                          unique(names(reads))))
    metadata(pairs) <- metadata(reads)
    xs <- values(reads)$XS
    has_xs <- !is.na(xs)
    pair_xs <- setNames(rep.int(NA, length(pairs)), 
                        names(pairs))
    pair_xs[names(reads)[has_xs]] <- xs[has_xs]
    values(pairs)$XS <- unname(pair_xs)
    pairs
  }  

  strandFromXS <- function(pairs) {
    xs <- values(pairs)$XS
    strand <- ifelse(!is.na(xs) & xs != "?", xs, "*")
    strand(pairs) <- relist(Rle(strand, elementLengths(pairs)), 
                            pairs)
    pairs
  }

  gr2key <- function(x) {
    paste(seqnames(x), start(x), end(x), strand(x), 
          sep = ":")
  }

  key2gr <- function(x, ...) {
    key_mat <- matrix(unlist(strsplit(x, ":", fixed=TRUE)), 
                      nrow = 4)
    GRanges(key_mat[1,],
            IRanges(as.integer(key_mat[2,]), 
                    as.integer(key_mat[3,])),
            key_mat[4,], ...)
  }
  findIsoformOverlaps <- function(pairs) {
    splices <- values(pairs)$splices
    hits <- findOverlaps(pairs, tx)
    hit_pairs <- ranges(pairs)[queryHits(hits)]
    hit_splices <- ranges(splices)[queryHits(hits)]
    hit_tx <- ranges(tx)[subjectHits(hits)]
    read_within <- 
      elementLengths(setdiff(hit_pairs, hit_tx)) == 0L
    tx_within <- 
      elementLengths(intersect(hit_tx, hit_splices)) == 0L
    compatible <- read_within & tx_within
    compat_hits <- hits[compatible]
    reads_unique <- tabulate(queryHits(compat_hits), 
                             queryLength(compat_hits)) == 1L
    unique <- logical(length(hits))
    unique[compatible] <- reads_unique[queryHits(compat_hits)]
    strand_specific <- 
      all(strand(pairs) != "*")[queryHits(hits)]
    values(hits) <- DataFrame(strand_specific,
                              compatible,
                              unique)
    list(hits = hits,
         compatible = compatible,
         strand_specific = strand_specific)
  }
  countIsoformHits <- function(hits, compatible, strand_specific) {
    countByTx <- function(x) {
      tabulate(subjectHits(hits)[x], subjectLength(hits))
    }
    compatible_strand <- 
      countByTx(with(values(hits), 
                     compatible & strand_specific))
    counts <- DataFrame(compatible_strand,
                        lapply(values(hits)[-1], countByTx))
    counts
  }

  summarizeSplices <- function(reads) {
    splices <- values(reads)$splices
    splices_flat <- unlist(splices, use.names = FALSE)
    if(length(splices_flat)){    
    splice_table <- table(gr2key(splices_flat))
    splice_summary <- 
      key2gr(names(splice_table), 
             score = as.integer(splice_table),
             novel = !names(splice_table) %in% tx_keys,
             seqlengths = seqlengths(splices))
  }else{
    splice_summary <- GRanges()
  }
    splice_summary
  }

  getUniqueReads <- function(reads, hits) {
    sel <- values(hits)$unique & 
    subjectHits(hits) %in% c(1, 4)
    reads[unique(queryHits(hits)[sel])]
  }

  message("Parsing gene structure from txdb...")
  if(!missing(id)){
  aldoa_gr <- exons(txdb, vals = list(gene_id = id),
                    columns = c("tx_id", "gene_id"))
  aldoa_gr <- keepSeqlevels(aldoa_gr, unique(as.character(seqnames(aldoa_gr))))
  ## FIXME later
  nms <- as.character(names(seqlengths(aldoa_gr)))
  nms.new <- paste(txdb.chr.pre, nms, sep = "")
  names(nms.new) <- nms
  aldoa_gr <- renameSeqlevels(aldoa_gr, nms.new)
  aldoa_range <- range(aldoa_gr)
  wh <- aldoa_range  
  aldoa_vals <- values(aldoa_gr)
  tx <- multisplit(aldoa_gr, aldoa_vals$tx_id)
  tx_to_val <- match(names(tx), unlist(aldoa_vals$tx_id))
  values(tx)$gene_id <- 
    rep(unlist(aldoa_vals$gene_id), 
        elementLengths(aldoa_vals$tx_id))[tx_to_val]
  values(tx)$tx_id <- names(tx)
}else if(!missing(which) & is(which, "GRanges")){
    isActiveSeq(txdb)[seqlevels(txdb)] <- FALSE
    seqnms <- as.character(unique(seqnames(which)))
    seqnms <- gsub(txdb.chr.pre, "", seqnms)
    isActiveSeq(txdb)[seqnms] <- TRUE
    ## aldoa_gr <- exons(txdb, columns = c("tx_id", "gene_id"))
    ## aldoa_gr <- keepSeqlevels(aldoa_gr, unique(as.character(seqnames(aldoa_gr))))
    ## nms <- as.character(names(seqlengths(aldoa_gr)))
    ## nms.new <- paste(txdb.chr.pre, nms, sep = "")
    ## names(nms.new) <- nms
    ## aldoa_gr <- renameSeqlevels(aldoa_gr, nms.new)
    ## aldoa_gr <- subsetByOverlaps(aldoa_gr, which)
    ## aldoa_range <- range(aldoa_gr)
    aldoa_range <- which
    exons_grl <- exonsBy(txdb)
    gr.l <- stack(exons_grl, ".sample")
    gr.l <- keepSeqlevels(gr.l, unique(as.character(seqnames(gr.l))))
    nms <- as.character(names(seqlengths(gr.l)))
    nms.new <- paste(txdb.chr.pre, nms, sep = "")
    names(nms.new) <- nms
    gr.l <- renameSeqlevels(gr.l, nms.new)
      exons_grl <- split(gr.l, values(gr.l)$.sample)
     ans <- subsetByOverlaps(exons_grl, which)
    values(ans)$tx_id <- names(ans)
    tx_gr <- transcripts(txdb, columns = c("tx_id", "gene_id"))
    values(ans)$gene_id <- 
      drop(values(tx_gr)$gene_id)[match(names(ans), 
                                        values(tx_gr)$tx_id)]
    tx <- ans
    ## wh <- aldoa_range
    ## aldoa_vals <- values(aldoa_gr)
    ## tx <- multisplit(aldoa_gr, aldoa_vals$tx_id)
    ## tx_to_val <- match(names(tx), unlist(aldoa_vals$tx_id))
    ## values(tx)$gene_id <- 
    ##   rep(unlist(aldoa_vals$gene_id), 
    ##       elementLengths(aldoa_vals$tx_id))[tx_to_val]
    ## values(tx)$tx_id <- names(tx)
}

  message("Parsing bam files")
  bamFiles <- Rsamtools::BamFileList(files)

  message("Analysing...")
  introns <- elementGaps(tx)
  introns_flat <- unlist(introns, use.names = FALSE)
  tx_keys <- gr2key(introns_flat)

  readReadRanges <- function(bam) {
    param <- ScanBamParam(tag = "XS", which = aldoa_range)
    ga <- readGappedAlignments(path(bam), 
                               use.names = TRUE, 
                               param = param)
    reads <- grglist(ga)
    metadata(reads)$bamfile <- bam
    splices <- elementGaps(reads)
    values(splices)$XS <- values(reads)$XS
    pairs <- pairReadRanges(reads)
    pairs <- strandFromXS(pairs)
    splices <- pairReadRanges(splices)
    splices <- strandFromXS(splices)
    values(pairs)$splices <- splices
    pairs
  }


  N <- length(bamFiles)
  nms <- names(bamFiles)
  lst_hits <- lst_counts <- lst_splices <- lst_rr <- list()
  message("parsing hits/counts/splcies")
  for(i in 1:N){
    bf <- bamFiles[[i]]
    rr <- readReadRanges(bf)
    lst_rr <- c(lst_rr, rr)
    hits.lst <- findIsoformOverlaps(rr)
    lst_hits <- c(lst_hits, hits.lst$hits)
    lst_counts <- c(lst_counts, countIsoformHits(hits.lst$hits, hits.lst$compatible, hits.lst$strand_specific))
    lst_splices <- c(lst_splices, summarizeSplices(rr))
  }
  names(lst_hits) <- nms
  names(lst_counts) <- nms
  names(lst_splices) <- nms
  names(lst_rr) <- nms
  ## normal <- readReadRanges(bamFiles)
  ## tumor <- readReadRanges(bamFiles$tumor)
  
  ## normal_hits <- findIsoformOverlaps(normal)
  ## normal_counts <- countIsoformHits(normal_hits)
  ## normal_splices <- summarizeSplices(normal)

  ## tumor_hits <- findIsoformOverlaps(tumor)
  ## tumor_counts <- countIsoformHits(tumor_hits)
  ## tumor_splices <- summarizeSplices(tumor)


###################################################
### code chunk number 39: combine-samples
###################################################
  assays <- do.call(mapply, c(list(cbind), lst_counts, list(SIMPLIFY = FALSE)))
  ## assays <- mapply(cbind, normal_counts, SIMPLIFY = FALSE)
  ## assays <- mapply(cbind, normal_counts, tumor_counts, 
  ##                  SIMPLIFY = FALSE)
  colData <- DataFrame(tumorStatus =  names(bamFiles))
  rownames(colData) <- colData$tumorStatus
  se <- SummarizedExperiment(assays, tx, colData)

###################################################
### code chunk number 40: order-se
###################################################
  uc <- assay(se, "unique")
  uc_ord <- order(rowSums(uc), decreasing = TRUE)
  uc_top <- uc[head(uc_ord, 2),]
  ## fisher.test(uc_top)$estimate

###################################################
### code chunk number 41: get-unique-reads
###################################################
  lst_uniq <- lapply(1:N, function(i){
    getUniqueReads(lst_rr[[i]], lst_hits[[i]])
  })
  names(lst_uniq) <- nms
  ## normal_uniq <- getUniqueReads(normal, normal_hits)
  ## tumor_uniq <- getUniqueReads(tumor, tumor_hits)
  both_uniq <- do.call(mstack, lapply(lst_uniq, unlist))

###################################################
### code chunk number 42: combine-splices
###################################################
  lst_uniq_splices <- lapply(lst_uniq, summarizeSplices)
  names(lst_uniq_splices) <- nms
  uniq_splices <- do.call(mstack, lst_uniq_splices)
  all_splices <- do.call(mstack, lst_splices)
  novel_splices <- all_splices[values(all_splices)$novel]
  ## novel_splices <-  all_splices[values(all_splices)$novel &
  ##                               values(all_splices)$score == 9]
  ## novel_splices <-  all_splices[values(all_splices)$novel &
  ##                               values(all_splices)$score == 9]
  ## uniq_novel_splices <- c(uniq_splices, novel_splices)
  ## uniq_novel_splices <- c(uniq_splices, all_splices)
  uniq_novel_splices <- c(uniq_splices, novel_splices)  
  both_uniq <- keepSeqlevels(both_uniq, unique(as.character(seqnames(both_uniq))))
  .wt <- max(width(uniq_novel_splices))/max(coverage(both_uniq)) * weight
  .wt <- as.numeric(.wt)
  aes.res <- do.call(aes,list(size = substitute(score), 
                              height = substitute(width/.wt, list(.wt = .wt)),
                              color = substitute(novel)))

  message("Constructing splicing graphics....")
  p.novel <- do.call(geom_arch, c(list(data = uniq_novel_splices,
                                    ylab = "coverage",
                                       rect.height = 0), list(aes.res)))
  p.s <- ggplot() + p.novel + do.call(stat_coverage, list(data = both_uniq, facets = name~.))
  message("Constructing gene model....")
  if(length(txdb.chr.pre))
    wh <- GRanges(gsub(txdb.chr.pre, "", as.character(seqnames(wh))),
                  ranges(wh))
  ## browser()
  tx_un <- stack(tx, ".sample")
  tx_cur <- keepSeqlevels(tx_un, unique(as.character(seqnames(tx_un))))
  tx_cur <- tx_cur[, setdiff(colnames(values(tx_cur)), c("tx_id", "gene_id"))]
  tx_cur <- split(tx_cur, values(tx_cur)$.sample)
  tx_track <- do.call(autoplot, list(object = tx_cur, geom = "alignment", ylab = ""))
  ## tx_track <- autoplot(tx_16, geom = "alignment", ylab = "")
  ## tx_track <- do.call(autoplot, list(object = txdb, which = wh))
  if(missing(xlim))
    xlim <- c(start(wh), end(wh))
  tracks(p.s, tx_track, xlim  = xlim, heights = c(3, 1))
}
