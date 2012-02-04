g## reference:http://groups.google.com/group/ggplot2/browse_thread/thread/72403c6997b79c3b?pli=1
## "link" and "ribbon" requie a special data structure
## 1. we could implement it as GRangesList with no direction specification
## 2. we could use a to.gr as element meta data which is a granges with direction,
## this is also a GRanges object which is general(implemented this first)

setMethod("disjointBins", "GRanges", function(x){
  res <- split(x, seqnames(x))
  res <- unlist(lapply(res, function(x){
    disjointBins(ranges(x))
  }))
  unname(res)
})


getIdeoGR <- function(gr){
  if(!is(gr, "GenomicRanges"))
    stop("require GenomicRanges")
  if(all(is.na(seqlengths(gr)))){
    warning("geom(ideogram) need valid seqlengths information for accurate mapping,
                 now use reduced information as ideogram... ")
    res <- reduce(gr, ignore = TRUE)
    start(res) <- 1
    return(res)
  }else{
    return(as(seqinfo(gr), "GenomicRanges"))
  }
}

## get major/mionr scale, y value, major text?
getScale <- function(gr, unit, n = 100){
  if(missing(unit)){
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
    GRanges(seqnames(gr),
            IRanges(start = c(major.pos, minor.pos),
                    width = 1),
            type = c(rep("major", length(major.pos)), rep("minor", length(minor.pos))),
            scale.y = c(rep(2, length(major.pos)),
                         rep(1, length(minor.pos))),
            text.major = c(as.character(format(major.pos, scientific = TRUE, digit = 1)),
              rep("", length(minor.pos))))
  })
  res <- unlist(grl)
  seqlengths(res) <- seqlengths(gr)
  res
}


setGeneric("layout_circle", function(data,...) standardGeneric("layout_circle"))
setMethod("layout_circle",  "GRanges",
          function(data, ..., geom = c("point", "line", "link", "ribbon","rect", "bar",
                                       "segment", "rectangle", "hist", "scale","ideogram",
                                "text"), linked.to,
                          radius = 10, trackWidth = 5, trackBuffer, circle.skip,
                          space.skip = 0.1, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluatio = n),
                   rect.inter.n = 5, rank,
                   scale.n = 60){
            
  geom <- match.arg(geom)
  if(geom == "rect")
    geom <- "rectangle"
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }

  ## rank
  if(!missing(rank)){
    radius <- radius + rank * trackWidth
  }
  ## idoegram parse seqlengths
  if(geom == "ideogram"){
    data <- getIdeoGR(data)
    res <- rectInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n)
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$group <- as.name(".biovizBase.group")
    if("fill" %in% names(aes.lst)){
      if(!"color" %in% names(aes.lst)){
        aes.lst$color <- aes.lst$fill
      }
    }
    aes <- do.call("aes", aes.lst)
    if(!"color" %in% names(aes.lst)){
      col <- I("black")
      p <- geom_polygon(data = df, aes, color = col)
    }else{
      p <- geom_polygon(data = df, aes)
    }
    ## p <- geom_polygon(data = df, aes)    
  }

  if(geom == "text"){
    obj <- gr2newLinear(data, space.skip)
    if("label" %in% names(aes.lst)){
      lbs <- as.character(aes.lst$label)
      if(!lbs %in% c(colnames(values(data)),"start", "end", "seqnames","width"))
        stop("label must be one of column names")
    }else{
      stop("missing label argument in aes()")
    }
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    ## compute angle
    if("angle" %in% names(aes.lst)){
      ags <- eval(aes.lst$angle, data)
      ags <- 90 - values(obj)$.biovizBase.angle * 180 / pi + ags
      values(obj)$.processed.angle <- ags
      aes.lst$angle <- as.name(".processed.angle")      
    }else{
      ags <- 90 - values(obj)$.biovizBase.angle * 180/pi 
      values(obj)$.processed.angle <- ags
      aes.lst$angle <- as.name(".processed.angle")      
    }
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_text(data = df, aes)    
  }
  
  if(geom == "point"){
    obj <- gr2newLinear(data, space.skip)    
    if(!"y" %in% names(aes.lst))
      stop("y is missing in aes()")
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_point(data = df, aes) 
  }
  
  if(geom == "line"){
    if(!"y" %in% names(aes.lst))
      stop("y is missing in aes()")
    obj <- gr2newLinear(data, space.skip)    
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = df, aes) 
  }
  
  if(geom == "segment"){
    ## TODO
    res <- segInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                      direction = direction)
    df <- as.data.frame(res)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$group <- as.name(".biovizBase.group")    
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = df, aes)
  }
  
  if(geom == "scale"){
    ## like ideogram
    res <- getIdeoGR(data)
    res <- getScale(res, n = scale.n)
    values(res)$.biovizBase.group <- seq_len(length(res))
    res0 <- res
    values(res0)$scale.y <- 0
    values(res0)$.biovizBase.group <- seq_len(length(res0))
    res <- c(res, res0)
    res <- gr2newLinear(res, space.skip)    
    res <- gr2circle(res, y = "scale.y", radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group)
    df <- df[idx, ]
    N <- nrow(df)
    res <- df[seq(1, N-1, by = 2),]
    res[,c(".biovizBase.xend", ".biovizBase.yend")] <-
      df[seq(2, N, by = 2), c(".biovizBase.x", ".biovizBase.y")]
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$yend <- as.name(".biovizBase.yend")
    aes.lst$xend <- as.name(".biovizBase.xend")
    aes <- do.call("aes", aes.lst)
    p <- geom_segment(data = res, aes)
  }

  
  if(geom == "rectangle"){
    res <- rectInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n)
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes.lst.p <- aes.lst
    aes.lst.p$y <- as.name(".biovizBase.y")
    aes.lst.p$x <- as.name(".biovizBase.x")
    aes.lst.p$group <- as.name(".biovizBase.group")
    ## TODO: fix color
    if("fill" %in% names(aes.lst.p)){
      if(!"color" %in% names(aes.lst.p)){
        aes.lst.p$color <- aes.lst.p$fill
      }
    }
    aes.p <- do.call("aes", aes.lst.p)    
    if(!"color" %in% names(aes.lst.p)){
      col <- I("black")
      p <- geom_polygon(data = df, aes.p, color = col)
    }else{
      p <- geom_polygon(data = df, aes.p)
    }

  }
  
  if(geom == "bar"){
    res <- barInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction)
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group)
    df <- df[idx, ]
    N <- nrow(df)
    res <- df[seq(1, N-1, by = 2),]
    res[,c(".biovizBase.xend", ".biovizBase.yend")] <-
      df[seq(2, N, by = 2), c(".biovizBase.x", ".biovizBase.y")]
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$yend <- as.name(".biovizBase.yend")
    aes.lst$xend <- as.name(".biovizBase.xend")
    aes <- do.call("aes", aes.lst)
      p <- geom_segment(data = res, aes)
    p
  }
  
  if(geom == "link"){
    res <- linkInter(data, space.skip = space.skip, linked.to = linked.to,
                     link.fun = link.fun, trackWidth = trackWidth, radius = radius,
                     direction = direction)
    aes.lst$y <- as.name("y")
    aes.lst$x <- as.name("x")
    aes.lst$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = res, aes)
  }
  if(geom == "ribbon"){
    stop("geom(ribbon) is not implemented yet")
  }
  p 
})


## first got a linear rearrangement, for like, grand linear view
## only consider granges here
## TODO:
## 1. ranked by chromosome
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
  angle.unit <- pi* 2/ max(temp.x)
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
  inter.fun <- function(x, y) approx(x, y, n = n)
    ## need to consider the space
  if(!length(y)){  
    values(data)$.levels <- disjointBins(data)
  }
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
    if(!length(y)){      
    res$.biovizBase.level <- c(rep(df[i, ".levels"] - 0.4, N/2),
                               rep(df[i, ".levels"] + 0.4, N/2))
  }
    df.extra <- do.call("rbind", lapply(1:N, function(k) df[i,]))
    res <- cbind(res, df.extra)
    res$.int.id <- seq_len(nrow(res))
    res
  })
  res <- do.call("rbind", lst)
  if(!length(y))
    .y <- ".biovizBase.level"
  else
    .y <- y

  res.gr <- GRanges(res$seqnames, IRanges(start = res$.biovizBase.new.x,
                                          width = 1),
                    strand = res$strand)
  values(res.gr) <- subset(res, select = -c(start, end, width, strand,seqnames))
  seqlengths(res.gr) <- seqlengths(data)
  res <- gr2newLinear(res.gr, space.skip)
  res <- gr2circle(res, y = .y, radius = radius, width = trackWidth,
                   direction = direction)  
}

barInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction){
  values(data)$.levels <- disjointBins(data)
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
  if(!length(y))
    .y <- ".biovizBase.level"
  else
    .y <- y
  res.gr <- GRanges(res$seqnames, IRanges(start = res$.biovizBase.new.x,
                                          width = 1),
                    strand = res$strand)
  values(res.gr) <- subset(res, select = -c(start, end, width, strand,seqnames))
  seqlengths(res.gr) <- seqlengths(data)
  res <- gr2newLinear(res.gr, space.skip)
plot(values(res)$.biovizBase.start, values(res)$scale.y)
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



