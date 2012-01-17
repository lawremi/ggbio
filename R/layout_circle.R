## first got a linear rearrangement, for like, grand linear view
## only consider granges here
## TODO:
## ranked by chromosome
## if seqlengths exists, use that as boundary, if not, data range * 0.05
gr2newLinear <- function(obj, space.skip = 0.1){
  obj <- sort(obj)
  seqs.l <- seqlengths(obj)  
  if(all(!is.na(seqs.l))){
    chr.l <- seqs.l
    seqs.suml <- sum(seqs.l)
  }else{
    ## if no seqlengths are found, use obj range
    chr.l <- max(end(split(obj, seqnames(obj))))
    seqs.suml <- sum(chr.l)
  }
  space.skip <- space.skip * seqs.suml
  skps <- space.skip * ((1:length(seqs.l)))
  names(skps) <- names(seqlengths(obj))
  chr.l <- cumsum(chr.l)
  chr.l2 <- c(0, chr.l[-length(chr.l)])
  names(chr.l2) <- names(chr.l)
  sts.new <- start(obj) + skps[as.character(seqnames(obj))] +
    chr.l2[as.character(seqnames(obj))] 
  values(obj)$.biovizBase.start <- sts.new
  obj
}


## then need a transformation to circlular view
## data is a GRanges object
gr2circle <- function(obj, x = ".biovizBase.start", y,
                      radius = 10, width = 10, direction = c("clockwise", "anticlockwise")){
  ## if(missing(x))
  ##   stop("x is missing")
  if(missing(y)){
    stop("y is missing")
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
  obj
}

## reference:http://groups.google.com/group/ggplot2/browse_thread/thread/72403c6997b79c3b?pli=1

blank_opts <- function(size=12) 
{ 
  o = list(axis.line=theme_blank(), 
    axis.text.x=theme_blank(), 
    axis.text.y=theme_blank(), 
    axis.ticks=theme_blank(), 
    axis.ticks.length=unit(0.3, "lines"), 
    axis.ticks.margin=unit(0.5, "lines"), 
    axis.title.x=theme_blank(), 
    axis.title.y=theme_blank(), 
    legend.background=theme_rect(fill="white", colour=NA), 
    legend.key=theme_rect(colour="white"), 
    legend.key.size=unit(1.2, "lines"), 
    legend.position="right", 
    legend.text=theme_text(size=size*0.8), 
    legend.title=theme_text(size=size*0.8, face="bold", 
      hjust=0), 
    panel.background=theme_blank(), 
    panel.border=theme_blank(), 
    panel.grid.major=theme_blank(), 
    panel.grid.minor=theme_blank(), 
    panel.margin=unit(0, "lines"), 
    plot.background=theme_blank(), 
    plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"), 
    plot.title=theme_text(size=size*1.2), 
    strip.background=theme_rect(fill="grey90", 
      colour="grey50"), 
    strip.text.x=theme_text(size=size*0.8), 
    strip.text.y=theme_text(size=size*0.8, angle=-90)) 
  return(structure(o, class="options"))}   

## "link" and "ribbon" requie a special data structure
## 1. we could implement it as GRangesList with no direction specification
## 2. we could use a to.gr as element meta data which is a granges with direction,
## this is also a GRanges object which is general(implemented this first)
layout_circle <- function(data, ..., geom = c("point", "line", "link", "ribbon",
                                       "segment", "rect", "hist"), linked.to,
                          radius = 10, trackWidth = 5, trackBuffer, circle.skip,
                          space.skip = 0.1, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluatio = n)){
  geom <- match.arg(geom)
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  ## df <- as.data.frame(data)
  ## y <- eval(aes.lst$y, df)
  ## first do the transformation
  obj <- gr2newLinear(data, space.skip)
  if("point" %in% geom){
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_point(data = df, aes) 
  }
  if("line" %in% geom){
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = df, aes) 
  }
  if("rect" %in% geom){
    
  }
  ## if("segment" %in% geom){
    
  ## }
  if("link" %in% geom){
    res <- linkInter(data, space.skip = space.skip, linked.to = linked.to,
                     link.fun = link.fun, trackWidth = trackWidth, radius = radius,
                     direction = direction)
    aes.lst$y <- as.name("y")
    aes.lst$x <- as.name("x")
    aes.lst$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = res, aes)
  }
  if("ribbon" %in% geom){
    
  }
  p
}
## interpolate segment or rectangle
rectInter <- function(data, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction,
                      inter.fun = function(x, y, n = 10) approx(x, y, n = 10)){
  ## do the linear interpolatoin first
  
  ## then add new data
  
}

segInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction,
                      inter.fun = function(x, y, n = 10) approx(x, y, n = 10)){
  ## do the linear interpolatoin first
  if(missing(y))
    disjointBins(gr)
  apropos("disjoint")
  data <- gr3
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res <- as.data.frame(do.call("cbind", inter.fun(c(df[i,"start"], df[i,"end"]))))
    colnames(res) <- c(".biovizBase.new.x", ".biovizBase.new.y")    
    res$.biovizBase.group <- i
    N <- nrow(res)
    df.extra <- do.call("rbind", lapply(1:N, function(i) df))
    res <- cbind(res, df.extra)
  })
  res <- do.call("rbind", lst)
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

