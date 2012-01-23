## reference:http://groups.google.com/group/ggplot2/browse_thread/thread/72403c6997b79c3b?pli=1
## "link" and "ribbon" requie a special data structure
## 1. we could implement it as GRangesList with no direction specification
## 2. we could use a to.gr as element meta data which is a granges with direction,
## this is also a GRanges object which is general(implemented this first)
setGeneric("layout_circle", function(data,...) standardGeneric("layout_circle"))
setMethod("layout_circle",  "GRanges",
          function(data, ..., geom = c("point", "line", "link", "ribbon","rect",
                                       "segment", "rectangle", "hist", "scale","ideogram",
                                "text"), linked.to,
                          radius = 10, trackWidth = 5, trackBuffer, circle.skip,
                          space.skip = 0.1, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluatio = n)){
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
  ## df <- as.data.frame(data)
  ## y <- eval(aes.lst$y, df)
  ## first do the transformation
  obj <- gr2newLinear(data, space.skip)
  if(geom == "point"){
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_point(data = df, aes) 
  }
  
  if(geom == "line"){
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    p <- geom_path(data = df, aes) 
  }
  if(geom == "segment"){
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
  if(geom == "rectangle"){
    res <- rectInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction)
    df <- as.data.frame(res)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$group <- as.name(".biovizBase.group")    
    aes <- do.call("aes", aes.lst)
    p <- geom_polygon(data = df, aes)
  }
  if(geom == "rectangle"){
    res <- rectInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction)
    df <- as.data.frame(res)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes.lst$group <- as.name(".biovizBase.group")    
    aes <- do.call("aes", aes.lst)
    p <- geom_polygon(data = df, aes)
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
    
  }
  p
})


## first got a linear rearrangement, for like, grand linear view
## only consider granges here
## TODO:
## 1. ranked by chromosome
## 2. if seqlengths exists, use that as boundary, if not, data range * 0.05
## 3. link
## 4. rectangle(##)
## 5. line
## 6. point
gr2newLinear <- function(obj, space.skip = 0.1){
  obj <- sort(obj)
  seqs.l <- seqlengths(obj)
  if(all(!is.na(seqs.l))){
    chr.l <- seqs.l
    seqs.suml <- sum(seqs.l)
  }else{
    ## if no seqlengths are found, use obj range
    chr.l <- max(end(split(obj, as.character(seqnames(obj)))))
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

## interpolate segment or rectangle
## rectangle only use stepping as y
rectInter <- function(data, y, space.skip = 0.1, trackWidth = 10, radius = 10,
                      direction = direction,
                      inter.fun = function(x, y, n = 4) approx(x, y, n = 4)){

  values(data)$.biovizBase.level <- disjointBins(ranges(data))
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- as.integer(inter.fun(c(df[i,"start"], df[i,"end"]), c(0, 0))$x)
    ## colnames(res) <- c(".biovizBase.new.x", ".biovizBase.new.y")
    res <- data.frame(.biovizBase.new.x = c(res.x, rev(res.x)))
    N <- nrow(res)    
    res$.biovizBase.group <- i
    res$.biovizBase.level <- c(rep(df[i, ".levels"] - 0.4, N/2), c(df[i, ".levels"] + 0.4, N/2))

    df.extra <- do.call("rbind", lapply(1:N, function(k) df[i,]))
    res <- cbind(res, df.extra)
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
                      inter.fun = function(x, y, n = 3) approx(x, y, n = n)){
  ## do the linear interpolatoin first
  if(!length(y)){
    values(data)$.biovizBase.level <- disjointBins(ranges(data))
  }
  df <- as.data.frame(data)
  lst <- lapply(1:nrow(df), function(i){
    res.x <- as.integer(inter.fun(c(df[i,"start"], df[i,"end"]), c(0, 0))$x)
    ## colnames(res) <- c(".biovizBase.new.x", ".biovizBase.new.y")
    res <- data.frame(.biovizBase.new.x = res.x)
    res$.biovizBase.group <- i
    res$.biovizBase.level <- df[i, ".levels"]
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


