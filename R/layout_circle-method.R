## reference:http://groups.google.com/group/ggplot2/browse_thread/thread/72403c6997b79c3b?pli=1
## "link" and "ribbon" requie a special data structure
## 1. we could implement it as GRangesList with no direction specification
## 2. we could use a to.gr as element meta data which is a granges with direction,
## this is also a GRanges object which is general(implemented this first)
setGeneric("layout_circle", function(data,...) standardGeneric("layout_circle"))
setMethod("layout_circle",  "GRanges",
          function(data, ..., geom = c("point", "line", "link", "ribbon","rect", "bar",
                                       "segment", "rectangle", "hist", "scale", 
                                "ideogram", "text"), linked.to,
                          radius = 10, trackWidth = 5, trackBuffer, circle.skip,
                          space.skip = 0.03, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluation = n),
                   rect.inter.n = 5, rank, 
                   scale.n = 60, scale.unit = NULL, scale.type = c("M", "B", "sci")){


  dots <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  scale.type <- match.arg(scale.type)
  geom <- match.arg(geom)
  if(geom == "rect")
    geom <- "rectangle"
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  args.extra <- args[unlist(lapply(args, function(x) class(eval(x)) != "uneval"))]
  if(length(aes.lst)){
    idx <- which(aes.lst)
    if(length(idx))
      aes.lst <- eval(args[[idx]])
    else
      aes.lst <- list()
  }else{
    aes.lst <- list()
  }


  ## rank
  if(!missing(rank)){
    radius <- radius + rank * trackWidth
  }
  ## idoegram parse seqlengths
  if(geom == "ideogram"){
    ## data.back <- data
    data <- getIdeoGR(data)
    res <- rectInter(data, y = as.character(aes.lst$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n)
    
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]

    ## aes.lst.text <- aes.lst
    ## aes.lst.text$y <- as.name(".biovizBase.y")
    ## aes.lst.text$x <- as.name(".biovizBase.x")
    
    aes.lst <- aes.lst[names(aes.lst) != "label"]
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
      args.extra$color <- col
    }

    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)
  }

  if(geom == "text"){

    if("label" %in% names(aes.lst)){
      lbs <- as.character(aes.lst$label)
      if(!lbs %in% c(colnames(values(data)),"start", "end", "seqnames","width"))
        stop("label must be one of column names")
    }else{
      stop("missing label argument in aes()")
    }
    obj <- gr2newLinear(data, space.skip)    
    obj <- gr2circle(obj, y = as.character(aes.lst$y), radius= radius, width = trackWidth,
                     direction = direction)
    ## compute angle
    if("angle" %in% names(aes.lst)){
      ags <- eval(aes.lst$angle, data)
      ags <-  - values(obj)$.biovizBase.angle * 180 / pi + ags
      values(obj)$.processed.angle <- ags
      aes.lst$angle <- as.name(".processed.angle")      
    }else{
      ags <-  - values(obj)$.biovizBase.angle * 180/pi 
      values(obj)$.processed.angle <- ags
      aes.lst$angle <- as.name(".processed.angle")      
    }

    if("angle" %in% names(dots)){
      ags <-  - values(obj)$.biovizBase.angle * 180 / pi +
        as.numeric(paste(as.character(dots$angle), collapse = ""))
      values(obj)$.processed.angle <- ags
      aes.lst$angle <- as.name(".processed.angle")      
    }

    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_text, args.tot)
    p <- list(res)
    
  }

  if(geom == "point"){
    obj <- gr2newLinear(data, space.skip)
    if(!"y" %in% names(aes.lst)){
      .y <- 1
      warning("y is missing in aes(), use equal y")
    }else{
      .y <- as.character(aes.lst$y)
    }
    obj <- gr2circle(obj, y = .y, radius= radius, width = trackWidth,
                     direction = direction)
    df <- as.data.frame(obj)
    aes.lst$y <- as.name(".biovizBase.y")
    aes.lst$x <- as.name(".biovizBase.x")
    aes <- do.call("aes", aes.lst)
    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_point, args.tot)
    p <- list(res)
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
    aes.lst$group <- as.name("seqnames")
    aes <- do.call("aes", aes.lst)
    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_path, args.tot)
    p <- list(res)
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
    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_path, args.tot)
    p <- list(res)

  }
  
  if(geom == "scale"){
    ## like ideogram
    res <- getIdeoGR(data)
    res <- getScale(res, scale.unit, n = scale.n, scale.type)
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
    aes.lst.text <- aes.lst[!names(aes.lst) %in% c("xend", "yend")]
    if("angle" %in% names(aes.lst)){
      ags <- eval(aes.lst$angle, data)
      ags <- 90 - res$.biovizBase.angle * 180 / pi + ags
      res$.processed.angle <- ags
      aes.lst.text$angle <- as.name(".processed.angle")      
    }else{
      ags <- 90 - res$.biovizBase.angle * 180/pi 
      res$.processed.angle <- ags
      aes.lst.text$angle <- as.name(".processed.angle")      
    }
    
    aes.lst.text$label <- substitute(text.major)    
    aes <- do.call("aes", aes.lst)
    aes.text <- do.call("aes", aes.lst.text)
    
    if(!"hjust" %in% c(names(args.extra), names(aes.lst.text)))
      aes.text$hjust <- 0
    if(!"size" %in% c(names(args.extra), names(aes.lst.text)))    
      aes.text$size <- 3
    args.tot <- c(list(data = res, aes.text), args.extra)
    res.text <- do.call(geom_text, args.tot)
    p <- list(res.text, geom_segment(data = res, aes))
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

    if("fill" %in% names(aes.lst.p)){
      if(!"color" %in% names(aes.lst.p)){
        aes.lst.p$color <- aes.lst.p$fill
      }
    }
    aes.p <- do.call("aes", aes.lst.p)
    if(!"color" %in% names(aes.lst.p)){
      col <- I("black")
      args.extra$color <- col
    }
    args.tot <- c(list(data = df, aes.p), args.extra)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)
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

    args.tot <- c(list(data = df, aes), args.extra)
    res <- do.call(geom_segment, args.tot)
    p <- list(res)
  }
  
  if(geom == "link"){
    res <- linkInter(data, space.skip = space.skip, linked.to = linked.to,
                     link.fun = link.fun, trackWidth = trackWidth, radius = radius,
                     direction = direction)
    aes.lst$y <- as.name("y")
    aes.lst$x <- as.name("x")
    aes.lst$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", aes.lst)
    args.tot <- c(list(data = res, aes), args.extra)
    res <- do.call(geom_path, args.tot)
    p <- list(res)

  }
  if(geom == "ribbon"){
    stop("geom(ribbon) is not implemented yet")
  }
  p <- c(p, list(opts(aspect.ratio = 1), theme_null()))
  p 
})




