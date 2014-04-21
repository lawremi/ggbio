setGeneric("layout_circle", function(data,...) standardGeneric("layout_circle"))

setMethod("layout_circle",  "GRanges",
          function(data, ..., geom = c("point","line", "link", "ribbon","rect", "bar",
                                       "segment","hist", "scale",  "heatmap",
                                "ideogram", "text"), linked.to,
                          radius = 10, trackWidth = 5,
                          space.skip = 0.015, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluation = n),
                   rect.inter.n = 60, rank,  ylim = NULL,
                   scale.n = 60, scale.unit = NULL, scale.type = c("M", "B", "sci"),
                   grid.n = 5, grid.background = "gray70", grid.line = "white",
                   grid = FALSE,
                   chr.weight = NULL){

  args <- dots <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  scale.type <- match.arg(scale.type)
  geom <- match.arg(geom)
  ## rank
  if(!missing(rank)){
    radius <- radius + rank * trackWidth
  }

  drawGrid <- function(){
    data <- getIdeoGR(data)
    res <- transformToRectInCircle(data, y = character(),
                     space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                     direction = direction, n = rect.inter.n, mul = 0, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    args.aes <-   args.non <- list()
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name(".biovizBase.group")
    args.non$fill <- args.non$color <- grid.background
    args.tot <- c(list(data = df), list(do.call(aes, args.aes)),args.non)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)
    data <- rep(data, grid.n)
    values(data)$.grid.level <- rep(1:grid.n, each = length(data)/grid.n)
    res <- transformToSegInCircle(data, y = ".grid.level",
                    space.skip = space.skip, trackWidth = trackWidth,
                                  n = rect.inter.n,
                    radius = radius, direction = direction, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    args.aes <-   args.non <- list()
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", args.aes)
    args.non$color <- grid.line
    args.tot <- c(list(data = df), list(aes), args.non)
    res <- do.call(geom_path, args.tot)
    p <- c(p ,list(res))
  }
  if(grid)
    p.grid <- drawGrid()
  ## idoegram parse seqlengths
  if(geom == "ideogram"){
    data <- getIdeoGR(data)
    res <- transformToRectInCircle(data, y = as.character(args.aes$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    args.aes <- args.aes[names(args.aes) != "label"]
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name(".biovizBase.group")


    if("fill" %in% names(args.aes)){
      if(!"color" %in% names(args.aes)){
        args.aes$color <- args.aes$fill
      }
    }
    aes <- do.call("aes", args.aes)
    if(!"color" %in% names(args.aes)){
      col <- I("black")
      args.non$color <- col
    }
    args.tot <- c(list(data = df, aes), args.non)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)

  }

  if(geom == "text"){
    obj <- transformToGenome(data, space.skip, chr.weight = chr.weight)
    if("label" %in% names(args.aes)){
      lbs <- as.character(args.aes$label)
      if(!lbs %in% c(colnames(mold(obj[1,])),"start", "end", "seqnames","width"))
        stop("label must be one of column names")
    }else{
      stop("missing label argument in aes()")
    }
    obj <- transformToCircle(obj, y = as.character(args.aes$y), radius= radius, ylim = ylim,
                             trackWidth = trackWidth,
                     direction = direction)
    ## compute angle
    if("angle" %in% names(args.aes)){
      ags <- eval(args.aes$angle, data)
      ags <-  - values(obj)$.circle.angle * 180 / pi + ags
      values(obj)$.processed.angle <- ags
      args.aes$angle <- as.name(".processed.angle")
    }else{
      ags <-  - values(obj)$.circle.angle * 180/pi
      values(obj)$.processed.angle <- ags
      args.aes$angle <- as.name(".processed.angle")
    }
    if("angle" %in% names(dots)){
      ags <-  - values(obj)$.circle.angle * 180 / pi +
        as.numeric(paste(as.character(dots$angle), collapse = ""))
      values(obj)$.processed.angle <- ags
      args.aes$angle <- as.name(".processed.angle")
    }
    names(obj) <- NULL
    df <- as.data.frame(obj)
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = df, aes), args.non)
    res <- do.call(geom_text, args.tot)
    p <- list(res)

  }

  if(geom == "point"){
    obj <- transformToGenome(data, space.skip, chr.weight = chr.weight)
    if(!"y" %in% names(args.aes)){
      .y <- 1
      warning("y is missing in aes(), use equal y")
    }else{
      .y <- as.character(args.aes$y)
    }
    obj <- transformToCircle(obj, y = .y, radius= radius, trackWidth = trackWidth, ylim = ylim,
                     direction = direction)
    names(obj) <- NULL
    df <- as.data.frame(obj)
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = df, aes), args.non)
    res <- do.call(geom_point, args.tot)
    p <- list(res)
  }

  if(geom == "line"){
    if(!"y" %in% names(args.aes))
      stop("y is missing in aes()")
    obj <- transformToGenome(data, space.skip, chr.weight = chr.weight)
    obj <- transformToCircle(obj, y = as.character(args.aes$y), ylim = ylim,
                             radius= radius, trackWidth = trackWidth,
                     direction = direction)
    names(obj) <- NULL
    df <- as.data.frame(obj)
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name("seqnames")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = df, aes), args.non)
    res <- do.call(geom_path, args.tot)
    p <- list(res)
  }

  if(geom == "segment"){
    res <- transformToSegInCircle(data, y = as.character(args.aes$y),
                    space.skip = space.skip, trackWidth = trackWidth,
                    radius = radius, direction = direction, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = df), list(aes), args.non)
    res <- do.call(geom_path, args.tot)
    p <- list(res)
  }

  if(geom == "heatmap"){
    res <- biovizBase:::transformToSegInCircle2(data, y = as.character(args.aes$y),
                    space.skip = space.skip, trackWidth = trackWidth,
                    radius = radius, direction = direction, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = df), list(aes), args.non)
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
    res <- transformToGenome(res, space.skip, chr.weight = chr.weight)
    res <- transformToCircle(res, y = "scale.y", radius= radius, trackWidth = trackWidth,
                             ylim = ylim,
                     direction = direction)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group)
    df <- df[idx, ]
    N <- nrow(df)
    res <- df[seq(1, N-1, by = 2),]
    res[,c(".circle.xend", ".circle.yend")] <-
      df[seq(2, N, by = 2), c(".circle.x", ".circle.y")]
    args.aes$y <- as.name(".circle.y")
    args.aes$x <- as.name(".circle.x")
    args.aes$yend <- as.name(".circle.yend")
    args.aes$xend <- as.name(".circle.xend")
    ## aes <- do.call("aes", args.aes)
    args.aes.text <- args.aes[!names(args.aes) %in% c("xend", "yend")]
    if("angle" %in% names(args.aes)){
      ags <- eval(args.aes$angle, data)
      ags <- 90 - res$.circle.angle * 180 / pi + ags
      res$.processed.angle <- ags
      args.aes.text$angle <- as.name(".processed.angle")
    }else{
      ags <- 90 - res$.circle.angle * 180/pi
      res$.processed.angle <- ags
      args.aes.text$angle <- as.name(".processed.angle")
    }
    args.aes.text$label <- as.name("text.major")
    if(!"hjust" %in% c(names(args.non), names(args.aes.text)))
      args.non$hjust <- 0
    if(!"size" %in% c(names(args.non), names(args.aes.text)))
      args.non$size <- 3

    aes <- do.call("aes", args.aes)
    aes.text <- do.call("aes", c(args.aes.text))
    args.tot <- c(list(data = res), list(aes.text), args.non)
    res.text <- do.call(geom_text, args.tot)
    res.seg <- do.call(ggplot2::geom_segment,c(list(data = res), list(aes)))
    p <- c(list(res.text), list(res.seg))
  }

  if(geom == "rect"){
    res <- transformToRectInCircle(data, y = as.character(args.aes$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    args.aes.p <- args.aes
    args.aes.p$y <- as.name(".circle.y")
    args.aes.p$x <- as.name(".circle.x")
    args.aes.p$group <- as.name(".biovizBase.group")
    aes.p <- do.call("aes", args.aes.p)
    if(!"color" %in% names(args.aes) & !"color" %in% names(args.non)){
      args.non$color <- "black"
    }
    if(!"fill" %in% names(args.aes) & !"fill" %in% names(args.non)){
      args.non$fill <- "black"
    }
    args.tot <- c(list(data = df, aes.p), args.non)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)
  }

  if(geom == "bar"){
    res <- transformToBarInCircle(data, y = as.character(args.aes$y),
                    space.skip = space.skip, trackWidth = trackWidth, radius = radius,
                    direction = direction, n = rect.inter.n, chr.weight = chr.weight)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    args.aes.p <- args.aes
    args.aes.p$y <- as.name(".circle.y")
    args.aes.p$x <- as.name(".circle.x")
    args.aes.p$group <- as.name(".biovizBase.group")
    aes.p <- do.call("aes", args.aes.p)
    if(!"color" %in% names(args.aes) & !"color" %in% names(args.non)){
      args.non$color <- "black"
    }
    if(!"fill" %in% names(args.aes) & !"fill" %in% names(args.non)){
      args.non$fill <- "black"
    }
    args.tot <- c(list(data = df, aes.p), args.non)
    res <- do.call(geom_polygon, args.tot)
    p <- list(res)
  }

  if(geom == "link"){
    res <- transformToLinkInCircle(data, space.skip = space.skip, linked.to = linked.to,
                     link.fun = link.fun, trackWidth = trackWidth, radius = radius,
                     direction = direction, chr.weight = chr.weight)
    args.aes$y <- as.name("y")
    args.aes$x <- as.name("x")
    args.aes$group <- as.name(".biovizBase.group")
    aes <- do.call("aes", args.aes)
    args.tot <- c(list(data = res, aes), args.non )
    res <- do.call(geom_path, args.tot)
    p <- list(res)

  }
  if(geom == "ribbon"){
    stop("geom(ribbon) is not implemented yet")
  }
  if(geom == "hist"){
    stop("geom(hist) is not implemented yet")
  }
  if(grid)
    p <- c(p.grid, p)
  p <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
  p
})



## layout_circle <- function(...){
##     .Deprecated("circle")
##     message("layout_circle() is now a lower level component to transform a linear object")
##     circle(...)
## }



circle <- function(...){
    args <- list(...)
    class(args) <- "circle"
    args
}

.radius <- function(args){
    idx <- names(args) %in% c("r", "radius")
    if(sum(idx)){
        return(args[[which(idx)[1]]])
    }else{
        return(NULL)
    }

}

.trackWidth <- function(args){
    idx <- names(args) == "trackWidth"
    if(sum(idx)){
        return(args[[which(idx)[1]]])
    }else{
        return(5)
    }
}

