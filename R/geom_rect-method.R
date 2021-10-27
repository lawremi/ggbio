## TODO::
## Let's load a RefSeq data
## naming the interval
## two mode? packed, full with name (default)
## reduce is just a stat transformation at lower level
setGeneric("geom_rect", function(data, ...) standardGeneric("geom_rect"))

setMethod("geom_rect", "ANY", function(data, ...){
    ggplot2::geom_rect(data = data, ...)
})

## alignment should be convenient toggle with chevron...
setMethod("geom_rect", "GRanges", function(data,...,
                                           xlab, ylab, main,
                                           facets = NULL,
                                           stat = c("stepping", "identity"),
                                           rect.height = NULL,
                                           group.selfish = TRUE){


  ## make this by hand
  args <- list(...)
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- remove_args(parseArgsForNonAes(args), "facets")
  es <- ifelse("extend.size" %in% names(args.non), args.non$extend.size, 0)
  facet <- build_facet(data, args)
  stat <- match.arg(stat)
  if(length(data)){
  if(stat == "stepping"){
    if(is.null(rect.height)) rect.height <- 0.4
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl, make_addStepping, args.aes, group.selfish, extend.size = es)
    df <- mold(unlist(res))

    args.aes <- remove_args(args.aes, c("xmin", "xmax", "ymin", "ymax", "data"))
    args.non <- remove_args(args.non, c("xmin", "xmax", "ymax", "ymax", "data", "facets"))
    gpn <- ifelse("group" %in% names(args), quo_name(args$group), "stepping")
    args.aes <- remove_args(args.aes, c("group", "size"))
    ## overcome 1 pixel problem
    args.aes.seg <- remove_args(args.aes, c("fill", "y", "xend", "yend", "x"))
    args.aes.seg <- c(args.aes.seg, list(x = substitute(start),
                                 xend = substitute(start),
                                 y = substitute(stepping - rect.height),
                                 yend = substitute(stepping + rect.height)))
    aes.res.seg <- do.call(aes, args.aes.seg)
    args.non.seg <- remove_args(args.non, "fill")
    args.res.seg <- c(list(data = df), list(aes.res.seg), args.non.seg)
    p <- list(do.call(ggplot2::geom_segment, args.res.seg))
    args.aes <- c(args.aes, list(xmin = substitute(start),
                                 xmax = substitute(end),
                                 ymin = substitute(stepping - rect.height),
                                 ymax = substitute(stepping + rect.height)))
    aes.res <- do.call(aes, args.aes)
    aes.res <- remove_args(aes.res, "y")
    args.res <- c(list(data = df), list(aes.res), args.non)
    p <- c(p, list(do.call(ggplot2::geom_rect, args.res)))
    p <- .changeStrandColor(p, args.aes)
    .df.sub <- group_df(df, gpn)
    ## FIXME:
    y_scale <- scale_y_continuous_by_group(.df.sub, gpn, group.selfish)
    p <- c(p, y_scale)
  }
  
  if(stat == "identity"){
    if(!"y" %in% names(args.aes)){
      if(!all(c("ymin","ymax", "xmin", "xmax") %in% names(args.aes))){
        stop("aes(xmin =, xmax= , ymin =, ymax= ) is required for stat 'identity',
              you could also specify aes(y =) only as alternative")
      }else{
        args.aes.seg <- args.aes
        args.aes.seg$x <- args.aes$xmin
        args.aes.seg$xend <- args.aes$xmax
        args.aes.seg$y <- args.aes$ymin
        args.aes.seg$yend <- args.aes$ymax
      }
    }else{
      .y <- quo_squash(args.aes$y)
      if(is.null(rect.height)){
         rect.height <- diff(range(values(data)[,as.character(.y)]))/20
      }
      args.aes.seg <- args.aes
      mapping <- list(y = .y, rect.height = rect.height)
      args.aes.seg$x <- as.name("start")
      args.aes.seg$xend <- as.name("start")
      args.aes.seg$y <- substitute(y + rect.height, mapping)
      args.aes.seg$yend <- substitute(y - rect.height , mapping)
      
      args.aes$xmin <- as.name("start")
      args.aes$xmax <- as.name("end")
      args.aes$ymin <- substitute(y + rect.height, mapping)
      args.aes$ymax <- substitute(y - rect.height , mapping)
    }
    df <- mold(data)

    ## overcome 1 pixel problem
    args.aes.seg <- remove_args(args.aes.seg, c("group", "size", "fill", "xmin", "xmax", "ymin", "ymax"))
    aes.res.seg <- do.call(aes, args.aes.seg)
    args.non.seg <- remove_args(args.non, "fill")
    args.res.seg <- c(list(data = df), list(aes.res.seg), args.non.seg)
    p <- list(do.call(ggplot2::geom_segment, args.res.seg))

    args.aes <- remove_args(args.aes, c("group", "size", "y"))
    aes.res <- do.call(aes, args.aes)

    args.res <- c(list(data = df), list(aes.res), args.non)
    p <- c(p, list(do.call(ggplot2::geom_rect, args.res)))
    p <- .changeStrandColor(p, args.aes)
  }
  }else{
    p <- NULL
  }

  p <- c(list(p), list(facet))

  if(identical(stat, "stepping"))
    labels <- Labels(xlab, ylab, main, fallback = c(x = "", y = ""))
  else labels <- Labels(xlab, ylab, main, fallback = c(x = ""))

  p <- c(p, labels)
  p
})

