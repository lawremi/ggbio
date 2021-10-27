setGeneric("geom_segment", function(data, ...) standardGeneric("geom_segment"))

setMethod("geom_segment", "ANY", function(data, ...){
    ggplot2::geom_segment(data  = data, ...)
})

setMethod("geom_segment", "GRanges", function(data,..., xlab, ylab, main,
                                           facets = NULL,
                                           stat = c("stepping", "identity"),
                                           group.selfish = TRUE){

  args <- list(...)
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non <- remove_args(args.non, "facets")
  facet <- build_facet(data, args)
  
  stat <- match.arg(stat)
  es <- ifelse("extend.size" %in% names(args.non), args.non$extend.size, 0)
  if(length(data)){
  if(stat == "stepping"){
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl, make_addStepping, args.aes, group.selfish, extend.size = es)
    df <- mold(unlist(res))

    args.aes <- remove_args(args.aes, c("x", "xend", "y", "yend", "data"))
    args.non <- remove_args(args.non, c("x", "xend", "yend", "yend", "data"))
    gpn <- ifelse("group" %in% names(args), quo_name(args$group), "stepping")
    args.aes <- remove_args(args.aes, "group")
    args.aes <- c(args.aes, list(x = substitute(start),
                                 xend = substitute(end),
                                 y = substitute(stepping),
                                 yend = substitute(stepping)))

    args.aes <- remove_args(args.aes, c("size", "fill"))
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res), args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
    .df.sub <- group_df(df, gpn)
    y_scale <- scale_y_continuous_by_group(.df.sub, gpn, group.selfish)
    p <- c(p, y_scale)
  }
  
  if(stat == "identity"){
    if(!"y" %in% names(args.aes)){
      if(!all(c("y","yend", "x", "xend") %in% names(args.aes))){
        stop("aes(x =, xend= , y =, yend= ) is required for stat 'identity',
              you could also specify aes(y =) only as alternative")
      }
    }else{
      .y <- args.aes$y
      args.aes$x <- as.name("start")
      args.aes$xend <- as.name("end")
      args.aes$y <- args.aes$yend <- .y
    }
    df <- mold(data)
    args.aes <- remove_args(args.aes, c("group", "size", "fill"))
    
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
  }
  }else{
    p <- NULL
  }
  p <- c(list(p) , list(facet))
  labels <- Labels(xlab, ylab, main, fallback = c(x = ""))
  p <- c(p, labels)
  p
})
