setGeneric("geom_rect", function(data, ...) standardGeneric("geom_rect"))
setMethod("geom_rect", "data.frame", function(data, ...){
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
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)
  stat <- match.arg(stat)
  if(length(data)){
  if(stat == "stepping"){
    if(is.null(rect.height))
      rect.height <- 0.4
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl,
                     function(dt){
                       if("group" %in% names(args.aes))
                         dt <- addStepping(dt, group.name = as.character(args.aes$group),
                                            group.selfish = group.selfish)
                       else
                         dt <- addStepping(dt)
                     })
    res <- unlist(res)
    df <- fortify(res)

    args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "ymin", "ymax", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("xmin", "xmax", "ymax", "ymax", "data"))]
    if("group" %in% names(args.aes))
      gpn <- as.character(args.aes$group)
    else
      gpn <- "stepping"
    ## overcome 1 pixel problem
    args.aes.seg <- args.aes[!names(args.aes) %in%  c("group", "fill", "y", "xend", "yend", "x")]
    args.aes.seg <- c(args.aes.seg, list(x = substitute(start),
                                 xend = substitute(start),
                                 y = substitute(stepping - rect.height),
                                 yend = substitute(stepping + rect.height)))

    args.aes.seg <- args.aes.seg[names(args.aes.seg) != "size"]
    aes.res.seg <- do.call(aes, args.aes.seg)
    args.res.seg <- c(list(data = df), list(aes.res.seg),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res.seg))
    
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- c(args.aes, list(xmin = substitute(start),
                                 xmax = substitute(end),
                                 ymin = substitute(stepping - rect.height),
                                 ymax = substitute(stepping + rect.height)))

    args.aes <- args.aes[names(args.aes) != "size"]
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    
    p <- c(p, list(do.call(ggplot2::geom_rect,args.res)))
    p <- .changeStrandColor(p, args.aes)
    .df.lvs <- unique(df$stepping)
    .df.sub <- df[, c("stepping", gpn)]
    .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
    ## FIXME:
    if(gpn != "stepping" & group.selfish){
      p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                         labels = as.character(.df.sub[, gpn]))))
    }else{
      p <- c(p, list(scale_y_continuous(breaks = NULL)))
    }
    if(missing(ylab))
      p <- c(p, list(ggplot2::ylab("Stepping")))
    else
      p <- c(p, list(ggplot2::ylab(ylab)))
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
      .y <- args.aes$y
      if(is.null(rect.height)){
         rect.height <- diff(range(values(data)[,as.character(.y)]))/20
      }
      args.aes.seg <- args.aes
      args.aes.seg$x <- as.name("start")
      args.aes.seg$xend <- as.name("start")
      args.aes.seg$y <- substitute(y + rect.height, list(y = .y, rect.height = rect.height))
      args.aes.seg$yend <- substitute(y - rect.height , list(y = .y, rect.height = rect.height))
      
      args.aes$xmin <- as.name("start")
      args.aes$xmax <- as.name("end")
      args.aes$ymin <- substitute(y + rect.height, list(y = .y, rect.height = rect.height))
      args.aes$ymax <- substitute(y - rect.height , list(y = .y, rect.height = rect.height))
    }
    df <- fortify(data)

    ## overcome 1 pixel problem
    args.aes.seg <- args.aes.seg[names(args.aes.seg) != "group"]
    args.aes.seg <- args.aes.seg[names(args.aes.seg) != "size"]
    
    aes.res.seg <- do.call(aes, args.aes.seg)
    args.res.seg <- c(list(data = df), list(aes.res.seg),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res.seg))
    
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- args.aes[names(args.aes) != "size"]
    
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    p <- c(p, list(do.call(ggplot2::geom_rect,args.res)))
    p <- .changeStrandColor(p, args.aes)
    if(!missing(ylab))
        p <- c(p, list(ggplot2::ylab(ylab)))
  }}else{
    p <- NULL
  }
  p <- c(list(p) , list(facet))

  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))
  

  
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  
  p
})

