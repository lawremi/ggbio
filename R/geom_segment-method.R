setGeneric("geom_segment", function(data, ...) standardGeneric("geom_segment"))
setMethod("geom_segment", "data.frame", function(data, ...){
  ggplot2::geom_segment(data  = data, ...)
})
## alignment should be convenient toggle with chevron...
setMethod("geom_segment", "GRanges", function(data,..., xlab, ylab, main,
                                           facets = NULL,
                                           stat = c("stepping", "identity"),
                                           rect.height = 0.4,
                                           group.selfish = TRUE){

  args <- list(...)
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)
  
  stat <- match.arg(stat)

  rect.height <- force(rect.height)
  if(length(data)){
  if(stat == "stepping"){
    ## if(rect.height <= 0 | rect.height >= 0.5)
    ##   stop("rect.height must be a value in (0,0.5)")
    
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

    args.aes <- args.aes[!(names(args.aes) %in% c("x", "xend", "y", "yend", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("x", "xend", "yend", "yend", "data"))]
    if("group" %in% names(args.aes))
      gpn <- as.character(args.aes$group)
    else
      gpn <- "stepping"
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- c(args.aes, list(x = substitute(start),
                                 xend = substitute(end),
                                 y = substitute(stepping),
                                 yend = substitute(stepping)))

    args.aes <- args.aes[names(args.aes) != "size"]
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
    .df.lvs <- unique(df$stepping)
    .df.sub <- df[, c("stepping", gpn)]
    .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
    if(gpn != "stepping" & group.selfish)
      p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                         labels = as.character(.df.sub[, gpn]))))
    else
      p <- c(p, list(scale_y_continuous(breaks = NULL)))
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
    df <- fortify(data)
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- args.aes[names(args.aes) != "size"]
    
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
  }}else{
    p <- NULL
  }
  p <- c(list(p) , list(facet))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))
  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  p
})
