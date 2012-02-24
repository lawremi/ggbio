setGeneric("geom_segment", function(data, ...) standardGeneric("geom_segment"))
setMethod("geom_segment", "data.frame", function(data, ...){
  ggplot2::geom_segment(data, ...)
})
## alignment should be convenient toggle with chevron...
setMethod("geom_segment", "GRanges", function(data,...,
                                           facets = NULL,
                                           stat = c("stepping", "identity"),
                                           rect.height = 0.4,
                                           group.selfish = TRUE){

  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  args.non <- args.non[!names(args.non) %in% c("data", "facets", "rect.height", "geom", "stat")]
  facet <- .buildFacetsFromArgs(data, args.facets)
  
  stat <- match.arg(stat)

  rect.height <- force(rect.height)
  
  if(stat == "stepping"){
    if(rect.height <= 0 | rect.height >= 0.5)
      stop("rect.height must be a value in (0,0.5)")
    
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl,
                     function(dt){
                       if("group" %in% names(args.aes))
                         dt <- addSteppings(dt, group.name = as.character(args.aes$group),
                                            group.selfish = group.selfish)
                       else
                         dt <- addSteppings(dt)
                     })
    res <- unlist(res)
    df <- fortify(data = res)

    args.aes <- args.aes[!(names(args.aes) %in% c("x", "xend", "y", "yend", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("x", "xend", "yend", "yend", "data"))]
    if("group" %in% names(args.aes))
      gpn <- as.character(args.aes$group)
    else
      gpn <- ".levels"
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- c(args.aes, list(x = substitute(start),
                                 xend = substitute(end),
                                 y = substitute(.levels),
                                 yend = substitute(.levels)))

    args.aes <- args.aes[names(args.aes) != "size"]
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
    .df.lvs <- unique(df$.levels)
    .df.sub <- df[, c(".levels", gpn)]
    .df.sub <- .df.sub[!duplicated(.df.sub$.levels),]
    if(gpn != ".levels" & group.selfish)
      p <- c(p , list(scale_y_continuous(breaks = .df.sub$.levels,
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
    df <- fortify(data = data)
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- args.aes[names(args.aes) != "size"]
    
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df), list(aes.res),
                  args.non)
    p <- list(do.call(ggplot2::geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
  }
  p <- c(list(p) , list(facet))  
  p
})
