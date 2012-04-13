setGeneric("geom_alignment", function(data, ...) standardGeneric("geom_alignment"))

## alignment should be convenient toggle with chevron...
setMethod("geom_alignment", "GRanges", function(data,...,
                                                xlab, ylab, main,
                                                facets = NULL,
                                                   stat = c("stepping", "identity"),
                                                   main.geom = c("rect", "arrowrect"),
                                                   gap.geom = c("chevron", "arrow", "segment"),
                                                   rect.height = 0.4,
                                                group.selfish = TRUE){
   

  stat <- match.arg(stat)
  args <- list(...)
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)

  ## rect.height <- force(rect.height)

  main.geom <- match.arg(main.geom)
  gap.geom <- match.arg(gap.geom)
  
  main.fun <- switch(main.geom,
                     rect = {
                       geom_rect
                     },
                     "arrowrect" = {
                       geom_arrowrect
                     })
  
  gap.fun <- switch(gap.geom,
                    chevron = {
                      geom_chevron
                    },
                    arrow = {
                      geom_arrow
                    },

                    segment = {
                      geom_segment
                    }
                    )
  if(length(data)){
  if(stat == "stepping"){
    args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "ymin", "ymax", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("xmin", "xmin", "ymin", "ymax", "data"))]
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl,
                     function(dt){
                       if("group" %in% names(args.aes)){
                         dt <- addStepping(dt, group.name = as.character(args.aes$group),
                                           group.selfish = group.selfish)
                       }else{
                         dt <- addStepping(dt)
                       }
                     })
    
    res <- unlist(res)
    df <- fortify(res)

    if("group" %in% names(args.aes))
      gpn <- as.character(args.aes$group)
    else
      gpn <- "stepping"
    
    args.aes <- args.aes[names(args.aes) != "group"]
    ## plot gaps

    gps <- getGaps(res, group.name = gpn, facets)
    if(length(gps)){
    gps <- keepSeqlevels(gps, names(seqlengths(res)))
    args.gaps <- args.aes[!names(args.aes) %in% c("x", "y",
                                                  "xend", "yend",
                                                  "label.type",
                                                  "label.size",
                                                  "label.color",
                                                  "size",
                                                  "fill",
                                                  "color",
                                                  "colour")]

    args.gaps.extra <- args.non[names(args.non) %in%
                            c("offset", "chevron.height")]
    args.gaps$y <- as.name("stepping")
    aes.lst <- do.call("aes", args.gaps)
    gps.lst <- c(list(aes.lst), list(data = gps, stat = "identity"),
                 args.gaps.extra)
    p <- list(do.call(gap.fun, gps.lst))
  }else{
    p <- NULL
  }
    ## plot main
    args.aes$y <- as.name("stepping")
    args.aes <- args.aes[names(args.aes) != "size"]
    args.non$stat = "identity"
    aes <- do.call(ggplot2::aes, args.aes)
    args.res <- c(list(data = res), list(aes),
                  args.non)

    p <- c(p, list(do.call(main.fun,args.res)))
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
   stop("stat identity is nor supported for geom alignment") 
  }}else{
    p <- NULL
  }
    p <- c(list(p) , list(ggplot2::ylab("")), list(facet))
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


