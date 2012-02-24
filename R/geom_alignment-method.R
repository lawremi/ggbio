setGeneric("geom_alignment", function(data, ...) standardGeneric("geom_alignment"))

## alignment should be convenient toggle with chevron...
setMethod("geom_alignment", "GRanges", function(data,...,
                                                facets = NULL,
                                                   stat = c("stepping", "identity"),
                                                   main.geom = c("rect", "5poly"),
                                                   gap.geom = c("chevron", "arrow", "segment"),
                                                   rect.height = 0.4,
                                                group.selfish = TRUE){
   


  stat <- match.arg(stat)
  
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  args.non <- args.non[!names(args.non) %in% c("data", "facets",
                                               "rect.height", "geom", "stat",
                                               "main.geom", "gap.geom")]
  facet <- .buildFacetsFromArgs(data, args.facets)
  
  rect.height <- force(rect.height)

  main.geom <- match.arg(main.geom)
  gap.geom <- match.arg(gap.geom)
  
  main.fun <- switch(main.geom,
                     rect = {
                       geom_rect
                     },
                     "5poly" = {
                       geom_5poly
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
  if(stat == "stepping"){
    args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "ymin", "ymax", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("xmin", "xmin", "ymin", "ymax", "data"))]
    if(rect.height <= 0 | rect.height >= 0.5)
      stop("rect.height must be a value in (0,0.5)")
    
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl,
                     function(dt){
                       if("group" %in% names(args.aes)){
dt <- addSteppings(dt, group.name = as.character(args.aes$group),              
                                           group.selfish = group.selfish)
                       ## tryres <- try(dt <- addSteppings(dt,
           ##                                  group.name = as.character(args.aes$group),
                         ##                    group.selfish = group.selfish))
                         ## if(inherits(tryres, "try-error")) browser()
                         ## dt
                       }else{
                         dt <- addSteppings(dt)
                       }
                     })
    res <- unlist(res)
    df <- fortify(data = res)

    if("group" %in% names(args.aes))
      gpn <- as.character(args.aes$group)
    else
      gpn <- ".levels"
    
    args.aes <- args.aes[names(args.aes) != "group"]
    ## plot gaps

    gps <- suppressWarnings(getGap(res, group.name = gpn, facets))
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
    args.gaps$y <- as.name(".levels")
    aes.lst <- do.call("aes", args.gaps)
    gps.lst <- c(list(aes.lst), list(data = gps, stat = "identity"),
                 args.gaps.extra)
    p <- list(do.call(gap.fun, gps.lst))
    ## plot main
    args.aes$y <- as.name(".levels")
    args.aes <- args.aes[names(args.aes) != "size"]
    args.non$stat = "identity"
    aes <- do.call(ggplot2::aes, args.aes)
    args.res <- c(list(data = res), list(aes),
                  args.non)
    p <- c(p, list(do.call(main.fun,args.res)))
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
   stop("stat identity is nor supported for geom alignment") 
  }
    p <- c(list(p) , list(ylab("")), list(facet))  
  p
})


