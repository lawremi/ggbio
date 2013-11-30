## For transcripts support three mode?
setGeneric("geom_alignment", function(data, ...) standardGeneric("geom_alignment"))


## alignment should be convenient toggle with chevron...
setMethod("geom_alignment", "GRanges", function(data,...,
                                                xlab, ylab, main,
                                                facets = NULL,
                                                stat = c("stepping", "identity"),
                                                range.geom = c("rect", "arrowrect"),
                                                gap.geom = c("chevron", "arrow", "segment"),
                                                rect.height = NULL,
                                                group.selfish = TRUE,
                                                label = TRUE){
   

  stat <- match.arg(stat)
  args <- list(...)
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)

  if("extend.size" %in% names(args.non))
    es <- args.non$extend.size
  else
    es <- 0
  
  if(is.null(rect.height))
    rect.height <- 0.4
  args.non$rect.height <- rect.height
  range.geom <- match.arg(range.geom)
  gap.geom <- match.arg(gap.geom)
  
  main.fun <- switch(range.geom,
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
                                           group.selfish = group.selfish,
                                           extend.size = es)
                       }else{
                         dt <- addStepping(dt,extend.size = es)
                       }
                     })
    
    res <- unlist(res)
    df <- mold(res)

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
  if(missing(xlab)) 
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))
  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  
  p
})




setMethod("geom_alignment", "TranscriptDb", function(data, ..., which,xlim,
                                                truncate.gaps = FALSE,
                                                truncate.fun = NULL,
                                                ratio = 0.0025, 
                                                xlab, ylab, main,
                                                facets = NULL,
                                                geom = "alignment",
                                                stat = c("identity", "reduce"),
                                                range.geom = "rect",
                                                gap.geom = "arrow",
                                                utr.geom = "rect",
                                                names.expr = "tx_name",
                                                     label = TRUE){

  
  stat <- match.arg(stat)
  gap.fun <- getGeomFun(gap.geom)
  range.fun <- getGeomFun(range.geom)
  utr.fun <- getGeomFun(utr.geom)    
  
  if(stat == "reduce")
    geom <- "reduced_gene"


  if(missing(which)){
    ## stop("missing which is not supported yet")
     p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
             list(ggplot2::xlim(c(0, 1))))
    return(p)
  }
    

  object <- data

  args <- list(...)
  args$facets <- facets

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.utr.non <- args.non
  args.gap.non <- args.non    
  if(need_color(args))
    args.non$color <- "grey20"
  
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)

  if(geom == "alignment"){
    message("Aggregating TranscriptDb...")
    gr <- crunch(object, which, truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)
    .xlim <- NULL
    if(length(gr)){
      .xlim <- c(start(range(gr, ignore.strand = TRUE)),
                 end(range(gr, ignore.strand = TRUE)))    
      
      message("Constructing graphics...")
      ## values(gr)$stepping <-  as.numeric(values(gr)$tx_id)
      ## FIXME: add flexible estimation
      if(label){
        es <- .transformTextToSpace("aaaaaaaaaaaaaaaaaa", limits = .xlim)
        gr <- addStepping(gr, group.name = "tx_id", group.selfish = FALSE, fix = "start",
                          extend.size = es)
      }else{
        gr <- addStepping(gr, group.name = "tx_id", group.selfish = FALSE)
        es <- 0
      }
      .xlim[1] <- min(.xlim) - es
      df <- mold(gr)

      if(label){
      ## get lalel
      .df.sub <- df[, c("stepping", "tx_id", "tx_name", "gene_id")]
      .df.sub <- .df.sub[!duplicated(.df.sub),]
       sts <- do.call(c, as.list(by(df, df$tx_id, function(x) min(x$start))))
      .df.sub$start <- sts - es * 0.1
      .labels <- NA
      if(is.expression(names.expr)){
        .labels <- eval(names.expr, .df.sub)
      }else if(is.character(names.expr)){

        if(length(names.expr) == nrow(.df.sub) &&   !(names.expr %in% colnames(.df.sub))){
          .labels <- names.expr
        }else{
          .labels <- sub_names(.df.sub, names.expr)
        }
      }else{
        .labels <- sub_names(.df.sub, names.expr)
      }
      .df.lvs <- unique(df$stepping)
      .df.sub$.labels <- .labels
    }
      ## cds
      gr.cds <- gr[values(gr)$type == "cds"]
      ## exons
      gr.exons <- gr[values(gr)$type == "exon"]
      args.cds.non <- args.non
      if(!"rect.height" %in% names(args.cds.non)){
        args.cds.non$rect.height <- 0.4
      }
      
      ## df.cds <- df[df$type == "cds",]
      if(length(gr.cds)){
        args.cds <- args.aes[names(args.aes) != "y"]
        args.cds$y <- as.name("stepping")
        aes.res <- do.call(aes, args.cds)
        args.cds.res <- c(list(data = gr.cds),
                          list(aes.res),
                          args.cds.non,
                          list(stat = "identity"))
        p <- do.call(range.fun, args.cds.res)
    }else if(length(gr.exons)){
        args.cds <- args.aes[names(args.aes) != "y"]
        args.cds$y <- as.name("stepping")
        aes.res <- do.call(aes, args.cds)
        ## input gr.exons
        args.cds.res <- c(list(data = gr.exons),
                          list(aes.res),
                          args.cds.non,
                          list(stat = "identity"))
        p <- do.call(range.fun, args.cds.res)
      }else{
          p <- NULL
      }
      ## utrs
      gr.utr <- gr[values(gr)$type == "utr"]
      args.utr <- args.aes[names(args.aes) != "y"]
      args.utr$y <- as.name("stepping")      
      aes.res <- do.call(aes, args.utr)

      if(!"rect.height" %in% names(args.non)){
        args.utr.non$rect.height <- 0.15
      }else{
        args.utr.non$rect.height <- args.non$rect.height/3
      }
      
      if(range.geom == "arrowrect" && utr.geom == range.geom){
        if(!"arrow.head" %in% names(args.utr.non)){
          args.utr.non$arrow.head <- 0.06
        }
        arrow.head.fix <- getArrowLen(gr.cds, arrow.head.rate = args.non$arrow.head)
        args.utr.non <- args.non[names(args.non) != "arrow.rate"]
        args.utr.non$arrow.head.fix <- arrow.head.fix
      }

      if(length(gr.utr)){
          args.utr.res <- c(list(data = gr.utr),
                            list(aes.res),
                            args.utr.non,
                            list(stat = "identity"))
          p <- c(p, list(do.call(utr.fun, args.utr.res)))
      }
      
      df.gaps <- getGaps(c(gr[values(gr)$type %in% c("utr", "cds")]),
                         group.name = "tx_id")

      args.aes.gaps <- args.aes[!(names(args.aes) %in% c("x", "y", "fill"))]
      aes.res <- do.call(aes, args.aes.gaps)
      
      if(!"arrow.rate" %in%  names(args.non)){
        if(!is.list(which)){
          arrow.rate <- 0.03 * (end(range(ranges(which))) - start(range(ranges(which))))/
            (end(range(ranges(df.gaps))) - start(range(ranges(df.gaps))))
          args.non$arrow.rate <- arrow.rate
        }
      }
      
      aes.res$y <- as.name("stepping")
      args.gaps.res <- c(list(data = df.gaps),
                         list(aes.res),
                         args.non,
                         list(stat = "identity"))
      if(length(df.gaps)){
          p <- c(p , list(do.call(gap.fun, args.gaps.res)))
      }
      if(label){
      aes.label <- do.call(aes, list(label = substitute(.labels),
                                     x = substitute(start),
                                     y = substitute(stepping)))
      args.label.res <- c(list(data = .df.sub),
                          list(aes.label),
                          list(hjust = 1),
                          args.non)

        p <- c(p , list(do.call(geom_text, args.label.res)))
      }
      p <- c(p , list(scale_y_continuous(breaks = NULL)))      


    }else{
      p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
             list(ggplot2::xlim(c(0, 1))))
    }
  }
  if(geom == "reduced_gene"){
    message("Aggregating TranscriptDb...")
    gr <- crunch(object, which, type = "reduce",
                             truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)

    .xlim <- NULL
    if(length(gr)){
      .xlim <- c(start(range(gr, ignore.strand = TRUE)),
                 end(range(gr, ignore.strand = TRUE)))
      ## gr <- fetch(object, which, type = "reduce")
      message("Constructing graphics...")
      values(gr)$stepping <-  1
      ## drawing
      ## just cds, gaps and utrs
      df <- as.data.frame(gr)
      gr.cds <- gr[values(gr)$type == "cds"]      
      args.aes <- args.aes[names(args.aes) != "y"]
      args.utr.non <- args.non            
      if(!"rect.height" %in% names(args.non)){
        args.non$rect.height <- 0.4
      }
      if(length(gr.cds)){
        args.cds <- args.aes[names(args.aes) != "y"]
        args.cds$y <- as.name("stepping")
        aes.res <- do.call(aes, args.cds)
        args.cds.res <- c(list(data = gr.cds),
                          list(aes.res),
                          args.non,
                          list(stat = "identity"))
        p <- do.call(range.fun, args.cds.res)
      }else{
        p <- NULL
      }
      
      ## utrs
      gr.utr <- gr[values(gr)$type == "utr"]
      args.utr <- args.aes[names(args.aes) != "y"]
      args.utr$y <- as.name("stepping")            
      aes.res <- do.call(aes, args.utr)
      if(!"rect.height" %in% names(args.utr.non)){
        args.utr.non$rect.height <- 0.15
      }else{
        args.utr.non$rect.height <- args.non$rect.height/3
      }
      if(range.geom == "arrowrect" && utr.geom == range.geom){
        if(!"arrow.head" %in% names(args.non)){
          args.utr.non$arrow.head <- 0.06
        }
        arrow.head.fix <- getArrowLen(gr.cds, arrow.head.rate = args.non$arrow.head)
        args.utr.non <- args.non[names(args.non) != "arrow.rate"]
        args.utr.non$arrow.head.fix <- arrow.head.fix
      }
      args.utr.res <- c(list(data = gr.utr),
                        list(aes.res),
                        args.utr.non,
                        list(stat = "identity"))
      p <- c(p, list(do.call(utr.fun, args.utr.res)))
      
      ## gaps
      gr.rr <- reduce(ranges(gr[(values(gr)$type %in%  c("utr", "cds"))]))
      df.gaps <- gaps(gr.rr, start = min(start(gr.rr)), end = max(end(gr.rr)))

      chrs <- unique(as.character(seqnames(gr)))
      df.gaps <- GRanges(chrs, df.gaps)
      ## values(df.gaps)$stepping <- 1

      if(!"arrow.rate" %in%  names(args.non)){
        if(!is.list(which)){
          arrow.rate <- 0.03 * (end(range(ranges(which))) - start(range(ranges(which))))/
            (end(range(ranges(df.gaps))) - start(range(ranges(df.gaps))))
          args.non$arrow.rate <- arrow.rate
        }
      }
      
      args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "xmin",
                                                    "xmax", "ymin", "ymax", "fill"))]
      aes.res <- do.call(aes, args.aes)

      args.gaps.res <- c(list(data = df.gaps),
                         list(aes.res),
                         args.non)

      
      p <- c(p , list(do.call(gap.fun, args.gaps.res)))

    }else{
      p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
             list(ggplot2::xlim(c(0, 1))))
    }
    p <- c(p, list(scale_y_continuous(breaks = NULL)),
           list(theme(axis.text.y = element_blank())))
  }


  if(missing(xlab)){
    xlab <- ""
  }else{
    xlab <- ggplot2::xlab(xlab)
  }

  p <- c(p, list(xlab(xlab)))
  if(missing(ylab)){
    p <- c(p, list(ggplot2::ylab(getYLab(object))))
  }else{
    p <- c(p, list(ylab(ylab)))
  }
  if(!missing(main))
    p <- c(p, labs(title = main))
  

  if(is_coord_truncate_gaps(gr)){
      gr <- gr[values(gr)$type %in% c("utr", "cds")]
      ss <- getXScale(gr)
      p <- c(p, list(scale_x_continuous(breaks = ss$breaks,
                                        labels = ss$labels)))
  }else{
      if(is.null(.xlim)){
          if(length(which) && is(which, "GRagnes")){
              .xlim <- c(start(range(which, ignore.strand = TRUE)),
                         end(range(which, ignore.strand = TRUE)))
              p <- c(p, list(scale_by_xlim(.xlim)))      
          }
      }else{
          p <- c(p, list(scale_by_xlim(.xlim)))      
      }
      if(missing(xlim)){
          xlim <- .xlim
      }
      p <- c(p, list(coord_cartesian(xlim = xlim)))
  }
  p <- setStat(p)  
  p  
})


.transformTextToSpace <- function(x = character(), limits = NULL,
                                  fixed = 80, size = 1 ){
  if(!length(limits))
    stop("pleast provide your limits of current viewed coordinate")
  nchar(x) * size / fixed * diff(limits)
}


## For bam file show individual aligments

setMethod("geom_alignment", "BamFile", function(data,..., which,
                                                what = c("rname", "strand", "pos", "qwidth", "seq"),
                                                xlab, ylab, main,
                                                facets = NULL){
    bi <- biovizBase:::scanBamGRanges(fl, which = which, what = what)
    bt <- bi[1:100]
    bt <- biovizBase::addStepping(bt)
    names(bt) <- NULL
    
    d <- as.data.frame(bt)

    library(ggplot2)
    qplot(x = start, y = stepping, label = df, geom = "text", data = d)

})
