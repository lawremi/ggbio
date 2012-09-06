## FIXME: more flexible name.expr arguments
setGeneric("stat_gene", function(data, ...) standardGeneric("stat_gene"))
setMethod("stat_gene", "TranscriptDb", function(data, ..., which,xlim,
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
                                                names.expr = "tx_name(gene_id)"){

  
  
  ## stat <- match.arg(stat)
  ## geom <- match.arg(geom)
  ## gap.geom <- match.arg(gap.geom)
  gap.fun <- getGeomFun(gap.geom)
  range.fun <- getGeomFun(range.geom)
  utr.fun <- getGeomFun(utr.geom)    
  
  if(stat == "reduce")
    geom <- "reduced_gene"


  if(missing(which))
    stop("missing which is not supported yet")

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
    gr <- biovizBase:::fetch(object, which, truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)
    .xlim <- c(start(range(gr, ignore.strand = TRUE)),
               end(range(gr, ignore.strand = TRUE)))    
    if(length(gr)){
      message("Constructing graphics...")
      
      values(gr)$stepping <-  as.numeric(values(gr)$tx_id)
      df <- as.data.frame(gr)      
      gr.cds <- gr[values(gr)$type == "cds"]
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
      args.utr.res <- c(list(data = gr.utr),
                        list(aes.res),
                        args.utr.non,
                        list(stat = "identity"))
      p <- c(p, list(do.call(utr.fun, args.utr.res)))
      
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
      
      p <- c(p , list(do.call(gap.fun, args.gaps.res)))

      .df.lvs <- unique(df$stepping)
      .df.sub <- df[, c("stepping", "tx_id", "tx_name", "gene_id")]
      .df.sub <- .df.sub[!duplicated(.df.sub),]
      .labels <- NA

      if(is.expression(names.expr)){
        .labels <- eval(names.expr, .df.sub)
      }else if(is.character(names.expr)){
        if(length(names.expr) == nrow(.df.sub)){
          .labels <- names.expr
        }else{
          .labels <- sub_names(.df.sub, names.expr)
        }
      }else{
        .labels <- sub_names(.df.sub, names.expr)
      }
      p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                         labels = .labels)))
    }else{
      p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
             list(ggplot2::xlim(c(0, 1))))
    }
  }
  if(geom == "reduced_gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(object, which, type = "single",
                             truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)

    .xlim <- c(start(range(gr, ignore.strand = TRUE)),
               end(range(gr, ignore.strand = TRUE)))
    
    if(length(gr)){
      ## gr <- fetch(object, which, type = "single")
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
           list(theme(axis.text.y = theme_blank())))
  }
  if(missing(xlab)){
        xlab <- getXLab(gr)
      }
  p <- c(p, list(xlab(xlab)))
  if(missing(ylab)){
    p <- c(p, list(ggplot2::ylab(getYLab(object))))
  }else{
    p <- c(p, list(ylab(ylab)))
  }
  if(!missing(main))
    p <- c(p, theme(title = main))
  
  ## test scale
  if(is_coord_truncate_gaps(gr)){
    gr <- gr[values(gr)$type %in% c("utr", "cds")]
    ss <- getXScale(gr)
    p <- c(p, list(scale_x_continuous(breaks = ss$breaks,
                                      labels = ss$labels)))
  }else{
    p <- c(p, list(scale_by_xlim(.xlim)))
    if(!missing(xlim)){
      p <- c(p, list(coord_cartesian(xlim = xlim)))
    } else if (!is.list(which)) {
      xlim <- c(start(which), end(which))
      p <- c(p, list(coord_cartesian(xlim = xlim)))
    }
  }
  p  
})
