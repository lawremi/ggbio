## FIXME: more flexible name.expr arguments
setGeneric("stat_gene", function(data, ...) standardGeneric("stat_gene"))
setMethod("stat_gene", "TranscriptDb", function(data, ..., which,xlim,
                                                truncate.gaps = FALSE,
                                                truncate.fun = NULL,
                                                ratio = 0.0025, 
                                                xlab, ylab, main,
                                                facets = NULL,
                                                geom = c("gene", "reduced_gene"),
                                                names.expr = expression(paste(tx_name,
                                               "(", gene_id,")", sep = ""))){


  ## need to test facets = gene
  ## if(is.null(geom))
  ##   geom <- "area"
  if(missing(which))
    stop("missing which is not supported yet")

  object <- data
  geom <- match.arg(geom)

  args <- list(...)
  args$facets <- facets
  
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)

  ## tweak with args.aes to overcome the problem that single tiny rectangle missed.
  if(!"fill" %in% names(args.aes) & "fill" %in% names(args.non)){
    args.non$fill <- "black"
  }
  if(geom == "gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(object, which, truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)
    if(length(gr)){
    message("Constructing graphics...")
    values(gr)$stepping <-  as.numeric(values(gr)$tx_id)
    ## drawing
    ## hard coded width of rect
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    ## p <- ggplot(df.cds)
    ## add a segment for hacking at 1pix
    if(nrow(df.cds)){
    args.cds <- args.aes[names(args.aes) != "y"]
    args.cds <- c(args.cds, list(x = substitute(start),
                         xend = substitute(start),
                         y = substitute(stepping - 0.4),
                         yend = substitute(stepping + 0.4)))
    aes.res <- do.call(aes, args.cds)
    args.cds.res <- c(list(data = df.cds),
                      list(aes.res),
                      args.non)
    p <- list(do.call(ggplot2::geom_segment, args.cds.res))
  }else{
    p <- NULL
  }
    ## rect
    if(nrow(df.cds)){
      args.cds <- args.aes[names(args.aes) != "y"]
      args.cds <- c(args.cds, list(xmin = substitute(start),
                                   xmax = substitute(end),
                                   ymin = substitute(stepping - 0.4),
                                   ymax = substitute(stepping + 0.4)))
      aes.res <- do.call(aes, args.cds)
      args.cds.res <- c(list(data = df.cds),
                        list(aes.res),
                        args.non)
      p <- c(p, list(do.call(ggplot2::geom_rect, args.cds.res)))
    }else{
      p <- NULL
    }
    ## utrs
    df.utr <- df[df$type == "utr",]
    ## add a segment for hacking at 1pix
    args.utr <- args.aes[names(args.aes) != "y"]
    args.utr <- c(args.utr, list(x = substitute(start),
                         xend = substitute(start),
                         y = substitute(stepping - 0.2),
                         yend = substitute(stepping + 0.2)))
    aes.res <- do.call(aes, args.utr)
    args.utr.res <- c(list(data = df.utr),
                      list(aes.res),
                      args.non)
    p <- c(p, list(do.call(ggplot2::geom_segment, args.utr.res)))

    args.utr <- args.aes[names(args.aes) != "y"]

    args.utr <- c(args.utr, list(xmin = substitute(start),
                         xmax = substitute(end),
                         ymin = substitute(stepping - 0.2),
                         ymax = substitute(stepping + 0.2)))
    aes.res <- do.call(aes, args.utr)
    args.utr.res <- c(list(data = df.utr),
                      list(aes.res),
                      args.non)
    p <- c(p, list(do.call(ggplot2::geom_rect, args.utr.res)))
    
    ## p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    ## df.gaps <- gr[values(gr)$type == "gap"]
    df.gaps <- getGaps(c(gr[values(gr)$type %in% c("utr", "cds")]),
                       group.name = "tx_id")
    args.aes.gaps <- args.aes[!(names(args.aes) %in% c("x", "y", "fill"))]
    aes.res <- do.call(aes, args.aes.gaps)
    args.gaps.res <- c(list(data = df.gaps),
                       list(aes.res),
                       args.non)
     p <- c(p , list(do.call(geom_chevron, args.gaps.res)))

    .df.lvs <- unique(df$stepping)
    .df.sub <- df[, c("stepping", "tx_id", "tx_name", "gene_id")]
    .df.sub <- .df.sub[!duplicated(.df.sub),]
    .labels <- NA
    ## names.expr <- substitute(names.expr)
    if(is.expression(names.expr))
      .labels <- eval(names.expr, .df.sub)
    if(is.character(names.expr)){
      if(length(names.expr) == nrow(.df.sub)){
        .labels <- names.expr
      }else{
        stop("names.expr has unqueal length with alignment stepping levels")
      }
    }
    ## if(is.call(names.expr)){
    ##   lst <- lapply(seq_len(nrow(.df.sub)),function(i){
    ##     temp <- do.call(substitute, list(substitute(names.expr, env = parent.frame(2)), 
    ##                            list(tx_name = as.name(as.character(.df.sub$tx_name[i])),
    ##                                 tx_id = as.numeric(as.character(.df.sub$tx_id[i])),
    ##                                gene_id = as.numeric(as.character(.df.sub$gene_id[i])))))
    ##     deparse(substitute(temp))
    ##   })
    ##   .labels <- do.call("c", lst)
    ## }
    p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                labels = .labels)))
  }else{
    p <- NULL
  }
  }
  if(geom == "reduced_gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(object, which, type = "single",
                             truncate.gaps = truncate.gaps,
                             truncate.fun = truncate.fun, ratio = ratio)

    if(length(gr)){
    ## gr <- fetch(object, which, type = "single")
    message("Constructing graphics...")
    values(gr)$stepping <-  1
    ## drawing
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    ## p <- ggplot(df.cds)
    args.aes <- args.aes[names(args.aes) != "y"]
    args.aes <- c(args.aes, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(stepping - 0.4),
                         ymax = substitute(stepping + 0.4)))
    ## only for rect
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df.cds),
                  list(aes.res),
                  args.non)
    p <- list(do.call(ggplot2::geom_rect,args.res))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args.utr <- args.aes[names(args.aes) != "y"]
    args.utr <- c(args.utr, list(xmin = substitute(start),
                         xmax = substitute(end),
                         ymin = substitute(stepping - 0.2),
                         ymax = substitute(stepping + 0.2)))
    aes.res <- do.call(aes, args.utr)
    args.utr.res <- c(list(data = df.utr),
                      list(aes.res),
                      args.non)
    p <- c(p, list(do.call(ggplot2::geom_rect, args.utr.res))) 
    ## gaps
    gr.rr <- reduce(ranges(gr[(values(gr)$type %in%  c("utr", "cds"))]))
    df.gaps <- gaps(gr.rr, start = min(start(gr.rr)), end = max(end(gr.rr)))
    chrs <- unique(as.character(seqnames(gr)))
    df.gaps <- GRanges(chrs, df.gaps)
    args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "fill"))]
    .df.lvs <- unique(df$stepping)
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = df.gaps),
                  list(aes.res),
                  args.non)
    p <- c(p, list(do.call(geom_chevron, args.res)))
  }else{
    p <- NULL
  }
    p <- c(p, list(scale_y_continuous(breaks = NULL)), list(opts(axis.text.y = theme_blank())))
  }
  if(missing(xlab)){
    if(length(gr)){
    chrs <- unique(seqnames(gr))
    gms <- genome(object)
    gm <- unique(gms[chrs])
    chrs.tx <- paste(chrs, sep = ",")
    if(is.na(gm)){
      xlab <- chrs.tx
    }else{
      gm.tx <- paste(gm)
      xlab <- paste(gm.tx,"::",chrs.tx, sep = "")      
    }}else{
      xlab <- ""
    }
  }
  p <- c(p, list(xlab(xlab)))
  if(missing(ylab)){
    p <- c(p, list(ggplot2::ylab(getYLab(object))))
  }else{
    p <- c(p, list(ylab(ylab)))
  }
  if(!missing(main))
    p <- c(p, opts(title = main))
  
  if(!missing(xlim)){
    ## xlim <- c(start(range(gr)),
    ##           end(range(gr)))
  p <- c(p, list(coord_cartesian(xlim = xlim, wise = TRUE)))
  }
  ## test scale
  if(is_coord_truncate_gaps(gr)){
    gr <- gr[values(gr)$type %in% c("utr", "cds")]
    ss <- getXScale(gr)
    p <- c(p, list(scale_x_continuous(breaks = ss$breaks,
                                labels = ss$labels)))
  }
  p  
})
