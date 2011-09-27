setGeneric("qplot", function(data, ...) standardGeneric("qplot"))
## ======================================================================
##        For "Granges"
## ======================================================================
setMethod("qplot", signature(data = "data.frame"), function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  do.call(ggplot2::qplot, args)
})

setMethod("qplot", signature(data = "matrix"), function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  do.call(ggplot2::qplot, args)
})

setMethod("qplot", signature(data = "data.frame"), function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  do.call(ggplot2::qplot, args)
})

setMethod("qplot", signature(data = "numeric"), function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  do.call(ggplot2::qplot, args)
})

setMethod("qplot", signature(data = "GRanges"), function(data, x, y,...,
                               facet_gr,
                               legend = TRUE,
                               ignore.strand = TRUE,
                               show.coverage = TRUE,
                               show.gaps = FALSE,
                               show.label = FALSE,
                               geom = c("full", "line","point",
                                 "segment", "histogram",
                                 "reduce",  "disjoin",                                 
                                 "coverage.line", "coverage.polygon",
                                 "mismatch.summary"
                                 )){
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  ## args <- as.list(match.call(expand.dots = FALSE)[-1])
  args.facet <- args[names(args) %in% c("nrow", "ncol", "scales", "space")]
  args <- args[!(names(args) %in% c("seqname", "geom", 
                                    "data", "legend", "nrow", "ncol"))]
  ## check "x", must be one of "start", "end", "midpoint"
  if(!(geom %in% c("full", "reduce", "disjoin", "segment"))){
    if("x" %in% names(args)){
      if(!(deparse(args$x) %in% c("start", "end", "midpoint")))
        stop("x must be one of start/end/midpoint without quote")
    }else{
      args <- c(args, list(x = substitute(midpoint)))
      message("use midpoint for x as default")
    }}
  ## check y
  if(geom %in% c("point", "line")){
    if(!("y" %in% names(args))){
      col.nm <- colnames(values(data))[1]
      if(length(col.nm))
      args <- c(args, list(y = as.name(col.nm)))
    }
  }
  ## color
  strandColor <- getOption("biovizBase")$strandColor
  ## check seqname
  if(!("scales" %in% names(args.facet)))
    args.facet <- c(args.facet, list(scales = "fixed"))
  seqname <- unique(as.character(seqnames(data)))
  allvars <- NULL
  if(missing(facet_gr)){
    if(!"facet" %in% names(args)){
      if(length(seqname) > 1){
        ## facet by default is by seqname
        facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facet)),
                              TRUE, FALSE)
        if(facet.logic){
        args.facet <- c(args.facet, list(facets = substitute(~seqnames)))
        facet <- do.call(facet_wrap, args.facet)
      }else{
        args.facet <- c(args.facet, list(facets = substitute(.~seqnames)))
        facet <- do.call(facet_grid, args.facet)
      }
      }}else{
        ## only allow simple case
        allvars <- all.vars(as.formula(args$facet))
        if(allvars[2] != "seqnames")
          stop("Column of facet formula can only be seqnames, such as . ~ seqnames,
              you can change row varaibles")
        if(allvars[1] %in% colnames(values(data)) &&
           !(geom %in% c("reduce", "disjoin","histogram",
                         "coverage.line", "coverage.polygon"))){
          facet <- do.call(facet_grid, c(args$facet, args.facet))
        }else{
          args.facet <- c(args.facet, list(facets = substitute(~seqnames)))
          facet <- do.call(facet_wrap, args.facet)
        }
      }}else{
        args.facet <- c(args.facet, list(facets = substitute(~.id.name)))
        args.facet$scales <- "free"
        facet <- do.call(facet_wrap, args.facet)
      }
  seqlevels(data) <- seqname
  p <- switch(geom,
              line = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                }
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                p + geom_line(do.call("aes", args))
              },
              point = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                }

                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                p + geom_point(do.call("aes", args))
              },
              full = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                }else{
                  grl <- split(data, seqnames(data))
                }
                data <- endoapply(grl,
                                  function(dt){
                                    if(!is.null(allvars) &&
                                       allvars[1] %in% colnames(values(data))){
                                      ndt <- split(dt, values(dt)[,allvars[1]])
                                      unlist(endoapply(ndt, function(x){
                                        if("group" %in% names(args))
                                          dt <- addSteppings(dt,
                                             group.name = as.character(args$group))
                                        else
                                          dt <- addSteppings(dt)
                                        dt
                                      }))
                                    }else{
                                      if("group" %in% names(args))
                                          dt <- addSteppings(dt,
                                             group.name = as.character(args$group))
                                        else
                                          dt <- addSteppings(dt)
                                      dt
                                    }})
                data <- unlist(data)
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                if(!("color" %in% names(args))){
                  if("fill" %in% names(args))
                    args <- c(args, list(color = args$fill))
                  else
                    args <- c(args, list(color = substitute(strand),
                                         fill = substitute(strand)))
                }
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(.levels - 0.4),
                                     ymax = substitute(.levels + 0.4)))
                gpn <- as.character(args$group)
                args <- args[names(args) != "group"]
                if(show.gaps){
                  gps <- getGap(data, group.name = gpn)
                  args.gaps <- c(args, list(x = substitute(start),
                                            xend = substitute(end),
                                       y = substitute(.levels),
                                       yend = substitute(.levels)))
                  p <- p + geom_segment(data = as.data.frame(gps),
                                        do.call(aes, args.gaps))

                }
                if(show.label){
                  label.gr <- getModelRange(data,
                                             group.name = gpn)
                  args.label <- c(args, list(x = substitute(start),
                                       y = substitute(.levels),
                                       label = substitute(.label)))
                  args.label <- args.label[names(args.label) != "size"]
                  p <- p + geom_text(data = as.data.frame(label.gr),
                                     do.call(aes, args.label),
                                     hjust = 1)
                }
                args.rect <- args[names(args) != "size"]
                p <- p + geom_rect(do.call("aes", args.rect)) +
                  scale_color_manual(values = strandColor)+
                    scale_fill_manual(values = strandColor)
              },
              reduce = {
                data <- reduce(data, ignore.strand = ignore.strand)
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                  
                }else{
                  grl <- split(data, seqnames(data))
                }

                ## grl <- split(data, seqnames(data))
                data <- endoapply(grl,
                                  function(dt){
                                    if(!is.null(allvars) &&
                                       allvars[1] %in% colnames(values(data))){
                                      ndt <- split(dt, values(dt)[,allvars[1]])
                                      unlist(endoapply(ndt, function(dt){
                                        values(dt)$.levels <-
                                          as.numeric(disjointBins(ranges(dt)))
                                        dt
                                      }))
                                    }else{
                                      values(dt)$.levels <-
                                        as.numeric(disjointBins(ranges(dt)))
                                      dt
                                    }})

                data <- unlist(data)
                df <- as.data.frame(data)
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                if(!("color" %in% names(args))){
                  if("fill" %in% names(args))
                    args <- c(args, list(color = args$fill))
                  else
                    args <- c(args, list(color = substitute(strand),
                                         fill = substitute(strand)))
                }
                args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                                     ymin = substitute(.levels - 0.4),
                                     ymax = substitute(.levels + 0.4)))
                p + geom_rect(do.call("aes", args)) +
                        scale_color_manual(values = strandColor)+
                          scale_fill_manual(values = strandColor)

              },
              disjoin = {
                data <- disjoin(data, ignore.strand = ignore.strand)
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                }else{
                  grl <- split(data, seqnames(data))
                }
                
                ## grl <- split(data, seqnames(data))
                data <- endoapply(grl,
                                  function(dt){
                                    if(!is.null(allvars) &&
                                       allvars[1] %in% colnames(values(data))){
                                      ndt <- split(dt, values(dt)[,allvars[1]])
                                      unlist(endoapply(ndt, function(dt){
                                        values(dt)$.levels <-
                                          as.numeric(disjointBins(ranges(dt)))
                                        dt
                                      }))
                                    }else{
                                      values(dt)$.levels <-
                                        as.numeric(disjointBins(ranges(dt)))
                                      dt
                                    }})

                data <- unlist(data)
                df <- as.data.frame(data)
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                if(!("color" %in% names(args))){
                  if("fill" %in% names(args))
                    args <- c(args, list(color = args$fill))
                  else
                    args <- c(args, list(color = substitute(strand),
                                         fill = substitute(strand)))
                }

                args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                                     ymin = substitute(.levels - 0.4),
                                     ymax = substitute(.levels + 0.4)))
                p + geom_rect(do.call("aes", args)) +
                  scale_color_manual(values = strandColor)+
                          scale_fill_manual(values = strandColor)                    
              },
              segment = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                }else{
                  grl <- split(data, seqnames(data))                  
                }

                
                ## grl <- split(data, seqnames(data))
                data <- endoapply(grl,
                                  function(dt){
                                    if(!is.null(allvars) &&
                                       allvars[1] %in% colnames(values(data))){
                                      ndt <- split(dt, values(dt)[,allvars[1]])
                                      unlist(endoapply(ndt, function(dt){
                                        if("group" %in% names(args))
                                          dt <- addSteppings(dt,
                                             group.name = as.character(args$group))
                                        else
                                          dt <- addSteppings(dt)
                                        dt
                                      }))
                                    }else{
                                        if("group" %in% names(args))
                                          dt <- addSteppings(dt,
                                             group.name = as.character(args$group))
                                        else
                                          dt <- addSteppings(dt)
                                      dt
                                    }})

                data <- unlist(data)
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                args <- args[!(names(args) %in% c("x", "y"))]
                if(!("color" %in% names(args))){
                  if("fill" %in% names(args))
                    args <- c(args, list(color = args$fill))
                  else
                    args <- c(args, list(color = substitute(strand),
                                         fill = substitute(strand)))
                }
                p <- ggplot(df)
                args <- c(args, list(x = substitute(start), xend = substitute(end),
                                     y = substitute(.levels),
                                     yend = substitute(.levels)))
                p + geom_segment(do.call("aes", args)) +
                  scale_color_manual(values = strandColor) +
                          scale_fill_manual(values = strandColor)                    
              },
              histogram = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                }else{
                  grl <- split(data, seqnames(data))                  
                }

                ## grl <- split(data, seqnames(data))
                data <- lapply(grl,
                               function(dt){
                                 vals <- as.numeric(coverage(ranges(dt)))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 if(length(unique(values(dt)$.id.name)))
                                   data.frame(vals = vals, seqs = seqs,
                                              .id.name = unique(values(dt)$.id.name),
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                                 else
                                   data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                               })
                df <- do.call("rbind", data)
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs)))
                if("binwidth" %in% names(args))
                  p + geom_histogram(do.call("aes", args),
                                     binwidth = args$binwidth)
                else
                  p + geom_histogram(do.call("aes", args))
              },
              coverage.line = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                  
                }else{
                  grl <- split(data, seqnames(data))
                }
                ## grl <- split(data, seqnames(data))
                data <- lapply(grl,
                               function(dt){
                                 vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 if(length(unique(values(dt)$.id.name)))                
                                   data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1],
                                              .id.name = unique(values(dt)$.id.name))
                                 else
                                   data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])               
                               })
                df <- do.call("rbind", data)
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p + geom_line(do.call("aes", args))+
                  ylab("coverage")
              },
              coverage.polygon = {
                if(!missing(facet_gr)){
                  lst.rc <- rangesCentric(data, facet_gr)
                  data <- lst.rc$gr
                  ## facet_gr.r <- lst.rc$which
                  if(args.facet$facets == "~.id.name")
                    grl <- split(data, values(data)$.id.name)
                  
                  if(args.facet$facets == "~seqnames")
                    grl <- split(data, seqnames(data))
                }else{
                  grl <- split(data, seqnames(data))
                }
                ## grl <- split(data, seqnames(data))
                data <- lapply(grl,
                               function(dt){
                                 vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 seqs <- c(seqs[1],seqs,tail(seqs, 1))
                                 vals <- c(0, vals, 0)
                                 if(length(unique(values(dt)$.id.name)))
                                   data.frame(vals = vals, seqs = seqs,
                                              .id.name = unique(values(dt)$.id.name),
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                                 else
                                   data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                               })
                df <- do.call("rbind", data)
                p <- ggplot(df)
                ## remove x
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p + geom_polygon(do.call("aes", args))+
                  ylab("coverage")
              },
              mismatch.summary = {
                if(!isPileupSum(data))
                  stop("For geom mismatch summary, data must returned from
                        pileupGRangesAsVariantTable function. Or is a GRanges
                        object including arbitrary columns: read, ref, count, depth,
                        match")
                p <- plotMismatchSum(data, show.coverage)
              }
              )
  if(!legend)
    p <- p + opts(legend.position = "none")
  p <- p + xlab(paste("Genomic Coordinates"))
  if(length(seqname)>1)
    p <- p + facet
  ## FIXME: make sure ylim is always equal
  ## if(fix.ylim){
  ##   nms <- names(p$layers[[1]]$mapping)
  ##   ## try y first
  ##   ylim <- range(values(data)[,as.character(p$layers[[1]]$mapping$y)])
  ##   ##  change to scale later
  ##   ylim <- c(ylim[1]-diff(ylim)*0.05, ylim[2] + diff(ylim)*0.05)
  ##   p <- p + scale_y_continuous(limits = ylim)
  ## }
  p
})

## ======================================================================
##        For "GRangesList"
## ======================================================================
## FIXME:
setMethod("qplot", "GRangesList", function(data, ..., freq, show.label = FALSE,
                                           show.gaps = TRUE){
  args <- list(...)
  nms <- rep(names(data), elementLengths(data))
  gr <- unlist(data)
  values(gr)$.grl.name <- nms
  if(!missing(freq)){
    freqs <- rep(freq[names(data)], elementLengths(data))
    values(gr)$.freq <- freqs
    args <- c(args, list(data = gr,
                         group = substitute(.grl.name),
                         show.label = show.label,
                         show.gaps = show.gaps,
                         size = substitute(.freq)))
  }else{
    args <- c(args, list(data = gr,
                         group = substitute(.grl.name),
                         show.label = show.label,
                         show.gaps = show.gaps))
  }
  do.call(qplot, args)
  ## qplot(gr, group = .grl.name, ..., show.label = show.label, show.gaps = show.gaps)
})

## ======================================================================
##        For "IRanges"
## ======================================================================

setMethod("qplot", "IRanges", function(data, ...,
                                       legend = TRUE,
                                       geom = c("full", "segment", "histogram",
                                         "coverage.line", "coverage.polygon",
                                         "reduce")){
  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "geom.engine",
                                    "data",  "legend"))]
  ## check "x", must be one of "start", "end", "midpoint"
  if("x" %in% names(args) && !missing(x)){
    if(!(deparse(args$x) %in% c("start", "end", "midpoint")))
      stop("x must be one of start/end/mipoint without quote")
  }else{
    args <- c(args, list(x = substitute(start)))
    message("use start for x as default")
  }
  geom <- match.arg(geom)
  p <- switch(geom,
              full = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(data))
                p <- ggplot(df)
                ## remove y
                
                args <- args[names(args) != "y"]
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(y - 0.4),
                                     ymax = substitute(y + 0.4)))
                p + geom_rect(do.call("aes", args))
              },
              reduce = {
                df <- as.data.frame(reduce(data))
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(reduce(data)))
                p <- ggplot(df)
                args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                                     ymin = substitute(y - 0.4), ymax = substitute(y + 0.4)))
                p + geom_rect(do.call("aes", args))
              },
              segment = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(data))
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(start), xend = substitute(end),
                                     y = substitute(y), yend = substitute(y)))
                p + geom_segment(do.call("aes", args))
              },
              histogram = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                p + geom_histogram(do.call("aes", args))
              },
              coverage.line = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                st <- min(start(data))
                cv <- coverage(data)
                vals <- as.numeric(cv)
                seqs <- seq.int(from = st, length.out = length(vals))
                ## remove x
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p + geom_line(do.call("aes", args))+
                  ylab("coverage")
              },
              coverage.polygon = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                st <- min(start(data))
                cv <- coverage(data)
                vals <- as.numeric(cv)
                seqs <- seq.int(from = st, length.out = length(vals))
                seqs <- c(seqs[1],seqs,tail(seqs, 1))
                vals <- c(0, vals, 0)
                ## remove x
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p + geom_polygon(do.call("aes", args))+
                  ylab("coverage")
              }
              )
  if(!legend)
    p <- p + opts(legend.position = "none")
  p <- p + xlab("Space")
  p
})


## ======================================================================
##        For "GappedAlignments"
## ======================================================================
setMethod("qplot", "GappedAlignments", function(data, ...,
                                                which,
                                                geom = c("gapped.pair",
                                                  "full"),
                                                show.junction = FALSE
                                                ){
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  args <- args[!(names(args) %in% c("geom", "which"
                                    ## "shrink", "shrink.ratio",
                                    ## "shrink.fun"
                                    ))]
  if(!missing(which))
    gr <- biovizBase:::fetch(data, which)
  else
    gr <- biovizBase:::fetch(data)
  if(geom == "gapped.pair"){
  gr.junction <- gr[values(gr)$junction == TRUE]
  grl <- split(gr.junction, values(gr.junction)$read.group)
  
  ir.gaps <- unlist(gaps(ranges(grl)))
  .lvs <- values(gr.junction)$.levels[match(names(ir.gaps),
                                          values(gr.junction)$read.group)]
  seqs <- unique(as.character(seqnames(gr.junction)))
  gr.gaps <- GRanges(seqs, ir.gaps, .levels = .lvs)
  gr.read <- gr  
  ## if(shrink){
  ##   max.gap <- maxGap(gr.read, shrink.ratio)
  ##   if(missing(shrink.fun))
  ##     shrink.fun <- shrinkageFun(gr.gaps, max.gap = max.gap)
  ##   gr.read <- shrink.fun(gr.read)
  ##   gr.gaps <- shrink.fun(gr.gaps)
  ## }
  df.read <- as.data.frame(gr.read)
  df.gaps <- as.data.frame(gr.gaps)
  p <- ggplot(df.read)
  strandColor <- getOption("biovizBase")$strandColor
  if(!("color" %in% names(args))){
    if("fill" %in% names(args))
      args <- c(args, list(color = substitute(fill)))
    else
      args <- c(args, list(color = substitute(strand),
                           fill = substitute(strand)))
  }

  args <- c(args, list(xmin = substitute(start),
                       xmax = substitute(end),
                       ymin = substitute(.levels - 0.4),
                       ymax = substitute(.levels + 0.4)))
  p <- p + geom_rect(do.call("aes", args))+
    scale_color_manual(values = strandColor)
  ## mapped read
  if(show.junction){
  args <- args[!(names(args) %in% c("x", "y"))]
  args <- c(args, list(x = substitute(start), xend = substitute(end),
                       y = substitute(.levels),
                       yend = substitute(.levels)))
  p <- p + geom_segment(data = df.gaps, do.call("aes", args), color = "red")
}}
  if(geom == "full"){
    p <- qplot(gr)
  }
  p <- p + xlab("Genomic Coordinate") + ylab("")
  p
})


## ======================================================================
##        For "BamFile"
## ======================================================================
## TODO
## 1. mismatch
## 2. simply summary
setMethod("qplot", "BamFile", function(data, ..., which,
                                       model,
                                       bsgenome,
                                       resize.extra = 10,
                                       show.coverage = TRUE,
                                       geom = c("gapped.pair",
                                         "full",
                                         ## "read.summary",
                                         "fragment.length",
                                         "coverage.line",
                                         "coverage.polygon",
                                         "mismatch.summary")){
  
  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "which", "heights"))]
  geom <- match.arg(geom)
  bf <- open(data)
  if(geom == "gapped.pair"){
    message("Read GappedAlignments from BamFile...")
    ga <- readBamGappedAlignments(bf,
                                  param = ScanBamParam(which = which),
                                  use.name = TRUE)
    p <- qplot(ga, ..., resize.extra = resize.extra)
    message("Done")
  }
  if(geom %in% c("coverage.line", "coverage.polygon", "full")){
    gr <- biovizBase:::fetch(ga)
    p <- qplot(gr, geom = geom)
  }
  if(geom %in% c("mismatch.summary")){
    if(missing(bsgenome)){
      stop("For geom mismatch.summary, please provide bsgenome(A BSgenome object)")
    }else
    if(!is(bsgenome, "BSgenome")){
      stop("bsgenome must be A BSgenome object")
    }
    pgr <- pileupAsGRanges(bamfile, region = which)
    pgr.match <- pileupGRangesAsVariantTable(pgr, bsgenome)
    p <- plotMismatchSum(pgr.match, show.coverage)
  }
  if(geom == "fragment.length"){
    if((missing(model)))
      stop("Fragment length require a specified model(GRanges)
    or txdb(TranscriptDb) object")
    if(!is(model, "GRanges"))
      stop("model must be a GRangs object")
    ## mds <- model
    which <- reduce(model)
    ## model <- model
    ga <- readBamGappedAlignments(data,
                                  param = ScanBamParam(which = which),           
                                  use.name = TRUE)
    dt <- biovizBase:::fetch(ga)
    s <- opts(axis.text.y = theme_blank(),
              axis.title.y = theme_blank(),
              axis.ticks = theme_blank())    
    ## now only support which contains one single gene
    ## if("show.label" %in% names(args))
    ##   show.label <- args$show.label
    ## else
    ##   show.label <- FALSE
    ## print(show.label)
    p.splice <- qplot(dt, model = model, group.name = "read.group",
                      geom = "splice") + ylab("") +
                        opts(title = "Splice Summary",
                             plot.title=theme_text(size=10),
                             plot.margin = unit(c(0,2,0,2), "lines"))+s
    ## ## model.all
    p.exons.r <- qplot(disjoin(ranges(model)))+ylab("")+
      opts(title = "Reduced Model",
           plot.title=theme_text(size=10),
           plot.margin = unit(c(0,2,0,2), "lines")) + xlab("Genomic Coordinate")+s
    ## p.ga <- qplot(ga) + ylab("") + opts(title = "Gapped Alignments",
    ##                                     plot.title=theme_text(size=10),
    ##                                     plot.margin = unit(c(0,2,0,0), "lines"))
    ## gina.r <- as(ga, "GRanges")
    ga.r <- biovizBase:::fetch(data, raw, which = which)
    p.cov <- qplot(ga.r, geom = "coverage.p") + ylab("") +
      opts(title = "Coverage",
           plot.title=theme_text(size=10),
           plot.margin = unit(c(0,2,0,2), "lines"))+s
    ## fragment based on dt
    names(dt) <- NULL
    ## g.gap <- gaps(disjoin(ranges(model)))
    ## cut.fun <- shrinkageFun(g.gap, 1L)
    ## dt.cut <- cut.fun(dt)
    ## FIXME: need to use 
    ## getFragLength <- function(data, data.ori, group.name = "qname"){
    ##   me <- split(data, values(data)[,group.name])
    ##   ir <- range(ranges(me, ignore.strand = TRUE))
    ##   me.ori <- split(data.ori, values(data.ori)[,group.name])
    ##   ir.ori <- range(ranges(me.ori, ignore.strand = TRUE))
    ##   ir <- unlist(ir)
    ##   ir.ori <- unlist(ir.ori)
    ##   gr <- GRanges(unique(as.character(seqnames(data)))[1],
    ##                 ir.ori)
    ##   values(gr)$wid <- width(ir)
    ##   gr
    ## }
    ## gr <- getFragLength(dt.cut, dt)
    gr <- getFragLength(dt, model)
    p.frag <- qplot(gr, y = .fragLength, geom = "point") + ylab("") +
      opts(title = "Fragment Length",
           plot.title=theme_text(size=10),
           plot.margin = unit(c(0,2,0,2), "lines"))+s
    ## if(missing(heights))
    ##   heights <- c(30, 30, 30, 30, 30)
    ## do.call(tracks, c(list(p.cov, p.frag, p.ga, p.splice, p.exons.r),
    ##                    list(heights = substitute(heights),
    ##                         ncol = 1)))
    tracks(p.cov, p.frag, p.splice, p.exons.r,
           heights = c(30, 30, 20, 15), ncol = 1)
  }
  ## list(p.cov = p.cov,
  ##      p.frag = p.frag,
  ##      p.align = p.ga)
  p
})
## ======================================================================
##  For "character" need to check if it's path including bamfile or not
## ======================================================================
setMethod("qplot", "character", function(data, x, y, ...,
                                         which, model,
                                         bsgenome,
                                         resize.extra = 10,
                                         show.coverage = TRUE,
                                         geom = c("gapped.pair",
                                           "full",
                                           "fragment.length",
                                           "coverage.line",
                                           "coverage.polygon",
                                           "mismatch.summary")){
  geom <- match.arg(geom)
  if(tools::file_ext(data) == "bam")
    bf <- BamFile(data)
  else
    stop("Please pass a bam file path")
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  qplot(bf,  ..., which = which, model = model, bsgenome = bsgenome,
        show.coverage = show.coverage,
        geom = geom, resize.extra = reszie.extra)
})


## ======================================================================
##        For "TranscriptDb"(Genomic Structure)
## ======================================================================
setMethod("qplot", "TranscriptDb", function(data, x, y, which, ...,
                                            geom = c(
                                              "full",
                                              "single")){
  ## FIXME: need to be in biovizBase
  color.def <- "black"
  fill.def <- "black"
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  .args <- args[!(names(args) %in% c("geom", "which","data"))]
  if(geom == "full"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(data, which)
    message("Constructing graphics...")
    values(gr)$.levels <-  as.numeric(values(gr)$tx_id)
    ## drawing
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    p <- ggplot(df.cds)
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.4),
                         ymax = substitute(.levels + 0.4)))
    ## only for rectangle 
    if(!("color" %in% names(args)))
      p <- p + geom_rect(do.call("aes", args), color = color.def)
    else
      p <- p + geom_rect(do.call("aes", args))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.2),
                         ymax = substitute(.levels + 0.2)))
    if(!("color" %in% names(args)))
      p <- p + geom_rect(data = df.utr, do.call("aes", args), color = color.def)
    else
      p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    df.gaps <- df[df$type == "gap",]
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    args <- c(args, list(x = substitute(start), xend = substitute(end),
                         y = substitute(.levels),
                         yend = substitute(.levels)))
    if(!("color" %in% names(args)))
      p <- p + geom_segment(data = df.gaps, do.call("aes", args), color = color.def)
    else
      p <- p + geom_segment(data = df.gaps, do.call("aes", args))
  }
  if(geom == "single"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(data, which, geom = geom)
    message("Constructing graphics...")
    values(gr)$.levels <-  1
    ## drawing
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    p <- ggplot(df.cds)
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.4),
                         ymax = substitute(.levels + 0.4)))
    ## only for rectangle 
    if(!("color" %in% names(args)))
      p <- p + geom_rect(do.call("aes", args), color = color.def)
    else
      p <- p + geom_rect(do.call("aes", args))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.2),
                         ymax = substitute(.levels + 0.2)))
    if(!("color" %in% names(args)))
      p <- p + geom_rect(data = df.utr, do.call("aes", args), color = color.def)
    else
      p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    df.gaps <- df[df$type == "gap",]
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    args <- c(args, list(x = substitute(start), xend = substitute(end),
                         y = substitute(.levels),
                         yend = substitute(.levels)))
    if(!("color" %in% names(args)))
      p <- p + geom_segment(data = df.gaps, do.call("aes", args), color = color.def)
    else
      p <- p + geom_segment(data = df.gaps, do.call("aes", args))
  }
  p
})



## ======================================================================
##        For "BSgenome"
## ======================================================================
setMethod("qplot", c("BSgenome"), function(data,  name, ...,
                                           geom = c("text",
                                             "segment",
                                             "point",
                                             "rectangle")){
  ## first need to get sequence
  seqs <- getSeq(data, name, as.character = TRUE)
  seqs <- IRanges:::safeExplode(seqs)
  xs <- seq(start(name), length.out = width(name))
  df <- data.frame(x = xs, seqs = seqs)
  geom <- match.arg(geom)
  p <- ggplot(df, ...)
  baseColor <- getOption("biovizBase")$DNABasesColor
    ## baseColor <- unlist(baseColor)
  p <- switch(geom,
              text = {
                p + geom_text(aes(x = x, y = 0, label = seqs ,color = seqs))+
                  scale_color_manual(values = baseColor)
              },
              segment = {
                p + geom_segment(aes(x = x, y = -1,
                                   xend = x, yend = 1,
                                   color = seqs)) +
                                     scale_color_manual(values = baseColor)+
                                     scale_y_continuous(limits = c(-10, 10))
              },
              point = {
                p + geom_point(aes(x = x, y = 0, color = seqs))+
                  scale_color_manual(values = baseColor)
              },
              rectangle = {
                p + geom_rect(aes(xmin = x, ymin = -1,
                                   xmax = x+0.9, ymax = 1,
                                   color = seqs, fill = seqs)) +
                                     scale_y_continuous(limits = c(-10, 10))+
                                       scale_color_manual(values = baseColor)+
                                         scale_fill_manual(values = baseColor)
              })
  p <- p + ylab("")
  p
})

