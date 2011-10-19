## ======================================================================
##        For "Granges"
## ======================================================================
setMethod("qplot", signature(data = "data.frame"), function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  do.call(ggplot2::qplot, args)  
})

setMethod("qplot", signature(data = "matrix"), function(data, ...){
  ggplot2::qplot(data, ...)
})

setMethod("qplot", signature(data = "integer"), function(data, ...){
  ggplot2::qplot(data, ...)
})

setMethod("qplot", signature(data = "numeric"), function(data, ...){
  ggplot2::qplot(data, ...)
})

## setMethod("qplot", signature(data = "numeric"), function(data, ...){
##   args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
##   do.call(ggplot2::qplot, args)
## })

setMethod("qplot", signature(data = "GRanges"), function(data, x, y,...,
                               facet_gr,
                               legend = TRUE,
                               show.coverage = TRUE,
                               show.gaps = FALSE,
                               show.label = FALSE,
                               geom = c("full", "line","point",
                                 "segment", "coverage.line", "coverage.polygon")){
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  ## args <- as.list(match.call(expand.dots = FALSE)[-1])
  args.facet <- args[names(args) %in% c("nrow", "ncol", "scales", "space")]
  args <- args[!(names(args) %in% c( "geom", "data", "legend", "nrow", "ncol",
                                    "facet_gr", 
                                    "show.coverage", "show.gaps", "show.label"))]
  if("freq" %in% names(args)){
    freq <- args$freq
    args <- args[names(args) != "freq"]
  }
    
  ## check "x", must be one of "start", "end", "midpoint"??
  if(!(geom %in% c("full", "segment"))){
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
      stop("Missing y for geom ", geom)
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
    if(!"facets" %in% names(args)){
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
        allvars <- all.vars(as.formula(args$facets))
        if(allvars[2] != "seqnames")
          stop("Column of facet formula can only be seqnames, such as . ~ seqnames,
              you can change row varaibles")
        if(allvars[1] %in% colnames(values(data)) 
           ## !(geom %in% c("coverage.line", "coverage.polygon"))
           ){
          facet <- do.call(facet_grid, c(args$facets, args.facet))
        }else{
          args.facet <- c(args.facet, list(facets = substitute(~seqnames)))
          facet <- do.call(facet_wrap, args.facet)
        }
      }}else{
        args.facet <- c(args.facet, list(facets = substitute(~.id.name)))
        args.facet$scales <- "free"
        facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facet)),
                              TRUE, FALSE)
        if(facet.logic)
         facet <- do.call(facet_wrap, args.facet)
        else
          facet <- do.call(facet_grid, args.facet)
      }
  ## seqlevels(data) <- seqname
  data <- keepSeqlevels(data, seqname)
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
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                if(!("color" %in% names(args))){
                  if("fill" %in% names(args))
                    args <- c(args, list(color = args$fill))
                  else
                    args <- c(args, list(color = substitute(strand),
                                         fill = substitute(strand)))
                }
                gpn <- as.character(args$group)
                args <- args[names(args) != "group"]
                if(show.gaps){
                  gps <- getGap(data, group.name = gpn)
                  args.gaps <- args[!names(args) %in% c("x", "y",
                                                        "xend", "yend",
                                                        "label.type",
                                                        "label.size",
                                                        "label.color",
                                                        "size")]
                  args.gaps.extra <- args[names(args) %in%
                                          c("offset", "chevron.height")]
                  ## if(!("offset" %in% names(args.gaps.extra)))
                  ##   args.gaps.extra <- c(args.gaps.extra, list(offset = 0))
                  aes.lst <- do.call("aes", args.gaps)
                  gps.lst <- c(list(aes.lst), list(data = gps),
                               args.gaps.extra)
                  p <- p + do.call(geom_chevron, gps.lst)
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
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(.levels - 0.4),
                                     ymax = substitute(.levels + 0.4)))
                args.rect <- args[names(args) != "size"]
                p <- p + geom_rect(do.call("aes", args.rect)) +
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

                gpn <- as.character(args$group)
                args <- args[names(args) != "group"]
                
                p <- ggplot(df)
                if(show.gaps){
                  gps <- getGap(data, group.name = gpn)
                  args.gaps <- args[!names(args) %in% c("x", "y",
                                                        "xend", "yend",
                                                        "label.type",
                                                        "label.size",
                                                        "label.color",
                                                        "size")]
                  args.gaps.extra <- args[names(args) %in%
                                          c("offset", "chevron.height")]
                  if(!("offset" %in% names(args.gaps.extra)))
                    args.gaps.extra <- c(args.gaps.extra, list(offset = 0))
                  aes.lst <- do.call("aes", args.gaps)
                  gps.lst <- c(list(aes.lst), list(data = gps),
                               args.gaps.extra)
                  p <- p + do.call(geom_chevron, gps.lst)
                }
                if(show.label){
                    label.type <- args$label.type
                    label.size <- args$label.size
                    label.color <- args$label.color
                    label.gr <- getModelRange(data, group.name = gpn)
                    
                    if(label.type == "count")
                      values(label.gr)$.label <-
                        freq[as.character(values(label.gr)$.label)]
                    
                  args.label <- c(args, list(x = substitute((start + end)/2),
                                       y = substitute(.levels + 0.3),
                                       label = substitute(.label)))
                  args.label <- args.label[names(args.label) != "size"]
                  p <- p + geom_text(data = as.data.frame(label.gr),
                                     do.call(aes, args.label),
                                     size = label.size, color = label.color)
                }
                args <- c(args, list(x = substitute(start),
                                     xend = substitute(end),
                                     y = substitute(.levels),
                                     yend = substitute(.levels)))
                p + geom_segment(do.call("aes", args)) +
                  scale_color_manual(values = strandColor) +
                          scale_fill_manual(values = strandColor)                    
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
               data <- lapply(grl,
                               function(dt){
                                    if(!is.null(allvars) &&
                                   allvars[1] %in% colnames(values(data))){
                              ndt <- split(dt, values(dt)[,allvars[1]])
                              lst <-lapply(ndt, function(dt){
                              vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 if(length(unique(values(dt)$.id.name)))                
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1],
                                              .id.name = unique(values(dt)$.id.name))
                                 else
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                              res[, allvars[1]] <- rep(unique(values(dt)[,allvars[1]]),
                                                       nrow(res))
                              res
                              })
                              do.call(rbind, lst)
                            }else{
                                vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 if(length(unique(values(dt)$.id.name)))                
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1],
                                              .id.name = unique(values(dt)$.id.name))
                                 else
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                              }
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
                                 if(!is.null(allvars) &&
                                   allvars[1] %in% colnames(values(data))){
                              ndt <- split(dt, values(dt)[,allvars[1]])
                              lst <-lapply(ndt, function(dt){
                                 vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 seqs <- c(seqs[1],seqs,tail(seqs, 1))
                                 vals <- c(0, vals, 0)
                                 if(length(unique(values(dt)$.id.name)))
                                  res <- data.frame(vals = vals, seqs = seqs,
                                              .id.name = unique(values(dt)$.id.name),
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                                 else
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                        res[, allvars[1]] <- rep(unique(values(dt)[,allvars[1]]),
                                                       nrow(res))
                              res                                 
                               })
                              do.call(rbind, lst)}else{
                                vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                                 seqs <- seq.int(from = min(start(dt)),
                                                 length.out = length(vals))
                                 seqs <- c(seqs[1],seqs,tail(seqs, 1))
                                 vals <- c(0, vals, 0)
                                 if(length(unique(values(dt)$.id.name)))
                                  res <- data.frame(vals = vals, seqs = seqs,
                                              .id.name = unique(values(dt)$.id.name),
                                              seqnames =
                                              as.character(seqnames(dt))[1])
                                 else
                                   res <- data.frame(vals = vals, seqs = seqs,
                                              seqnames =
                                              as.character(seqnames(dt))[1])                                
                              }})
                df <- do.call("rbind", data)
                p <- ggplot(df)
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
  p <- p + xlab(paste("Genomic Coordinates"))
  if(length(seqname)>1)
    p <- p + facet
  p
})

## ======================================================================
##        For "GRangesList"
## ======================================================================
## FIXME:
setMethod("qplot", "GRangesList", function(data, ..., freq, show.label = FALSE,
                                           show.gaps = TRUE,
                                           scale.size = c(5, 17),
                                           label.type = c("name", "count"),
                                           label.size = 5,
                                           label.color = "black"){
  label.type <- match.arg(label.type)
  args <- list(...)
  ## args <- args[names(args) != scale.size]
  if(is.null(names(data)))
    nm.data <- as.character(seq(length(data)))
  else
    nm.data <- names(data)
  nms <- rep(nm.data, elementLengths(data))
  gr <- unlist(data)
  values(gr)$.grl.name <- nms
  if(!missing(freq)){
    freqs <- rep(freq[names(data)], elementLengths(data))
    values(gr)$.freq <- freqs
    args <- c(args, list(data = gr,
                         group = substitute(.grl.name),
                         show.label = show.label,
                         show.gaps = show.gaps,
                         size = substitute(.freq),
                         label.type = label.type,
                         label.size = label.size,
                         label.color = label.color,
                         freq = freq))
    args <- c(args, list(geom = "segment"))    
  }else{
    args <- c(args, list(data = gr,
                         group = substitute(.grl.name),
                         show.label = show.label,
                         show.gaps = show.gaps,
                         label.type = label.type,
                         label.size = label.size,
                         label.color = label.color))                         
    args <- c(args, list(geom = "full"))
  }
  p <- do.call(qplot, args)
  p <- p + scale_size(to = scale.size) + ylab("")
  p
})

## ======================================================================
##        For "IRanges"
## ======================================================================

setMethod("qplot", "IRanges", function(data, ...,
                                       legend = TRUE,
                                       geom = c("full", "segment", 
                                         "coverage.line", "coverage.polygon")){
  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "geom.engine",
                                    "data",  "legend"))]
  ## check "x", must be one of "start", "end", "midpoint"
  ## if("x" %in% names(args) && !missing(x)){
  ##   if(!(deparse(args$x) %in% c("start", "end", "midpoint")))
  ##     stop("x must be one of start/end/mipoint without quote")
  ## }else{
  ##   args <- c(args, list(x = substitute(midpoint)))
  ##   message("use start for x as default")
  ## }
  geom <- match.arg(geom)
  p <- switch(geom,
              full = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(data))
                p <- ggplot(df)
                args <- args[names(args) != "y"]
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(y - 0.4),
                                     ymax = substitute(y + 0.4)))
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
  args <- args[!(names(args) %in% c("geom", "which", "show.junction",
                                    "show.pair"))]
  if(!missing(which))
    gr <- biovizBase:::fetch(data, which)
  else
    gr <- biovizBase:::fetch(data)
  if(geom == "gapped.pair"){
  gr.junction <- gr[values(gr)$junction == TRUE]
  ## grl <- split(gr.junction, values(gr.junction)$read.group)
  ## get gaps??
  ## ir.gaps <- unlist(gaps(ranges(grl)))
  ## .lvs <- values(gr.junction)$.levels[match(names(ir.gaps),
  ##                                         values(gr.junction)$read.group)]
  ## seqs <- unique(as.character(seqnames(gr.junction)))
  gr.gaps <- getGap(gr.junction, "qname")
  ## gr.gaps <- GRanges(seqs, ir.gaps, .levels = .lvs)
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
    scale_color_manual(values = strandColor) +
      scale_fill_manual(values = strandColor)
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
                                       bsgenome,
                                       resize.extra = 10,
                                       show.coverage = TRUE,
                                       geom = c("gapped.pair",
                                         "full",
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
    message("plotting...")
    args.ga <- args[names(args) %in% "show.junction"]
    args <- c(args.ga, list(data = ga))

    p <- do.call(qplot, args)
    ## p <- qplot(ga, ..., resize.extra = resize.extra)

  }
  if(geom %in% c("coverage.line", "coverage.polygon", "full")){
    ga <- readBamGappedAlignments(bf,
                                  param = ScanBamParam(which = which),
                                  use.name = TRUE)
    gr <- biovizBase:::fetch(ga, type = "raw")
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
  p + ylab("")
})
## ======================================================================
##  For "character" need to check if it's path including bamfile or not
## ======================================================================
setMethod("qplot", "character", function(data, ...,
                                         which, 
                                         bsgenome,
                                         resize.extra = 10,
                                         show.coverage = TRUE,
                                         geom = c("gapped.pair",
                                           "full",
                                           "coverage.line",
                                           "coverage.polygon",
                                           "mismatch.summary")){
  geom <- match.arg(geom)
  if(tools::file_ext(data) == "bam")
    bf <- BamFile(data)
  else
    stop("Please pass a bam file path")
  qplot(bf,  ..., which = which, model = model, bsgenome = bsgenome,
        show.coverage = show.coverage,
        geom = geom, resize.extra = reszie.extra)
})


## ======================================================================
##        For "TranscriptDb"(Genomic Structure)
## ======================================================================
setMethod("qplot", "TranscriptDb", function(data, which, ...,
                                            geom = c(
                                              "full",
                                              "single",
                                              "tx")){
  ## FIXME: need to be in biovizBase
  ## colored by strand?
  color.def <- "black"
  fill.def <- "black"
  ## strandColor <- getBioColor("STRAND")
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
    ## df.gaps <- df[df$type == "gap",]
    df.gaps <- gr[values(gr)$type == "gap"] 
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    ## args <- c(args, list(x = substitute(start), xend = substitute(end),
    ##                      y = substitute(.levels),
    ##                      yend = substitute(.levels)))
    if(!("color" %in% names(args)))
      p <- p + geom_chevron(data = df.gaps, do.call("aes", args), color = color.def)
    else
      p <- p + geom_chevron(data = df.gaps, do.call("aes", args))
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
    ## df.gaps <- df[df$type == "gap",]
    gr.rr <- reduce(ranges(gr[(values(gr)$type %in%  c("utr", "cds"))]))
    df.gaps <- gaps(gr.rr, start = min(start(gr.rr)), end = max(end(gr.rr)))
    chrs <- unique(as.character(seqnames(gr)))
    df.gaps <- GRanges(chrs, df.gaps)
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    if(!("color" %in% names(args)))
      p <- p + geom_chevron(data = df.gaps, do.call("aes", args), color = color.def)
    else
      p <- p + geom_chevron(data = df.gaps, do.call("aes", args))
  }
  if(geom == "tx"){
    exons.tx <- exonsBy(data, by = "tx")
    exons <- subsetByOverlaps(exons.tx, which)
    args <- c(.args, list(data = exons))
    p <- do.call(qplot, args)
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


## ======================================================================
##        For "Rle"
## ======================================================================
# geom: line/point
# simply smoothing
setMethod("qplot", "Rle", function(data, lower, ...,
                                   size, shape, color, alpha,
                                   xlab = "x", ylab = "y",
                                   geom = c("point", "line", "segment"),
                                   type = c("raw", "viewMaxs","viewMins",
                                     "viewSums", "viewMeans")){


  geom <- match.arg(geom)
  type <- match.arg(type)
  if(type %in% c("viewMaxs", "viewMeans", "viewMins", "viewSums") && missing(lower))
    stop("please at least specify the value of lower, you could pass
          extra parameters to slice too")

  ## args
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])  
  args <- args[names(args) %in% c("size", "shape", "color", "alpha", "geom")]
  if(!"geom" %in% names(args))
    args$geom <- geom
  ## args <- c(geom = geom, args)
  args.dots <- list(...)
  args.slice <- args.dots[names(args.dots) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")]
  if(!missing(lower))
    args.slice <- c(list(x = data,
                         lower = lower), args.slice)
  df <- switch(type,
              raw = {
               x <- 1:length(data)                
               y <- as.numeric(data)
               data.frame(x = x, y = y)
              },
              viewMaxs = {
                vs <- do.call(slice, args.slice)
                x <- viewWhichMaxs(vs)                
                y <- viewMaxs(vs)
                data.frame(x = x, y = y)
              },
              viewMins = {
                vs <- do.call(slice, args.slice)
                x <- viewWhichMins(vs)                
                y <- viewMins(vs)
                data.frame(x = x, y = y)
              },
              viewSums = {
                vs <- do.call(slice, args.slice)
                x <- start(vs) + width(vs)/2
                y <- viewSums(vs)
                data.frame(x = x, y = y)                
              },
              viewMeans = {
                vs <- do.call(slice, args.slice) 
                x <- start(vs) + width(vs)/2
                y <- viewMeans(vs)
                data.frame(x = x, y = y)                
              })

  ## df <- data.frame(x = x, y = y)
  if(args$geom == "segment")
    args <- c(list(data = df,
                   x = substitute(x),
                   xend = substitute(x),
                   y = 0,
                   yend = substitute(y)),
              args)
  else
    args <- c(list(data = df,
                   x = substitute(x),
                   y = substitute(y)),
              args)
  p <- do.call(ggplot2::qplot, args)
  p <- p + xlab(xlab) + ylab(ylab)
  p
})
                                     

## ======================================================================
##        For "RleList"
## ======================================================================
## 1. facet by list
## 2. support as what has been supported in Rle.
setMethod("qplot", "RleList", function(data, lower, ...,
                                   size, shape, color, alpha,
                                       facetByRow = TRUE,
                                   xlab = "x", ylab = "y",
                                   geom = c("point", "line", "segment"),
                                   type = c("raw", "viewMaxs","viewMins",
                                     "viewSums", "viewMeans")){

  geom <- match.arg(geom)
  type <- match.arg(type)
  if(type %in% c("viewMaxs", "viewMeans", "viewMins", "viewSums") && missing(lower))
    stop("please at least specify the value of lower, you could pass
          extra parameters to slice too")

  ## args
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])  
  args <- args[names(args) %in% c("size", "shape", "color", "alpha", "geom")]
  if(!"geom" %in% names(args))
    args$geom <- geom
  
  ## args <- c(geom = geom, args)
  args.dots <- list(...)
  args.slice <- args.dots[names(args.dots) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")]
  if(!missing(lower))
    args.slice <- c(list(x = data,
                         lower = lower), args.slice)

  df <- switch(type,
              raw = {
               x <- do.call(c,lapply(elementLengths(data),function(n) 1:n))
               y <- as.numeric(unlist(data))
               if(is.null(names(data)))
                 nms <- rep(1:length(data), times = elementLengths(data))
               else
                 nms <- rep(names(data), times = elementLengths(data))
               data.frame(x = x, y = y, listName = nms)
              },
              viewMaxs = {
                vs <- do.call(slice, args.slice)
                x <- viewWhichMaxs(vs)
                y <- viewMaxs(vs)
               if(is.null(names(x)))
                 nms <- rep(1:length(x), times = elementLengths(x))
               else
                 nms <- rep(names(x), times = elementLengths(x))
                data.frame(x = unlist(x), y = unlist(y), listName = nms)
              },
              viewMins = {
                vs <- do.call(slice, args.slice)
                x <- viewWhichMins(vs)                
                y <- viewMins(vs)
               if(is.null(names(x)))
                 nms <- rep(1:length(x), times = elementLengths(x))
               else
                 nms <- rep(names(x), times = elementLengths(x))
                data.frame(x = unlist(x), y = unlist(y), listName = nms)                
              },
              viewSums = {
                vs <- do.call(slice, args.slice)
                x <- start(vs) + width(vs)/2
               if(is.null(names(x)))
                 nms <- rep(1:length(x), times = elementLengths(x))
               else
                 nms <- rep(names(x), times = elementLengths(x))
                y <- viewSums(vs)
                data.frame(x = unlist(x), y = unlist(y), listName = nms)                
              },
              viewMeans = {
                vs <- do.call(slice, args.slice) 
                x <- start(vs) + width(vs)/2
               if(is.null(names(x)))
                 nms <- rep(1:length(x), times = elementLengths(x))
               else
                 nms <- rep(names(x), times = elementLengths(x))
                y <- viewMeans(vs)
                data.frame(x = unlist(x), y = unlist(y), listName = nms)                
              })

  if(facetByRow)
    facets <- listName ~ .
  else
    facets <- . ~ listName
  rm("x", "y")
  if(args$geom == "segment")
    args <- c(list(data = df,
                   facets = facets,
                   x = substitute(x),
                   xend = substitute(x),
                   y = 0,
                   yend = substitute(y)),
              args)
  else
    args <- c(list(data = df,
                   facets = facets,                   
                   x = substitute(x),
                   y = substitute(y)),
              args)
  
  p <- do.call(ggplot2::qplot, args)
  p <- p + xlab(xlab) + ylab(ylab)
  p
})
                                     
