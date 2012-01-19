## TODO: Priority this week til Wednesday
## 1. GRanges y for rectangle/segment, stat = "identity", y changed for free(done)
## 2. BSgenome(done)
## 3. Rle(done)
## 4. TranscriptDb(remove tx, change name)(done)
## 5. GappedAlignments
## 6. Bam/BamFile/character
setGeneric("autoplot", function(data, ...) standardGeneric("autoplot"))
## auto play need to check arguments and dispatch them to the right
## place
formals.qplot <- names(formals(qplot))

formals.facet_grid <- names(formals(facet_grid))
formals.facet_wrap <- names(formals(facet_wrap))
formals.facets <- union(formals.facet_grid, formals.facet_wrap)
formals.facets <- formals.facets[formals.facets != "..."]

## ======================================================================
##        For "Granges"
## ======================================================================

setMethod("autoplot", signature(data = "GRanges"), function(data, ...,
                                  legend = TRUE,
                                  xlab, ylab, main,
                                  facets, facet, 
                                  stat = c("identity", "coverage", "stepping"),
                                  geom = c("rectangle", "segment","alignment",
                                    "line","point", "polygon"),
                                  coord = "linear",
                                  rect.height = 0.4
                                  ){
  if(rect.height <= 0 | rect.height >= 0.5)
    stop("rect.height must be a value in (0,0.5)")
  data.back <- data
  formals.cur <- c("data", "stat", "geom", "coord", "facets", "legend",
                   "xlab", "ylab", "main", "rect.height")
  geom <- match.arg(geom)
  stat <- match.arg(stat)
  

  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.facets <- args[names(args) %in% formals.facets]
  args <- args[!(names(args) %in% formals.cur)]

  ## 
  if(geom %in% c("rectangle", "alignment", "segment" )){
    if(!("y" %in% names(args))){
      if(stat %in% c("identity", "coverage"))
        message("stat convert to stepping ")
      stat <- "stepping"
    }
  }

  
  ## check "x", must be one of "start", "end", "midpoint"
  if(!(geom %in% c("alignment", "segment", "rectangle"))){
    if("x" %in% names(args)){
      if(!(deparse(args$x) %in% c("start", "end", "midpoint")))
        stop("x must be one of start/end/midpoint without quote")
    }else{
      args <- c(args, list(x = substitute(midpoint)))
      message("use midpoint for x as default")
    }}
  
  ## check y
  if(geom %in% c("point", "line") & stat == "identity"){
    if(!("y" %in% names(args))){
      stop("Missing y for geom ", geom)
    }
  }
  
  ## color
  ## question: arbitrary color??
  ## I guess it's a no
  strandColor <- getOption("biovizBase")$strandColor
  
  ## check seqname
  seqname <- unique(as.character(seqnames(data)))
  data <- keepSeqlevels(data, seqname)
  
  ## start to deal with facet...
  allvars <- NULL
  grl <- NULL

  ## alias
  if(missing(facets)){
    if(!missing(facet)){
      facets <- facet
    }else{
      facets <- NULL
    }}
  ## get the right facets
  if(length(facets)){
    ## if facets is a GRanges object
    ## what's .id.name?should I put it here?
    if(is(facets, "GRanges")){
      ## args.facets <- c(args.facets, list(facets = substitute(~.bioviz.facetid)))
      args.facets$facets <- substitute(~.bioviz.facetid)
      ## ok, default is "free"
      if(!("scales" %in% names(args.facets)))
        args.facets$scales <- "free"
      facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                            TRUE, FALSE)
      if(facet.logic)
        facet <- do.call(facet_wrap, args.facets)
      else
        facet <- do.call(facet_grid, args.facets)
      
      data <- newDataAfterFacetByGr(data, facets)
      if(args.facets$facets == "~.bioviz.facetid")
        grl <- split(data, values(data)$.bioviz.facetid)
      if(args.facets$facets == "~seqnames")
        grl <- split(data, seqnames(data))
    }else{
      ## only allow simple case
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      allvars <- all.vars(as.formula(args.facets$facets))
      if(allvars[2] != "seqnames")
        stop("Column of facet formula can only be seqnames, such as . ~ seqnames,
              you can change row varaibles")
      if(allvars[1] %in% colnames(values(data))){
        facet <- do.call(facet_grid, c(args$facets, args.facets))
      }else{
        ## args.facet <- c(args.facet, list(facets = substitute(~seqnames)))
        args.facets <- c(args.facets, list(facets = substitute(~seqnames)))
        facet <- do.call(facet_wrap, args.facets)
      }
      grl <- split(data, seqnames(data))
    }}else{
        ## missing facets
        ## default is "fixed"
        if(!("scales" %in% names(args.facets)))
          args.facets <- c(args.facets, list(scales = "fixed"))
        if(length(seqname)){
          ## facet by default is by seqname(a good practice)
          facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                                TRUE, FALSE)
          if(facet.logic){
            args.facets <- c(args.facets, list(facets = substitute(~seqnames)))
            facet <- do.call(facet_wrap, args.facets)
          }else{
            args.facets <- c(args.facets, list(facets = substitute(.~seqnames)))
            facet <- do.call(facet_grid, args.facets)
          }
          grl <- split(data, seqnames(data))
        }}

  if(stat == "coverage"){
    data <- lapply(grl,
                   function(dt){
                     if(!is.null(allvars) &&
                        allvars[1] %in% colnames(values(data))){
                       ndt <- split(dt, values(dt)[,allvars[1]])
                       ## browser()

                       ## coverage(ranges(ndt),shift = as.list(-min(start(ndt))+1))
                       lst <-lapply(ndt, function(dt){
                         sts <- min(start(dt))
                         vals <- as.numeric(coverage(ranges(dt),
                                                     shift = -min(start(dt))+1))
                         seqs <- seq.int(from = sts,
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
    data <- do.call("rbind", data)
  }
  if(stat == "stepping"){
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
  }

  if(stat == "identity"){
    df <- as.data.frame(data)
  }


  ## color scheme 
  isStrand.color <- FALSE
  isStrand.fill <- FALSE
  ## default with no color
  if(("color" %in% names(args))){
    if(as.character(args$color) == "strand")
      isStrand.color <- TRUE
  }
  if(("fill" %in% names(args))){
    if(as.character(args$fill) == "strand")
      isStrand.fill <- TRUE
  }




  p <- switch(geom,
              line = {
                ## geom line with default stat: coverage
                df <- as.data.frame(data)
                if(stat == "coverage"){
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p <- p + geom_line(do.call("aes", args))
              }
                if(stat == "identity"){
                  df$midpoint <- (df$start+df$end)/2
                  p <- ggplot(df)
                  p <- p + geom_line(do.call("aes", args))
                }
                p
              },
              point = {
                df <- as.data.frame(data)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                p <- p + geom_point(do.call("aes", args))
                p
              },
              alignment = {
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                if("group" %in% names(args))
                  gpn <- as.character(args$group)
                else
                  gpn <- ".levels"
                args <- args[names(args) != "group"]
                ## if(show.gaps){
                  ## show.gaps turn on the connectors
                gps <- suppressWarnings(getGap(data, group.name = gpn))
                args.gaps <- args[!names(args) %in% c("x", "y",
                                                      "xend", "yend",
                                                      "label.type",
                                                      "label.size",
                                                      "label.color",
                                                      "size",
                                                      "fill",
                                                      "color",
                                                      "colour")]
                  ## temporily disable color/fill
                  args.gaps.extra <- args[names(args) %in%
                                          c("offset", "chevron.height")]
                  aes.lst <- do.call("aes", args.gaps)
                  gps.lst <- c(list(aes.lst), list(data = gps),
                               args.gaps.extra)
                  p <- p + do.call(geom_chevron, gps.lst)
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(.levels - rect.height),
                                     ymax = substitute(.levels + rect.height)))
                args.rect <- args[names(args) != "size"]
                p <- p + geom_rect(do.call("aes", args.rect))
                if(isStrand.color)
                  p <- p + scale_color_manual(values = strandColor)
                if(isStrand.fill)
                  p <- p + scale_fill_manual(values = strandColor)
                ## p <- p + opts(axis.text.y = theme_blank())
                ## p <- p + scale_y_continuous(breaks = df$.levels,
                ##                             labels = df$.levels)
                .df.lvs <- unique(df$.levels)
                .df.sub <- df[, c(".levels", gpn)]
                .df.sub <- .df.sub[!duplicated(.df.sub),]
                if(gpn != ".levels")
                  p <- p + scale_y_continuous(breaks = .df.sub$.levels,
                                              labels = as.character(.df.sub[, gpn]))
                else
                  p <- p + scale_y_continuous(breaks = NA)
                ##   p <- p + scale_y_continuous(breaks = .df.sub$.levels,
                ##                          labels = as.character(.df.sub[, gpn]))               
                ## }
                p
              },
              rectangle = {
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]


                if("group" %in% names(args))
                  gpn <- as.character(args$group)
                else
                  gpn <- ".levels"
                args <- args[names(args) != "group"]
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(.levels - rect.height),
                                     ymax = substitute(.levels + rect.height)))
                args.rect <- args[names(args) != "size"]
                p <- p + geom_rect(do.call("aes", args.rect))
                if(isStrand.color)
                  p <- p + scale_color_manual(values = strandColor)
                if(isStrand.fill)
                  p <- p + scale_fill_manual(values = strandColor)
                .df.lvs <- unique(df$.levels)
                .df.sub <- df[, c(".levels", gpn)]
                .df.sub <- .df.sub[!duplicated(.df.sub),]
                if(gpn != ".levels")
                  p <- p + scale_y_continuous(breaks = .df.sub$.levels,
                                              labels = as.character(.df.sub[, gpn]))
                else
                  p <- p + scale_y_continuous(breaks = NA)
                
                p
              },              
              segment = {
                p <- ggplot(df)
                if(stat == "stepping"){
                  args <- args[!(names(args) %in% c("x", "y"))]

                  gpn <- as.character(args$group)
                  args <- args[names(args) != "group"]
                  

                  args <- c(args, list(x = substitute(start),
                                       xend = substitute(end),
                                       y = substitute(.levels),
                                       yend = substitute(.levels)))
                }else{
                  args <- args[!(names(args) %in% c("x"))]

                  gpn <- as.character(args$group)
                  args <- args[names(args) != "group"]
                  

                  args <- c(args, list(x = substitute(start),
                                       xend = substitute(end)))
                  args$yend <- args$y
                }
                p <- p + geom_segment(do.call("aes", args)) 

                if(isStrand.color)
                  p <- p + scale_color_manual(values = strandColor)
                p
              },
              polygon = {
                df <- as.data.frame(data)
                if(stat == "coverage"){
                p <- ggplot(df)
                ## geom polygon: associated with default stat coverage
                ## remove x
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p <- p + geom_polygon(do.call("aes", args))
                p
                
              }
                if(stat == "identity"){
                  df$midpoint <- (df$start+df$end)/2
                  idx <- order(df$seqnames, df$start)
                  df <- df[idx, ]
                  p <- ggplot(df)
                  p <- p + geom_polygon(do.call("aes", args))
                }
                p
              }
              )
  if(!legend)
    p <- p + opts(legend.position = "none")
  
  if(missing(xlab)){
    chrs <- unique(seqnames(data.back))
    gms <- genome(data.back)
    gm <- unique(gms[chrs])
    chrs.tx <- paste(chrs, sep = ",")    
    if(is.na(gm)){
      xlab <- chrs.tx
    }else{
      gm.tx <- paste(gm)
      xlab <- paste(gm.tx,"::",chrs.tx, sep = "")      
    }
  }
    p <- p + xlab(xlab)
  ## tweak with default y lab
  if(missing(ylab)){
    if(stat != "identity"){
    if(stat == "coverage")
      ylab <- "Coverage"
    if(stat == "stepping" | geom == "alignment")
      ylab <- ""
    p <- p + ylab(ylab)
  }
  }else{
    p <- p + ylab(ylab)
  }
  
  ## if(stat == "stepping" | geom == "alignment")
  ##   p <- p + scale_y_continuous(breaks = NA)
  if(!missing(main))
    p <- p + opts(title = main)
  ## is this a good practice?
  p <- p + facet
  p
})

## ======================================================================
##        For "GRangesList"
## ======================================================================
setMethod("autoplot", "GRangesList", function(data, ...,
                                              geom = c("alignment", "rectangle")){
  ## make it easy, remove all the elementadata
  ## users could always coerce it to a GRanges
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  if(is.null(names(data)))
    nm.data <- as.character(seq(length(data)))
  else
    nm.data <- names(data)
  nms <- rep(nm.data, elementLengths(data))
  res <- lapply(1:length(data), function(i){
    vals <- values(data[i])
    n <- length(data[[i]])
    do.call("rbind", lapply(1:n, function(i) vals))
  })
  res <- do.call("rbind", res)
  gr <- unlist(data)
  values(gr) <- res
  values(gr)$.grl.name <- nms
  args <- args[!(names(args) %in% c("data", "group", "geom"))]
  args <- c(args, list(group = substitute(.grl.name),
                       data = gr,
                       geom = geom))
  p <- do.call(autoplot, args)
  p
})          

## ======================================================================
##        For "IRanges"
## ======================================================================

setMethod("autoplot", "IRanges", function(data, ...,
                                          legend = TRUE,
                                          xlab, ylab, main,
                                          facets, facet, 
                                          stat = c("identity", "coverage", "step"),
                                          geom = c("rectangle", "segment","alignment",
                                            "line","point", "polygon"),
                                          coord = "linear"){

  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "geom.engine",
                                    "data",  "legend"))]
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
  p
})


## ======================================================================
##        For "GappedAlignments"
## ======================================================================
setMethod("autoplot", "GappedAlignments", function(data, ...,
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
    gr.gaps <- getGap(gr.junction, "qname")
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
    p <- autoplot(gr)
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
setMethod("autoplot", "BamFile", function(data, ..., which,
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

    p <- do.call(autoplot, args)
    ## p <- autoplot(ga, ..., resize.extra = resize.extra)

  }
  if(geom %in% c("coverage.line", "coverage.polygon", "full")){
    ga <- readBamGappedAlignments(bf,
                                  param = ScanBamParam(which = which),
                                  use.name = TRUE)
    gr <- biovizBase:::fetch(ga, type = "raw")
    p <- autoplot(gr, geom = geom)
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
setMethod("autoplot", "character", function(data, ...,
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
  autoplot(bf,  ..., which = which, model = model, bsgenome = bsgenome,
           show.coverage = show.coverage,
           geom = geom, resize.extra = reszie.extra)
})

## FIX THIS first:
## ======================================================================
##        For "TranscriptDb"(Genomic Structure)
## ======================================================================
setMethod("autoplot", "TranscriptDb", function(data, which, ...,
                                               xlab, ylab, main,                           
                                               geom = c(
                                                 "gene",
                                                 "reduced_gene")){
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  .args <- args[!(names(args) %in% c("geom", "which","data"))]
  if(geom == "gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(data, which)
    ## gr <- fetch(data, which)
    message("Constructing graphics...")
    values(gr)$.levels <-  as.numeric(values(gr)$tx_id)
    ## drawing
    ## hard coded width of rectangle
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    p <- ggplot(df.cds)
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start),
                         xmax = substitute(end),
                         ymin = substitute(.levels - 0.4),
                         ymax = substitute(.levels + 0.4)))
    ## only for rectangle 
    ## if(!("color" %in% names(args)))
    ##   p <- p + geom_rect(do.call("aes", args), color = color.def)
    ## else
    p <- p + geom_rect(do.call("aes", args))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start),
                         xmax = substitute(end),
                         ymin = substitute(.levels - 0.2),
                         ymax = substitute(.levels + 0.2)))
    p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    df.gaps <- gr[values(gr)$type == "gap"] 
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    p <- p + geom_chevron(data = df.gaps, do.call("aes", args))
    .df.lvs <- unique(df$.levels)
    .df.sub <- df[, c(".levels", "tx_id")]
    .df.sub <- .df.sub[!duplicated(.df.sub),]
    p <- p + scale_y_continuous(breaks = .df.sub$.levels, labels = .df.sub$tx_id)
    p
  }
  if(geom == "reduced_gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(data, which, type = "single")
    ## gr <- fetch(data, which, type = "single")
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
    p <- p + geom_rect(do.call("aes", args))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.2),
                         ymax = substitute(.levels + 0.2)))
    p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    ## df.gaps <- df[df$type == "gap",]
    gr.rr <- reduce(ranges(gr[(values(gr)$type %in%  c("utr", "cds"))]))
    df.gaps <- gaps(gr.rr, start = min(start(gr.rr)), end = max(end(gr.rr)))
    chrs <- unique(as.character(seqnames(gr)))
    df.gaps <- GRanges(chrs, df.gaps)
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    .df.lvs <- unique(df$.levels)
    p <- p + scale_y_continuous(breaks = .df.lvs, labels = .df.lvs)
    p <- p + geom_chevron(data = df.gaps, do.call("aes", args))
  }
  ## if(geom == "tx"){
  ##   exons.tx <- exonsBy(data, by = "tx")
  ##   exons <- subsetByOverlaps(exons.tx, which)
  ##   args <- c(.args, list(data = exons))
  ##   p <- do.call(autoplot, args)
  ## }
  if(missing(xlab)){
    chrs <- unique(seqnames(which))
    gms <- genome(data)
    gm <- unique(gms[chrs])
    chrs.tx <- paste(chrs, sep = ",")
    if(is.na(gm)){
      xlab <- chrs.tx
    }else{
      gm.tx <- paste(gm)
      xlab <- paste(gm.tx,"::",chrs.tx, sep = "")      
    }
  }
  p <- p + xlab(xlab)
  ## TODO: need to automatically use name as y
  if(missing(ylab)){
      p <- p + ggplot2::ylab("")
  }else{
    p <- p + ylab(ylab)
  }
  ## if(stat == "stepping" | geom == "alignment")
  ##   p <- p + scale_y_continuous(breaks = NA)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})



## ======================================================================
##        For "BSgenome"
## ======================================================================
setMethod("autoplot", c("BSgenome"), function(data,  which, ...,
                                              xlab, ylab, main,
                                              geom = c("text",
                                                "segment",
                                                "point",
                                                "rectangle")){
  dots <- list(...)
  if(!"color" %in% names(dots))
    isDNABaseColor <- TRUE
  else
    isDNABaseColor <- FALSE
  seqs <- getSeq(data, which, as.character = TRUE)
  seqs <- IRanges:::safeExplode(seqs)
  xs <- seq(start(which), length.out = width(which))
  df <- data.frame(x = xs, seqs = seqs)
  geom <- match.arg(geom)
  ## dots.wocolor <- dots[names(dots) != "color"]
  ## p <- do.call(ggplot, c(list(data = df), dots.wocolor))
  p <- ggplot(data = df, ...)
  baseColor <- getOption("biovizBase")$DNABasesColor
  if(!isDNABaseColor)
    cols <- dots$color
  p <- switch(geom,
              text = {
                if(isDNABaseColor)
                  p + geom_text(aes(x = x, y = 0, label = seqs ,color = seqs))+
                    scale_color_manual(values = baseColor)
                else
                  p + geom_text(aes(x = x, y = 0, label = seqs), color = cols)

              },
              segment = {
                if(isDNABaseColor)                
                  p + geom_segment(aes(x = x, y = -1,
                                       xend = x, yend = 1,
                                       color = seqs)) +
                                         scale_color_manual(values = baseColor)+
                                           scale_y_continuous(limits = c(-10, 10))
                else
                  p + geom_segment(aes(x = x, y = -1,
                                       xend = x, yend = 1), color = cols)+
                                   scale_y_continuous(limits = c(-10, 10))
                                 },
                point = {
                  if(isDNABaseColor)                                
                    p + geom_point(aes(x = x, y = 0, color = seqs))+
                      scale_color_manual(values = baseColor)
                  else
                    p + geom_point(aes(x = x, y = 0), color = cols)
                  
                },
                rectangle = {
                  if(isDNABaseColor)                                                
                    p + geom_rect(aes(xmin = x, ymin = -1,
                                      xmax = x+0.9, ymax = 1,
                                      color = seqs, fill = seqs)) +
                                        scale_y_continuous(limits = c(-10, 10))+
                                          scale_color_manual(values = baseColor)+
                                            scale_fill_manual(values = baseColor)
                  else
                    p + geom_rect(aes(xmin = x, ymin = -1,
                                      xmax = x+0.9, ymax = 1), color = cols, fill = cols) +
                                        scale_y_continuous(limits = c(-10, 10))

                })
  if(missing(xlab)){
    chrs <- unique(seqnames(which))
    gms <- genome(data)
    gm <- unique(gms[chrs])
    chrs.tx <- paste(chrs, sep = ",")    
    if(is.na(gm)){
      xlab <- chrs.tx
    }else{
      gm.tx <- paste(gm)
      xlab <- paste(gm.tx,"::",chrs.tx, sep = "")      
    }
  }
    p <- p + xlab(xlab)
  ## tweak with default y lab
  if(missing(ylab)){
      ylab = ""
  }
  p <- p + ylab(ylab)
  ## if(stat == "stepping" | geom == "alignment")
  p <- p + scale_y_continuous(breaks = NA)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})


## ======================================================================
##        For "Rle"
## ======================================================================
## geom: ... color = I("red"), doesn't work
setMethod("autoplot", "Rle", function(data, lower, ...,
                                      xlab = "x", 
                                      ylab = "y", main,
                                      color, size, shape, alpha,
                                      geom = c("point", "line", "segment"),
                                      type = c("raw", "viewMaxs","viewMins",
                                        "viewSums", "viewMeans")){


  geom <- match.arg(geom)
  type <- match.arg(type)
  if(type %in% c("viewMaxs", "viewMeans", "viewMins", "viewSums") && missing(lower))
    stop("please at least specify the value of lower, you could pass
          extra parameters to slice too")

  args.dots <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.slice <- args.dots[names(args.dots) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")] 
  args <- args.dots[names(args.dots) %in% c("size", "shape", "color", "alpha", "geom")]
  if(!"geom" %in% names(args))
    args$geom <- geom  
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
  p <- do.call(qplot, args)
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})


## ======================================================================
##        For "RleList"
## ======================================================================
## 1. facet by list
## 2. support as what has been supported in Rle.
setMethod("autoplot", "RleList", function(data, lower, ...,
                                      xlab = "x", 
                                      ylab = "y", main,
                                          size, shape, color, alpha, 
                                          facetByRow = TRUE,
                                          geom = c("point", "line", "segment"),
                                          type = c("raw", "viewMaxs","viewMins",
                                            "viewSums", "viewMeans")){

  geom <- match.arg(geom)
  type <- match.arg(type)
  if(type %in% c("viewMaxs", "viewMeans", "viewMins", "viewSums") && missing(lower))
    stop("please at least specify the value of lower, you could pass
          extra parameters to slice too")

  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])  
  args <- args[names(args) %in% c("size", "shape", "color", "alpha", "geom")]
  if(!"geom" %in% names(args))
    args$geom <- geom
  
  ## args <- c(geom = geom, args)
  ## args.dots <- list(...)
  args.slice <- args[names(args) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")]
  ## args <- args.dots[!(names(args) %in%
  ##                         c("upper", "includeLower",
  ##                           "includeUpper", "rangesOnly"))]
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
  p <- do.call(qplot, args)
    p <- p + xlab(xlab)
    p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})

