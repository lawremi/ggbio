setGeneric("autoplot")

formals.qplot <- getFormalNames(qplot)
formals.facet_grid <- getFormalNames(facet_grid)
formals.facet_wrap <- getFormalNames(facet_wrap)
formals.facets <- union(formals.facet_grid, formals.facet_wrap)

.ggbio.geom <- c("rect", "chevron", "alignment", "arrowrect", "arrow", "segment", "arch")
.ggbio.stat <- c("identity", "coverage", "stepping", "aggregate", "table",
                 "gene", "mismatch")

## ======================================================================
##        For "Granges"
## ======================================================================

setMethod("autoplot", "GRanges", function(object, ...,
                                          xlab, ylab, main,
                                          truncate.gaps = FALSE,
                                          truncate.fun = NULL,
                                          ratio = 0.0025,
                                          space.skip = 0.1,
                                          legend = TRUE,
                                          geom = NULL,
                                          stat = NULL,
                                          coord = c("default", "genome", "truncate_gaps"),
                                          layout = c("linear", "karyogram", "circle")
                                          ){

  coord <- match.arg(coord)
  args <- list(...)
  
  if(coord == "genome"){
    object <- transformToGenome(object, space.skip = space.skip)
  }
  
  formals.cur <- c("object", "stat", "geom", "legend",
                   "xlab", "ylab", "main")

  ## truncate
  if(truncate.gaps){
    if(is.null(truncate.fun)){
      object.s <- reduce(object, ignore.strand = TRUE)
      truncate.fun <- shrinkageFun(gaps(object.s, min(start(object.s)), max(end(object.s))),
                                   maxGap(gaps(object.s, min(start(object.s)), max(end(object.s))),
                                          ratio = ratio))
    }
    object <- truncate.fun(object)
  }
  ## ------------------------------
  ## geom/stat check
  ## ------------------------------
  if(is.null(stat)){
    if(is.null(geom)){
      geom <- .ggbio.geom[1]
    }
  }else{
      if(!is.null(geom)){
        args$geom <- geom
      }
  }

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)

  if((!is.null(geom) && geom %in% .ggbio.geom) |
     (!is.null(stat) && stat %in% .ggbio.stat)){
     args.non$data <- object
  }else{
    args.non$data <- fortify(object)
    if(!"x" %in% names(args.aes))
      args.aes$x <- as.name("midpoint")
  }
  
  ## ------------------------------
  ## layout check
  ## ------------------------------
  layout <- match.arg(layout)
  ## use the default x
  ## since some of the geom or stat are not fully supported by all layout
  if(layout == "linear"){
  ## ------------------------------
  ##   get the right function
  ## ------------------------------
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    .fun <- getDrawFunFromGeomStat(geom, stat)
    p <- list(do.call(.fun, args.res))
    if(!legend)
      p <- c(p, list(opts(legend.position = "none")))


    if(missing(xlab)) {
      chrs <- unique(as.character(seqnames(object)))
      gms <- genome(object)
      gm <- gms[chrs]
      xlab <- paste(ifelse(is.na(gm), chrs, paste0(gm, "::", chrs)),
                    collapse = ",")
    }
    p <- c(p, list(xlab(xlab)))
    ## tweak with default y lab
    if(!missing(ylab))
      p <- c(p,list(ylab(ylab)))
    if(!missing(main))
      p <- c(p, list(opts(title = main)))
    
    p <- ggplot() + p
  }  
  if(layout == "karyogram"){
    p <- plotStackedOverview(object, ...,  geom = geom)
    ## FIXME: xlab/ylab/main
  }
  if(layout == "circle"){
    p <- ggplot() + layout_circle(object, ..., geom = geom)
  }
  ## test scale
  if(is_coord_truncate_gaps(object) | is_coord_genome(object)){
    ss <- getXScale(object)
    p <- p + scale_x_continuous(breaks = ss$breaks,
                                labels = ss$labels)
  }
  p
})

## ======================================================================
##        For "GRangesList"
## ======================================================================
setMethod("autoplot", "GRangesList", function(object, ...,
                                              xlab, ylab, main,
                                              indName = "grl_name",
                                              geom = NULL, stat = NULL,
                                              type = c("none", "sashimi"),
                                              coverage.col = "gray50",
                                              coverage.fill = coverage.col,
                                              group.selfish = FALSE,
                                              arch.offset = 1.3){

  type <- match.arg(type)
  args <- list(...)

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)

  args.non$group.selfish <- group.selfish
  if(type == "none")  {
    ## geom <- match.arg(geom)
    if(is.null(geom) & is.null(stat))
      geom <- "alignment"
    args.non$geom <- geom
    gr <- flatGrl(object, indName)
    ## default or not?
    ## args.aes$group <- substitute(.grl.name)
    if(!"group" %in% names(args.aes))
      args.aes$group <- as.name(indName)
    aes.res <- do.call(aes, args.aes)
    args.non$object <- gr
    args.res <- c(args.non, list(aes.res))
    p <- do.call(autoplot, args.res)
    p
  }
  if(type == "sashimi"){
    gps <- psetdiff(unlist(range(object), use.names=FALSE), object)
    ## coverage
    gr <- flatGrl(object, indName)
    max.h <- max(coverage(gr)) * arch.offset
    gps.gr <- unlist(gps)
    if(!"color" %in% names(args.non))
      args.non$color <- "steelblue"
    p <- ggplot() +
      do.call(stat_table, c(list(data = gps.gr, geom = "arch", max.height = max.h),
                                 list(aes(size = score)),args.non)) +
      do.call(stat_coverage, c(list(data = gr),
                               list(fill = coverage.fill, color = coverage.col)))
  }
  if(!missing(xlab))
    p <- p + xlab(xlab)
  if(!missing(ylab))
    p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})          

## ======================================================================
##        For "IRanges"
## ======================================================================

setMethod("autoplot", "IRanges", function(object, ..., xlab, ylab, main){
  ## ok, for simple impmlementation, let's make it a GRanges.....:)
  df <- values(object)
  values(object) <- NULL
  gr <- GRanges("chr_non", object)
  values(gr) <- df
  p <- autoplot(gr, ...)
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("")
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main) +
      opts(strip.background = theme_rect(colour = 'NA', fill = 'NA'))+ 
        opts(strip.text.y = theme_text(colour = 'white')) 
  p
})


## ======================================================================
##        For "GappedAlignments"
## ======================================================================
setMethod("autoplot", "GappedAlignments", function(object, ...,
                                                   xlab, ylab, main,
                                                   which,
                                                   geom = "gapped.pair",
                                                   show.junction = FALSE
                                                   ){

  args <- list(...)
  if(!missing(which))
    gr <- biovizBase:::fetch(object, which)
  else
    gr <- biovizBase:::fetch(object)
  if(geom == "gapped.pair"){
    gr.junction <- gr[values(gr)$junction == TRUE]
    gr.gaps <- getGaps(gr.junction, "qname")
    gr.read <- gr  
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
                         ymin = substitute(stepping - 0.4),
                         ymax = substitute(stepping + 0.4)))
    p <- p + ggplot2::geom_rect(do.call("aes", args))+
      scale_color_manual(values = strandColor) +
        scale_fill_manual(values = strandColor)
    ## mapped read
    if(show.junction){
      args <- args[!(names(args) %in% c("x", "y"))]
      args <- c(args, list(x = substitute(start), xend = substitute(end),
                           y = substitute(stepping),
                           yend = substitute(stepping)))
      p <- p + ggplot2::geom_segment(data = df.gaps, do.call("aes", args), color = "red")
    }}else{
    p <- autoplot(gr, ..., geom = geom)
  }
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("Genomic Coordinate")
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main) 
  p
})


## ======================================================================
##        For "BamFile"
## ======================================================================
## TODO
## 1. mismatch
## 2. simply summary
setMethod("autoplot", "BamFile", function(object, ..., which,
                                          xlab, ylab, main,
                                          bsgenome,
                                          geom = "line",
                                          stat = "coverage",
                                          method = c("estimate", "raw"),
                                          resize.extra = 10,
                                          show.coverage = TRUE){

  args <- list(...)
  method <- match.arg(method)
  bf <- open(object)
  if(geom == "gapped.pair"){
    message("Read GappedAlignments from BamFile...")
    ga <- readBamGappedAlignments(bf,
                                  param = ScanBamParam(which = which),
                                  use.name = TRUE)
    message("plotting...")
    args.ga <- args[names(args) %in% "show.junction"]
    args <- c(args.ga, list(object = ga))
    p <- do.call(autoplot, args)
  }else{
    if(stat == "coverage"){
      if(!missing(which))
        p <- ggplot() + stat_coverage(bf, ..., method = method, geom  =  geom, which = which)
      else
        p <- ggplot() + stat_coverage(bf, ..., method = method, geom  =  geom)
    }else if(stat == "mismatch"){
      p <- ggplot() + stat_mismatch(bf, ..., bsgenome = bsgenome, which = which)
    }else{
      ga <- readBamGappedAlignments(bf,
                                    param = ScanBamParam(which = which),
                                    use.name = TRUE)
      gr <- biovizBase:::fetch(ga, type = "raw")
      p <- autoplot(gr, ..., geom = geom, stat = stat)
    }
  }
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("Genomic Coordinate")
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main) 

  p
})

## ======================================================================
##  For "character" need to check if it's path including bamfile or not
## ======================================================================
setMethod("autoplot", "character", function(object, ..., xlab, ylab, main,
                                            asRangedData = FALSE){
  .ext <- tools::file_ext(object)
  if(.ext == "bam"){
    message("reading in as Bamfile")
    obj <- BamFile(object)
  }else{
    message("reading in")
    obj <- import(object, asRangedData = asRangedData)
  }
  p <- autoplot(obj,  ...)
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("Genomic Coordinate")
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main) 
  p  
})

## FIX THIS first:
## ======================================================================
##        For "TranscriptDb"(Genomic Structure)
## ======================================================================
setMethod("autoplot", "TranscriptDb", function(object, which, ...,
                                               xlab, ylab, main,
                                               truncate.gaps = FALSE,
                                               truncate.fun = NULL,
                                               ratio = 0.0025, 
                                               geom = c("gene", "reduced_gene"),
                                               names.expr = expression(paste(tx_name,
                                                   "(", gene_id,")", sep = ""))){
  geom <- match.arg(geom)
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- object  
  args.non$truncate.gaps <- truncate.gaps
  args.non$truncate.fun <- truncate.fun
  args.non$ratio <- ratio
  if(!missing(which))
    args.non$which <- which
  aes.res <- do.call(aes, args.aes)
  args.res <- c(args.non,list(aes.res))
  p <- ggplot() + do.call(stat_gene, args.res)
  if(!missing(xlab))
    p <- p + xlab(xlab)
  if(!missing(ylab))
    p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})



## ======================================================================
##        For "BSgenome"
## ======================================================================
setMethod("autoplot", c("BSgenome"), function(object,  which, ...,
                                              xlab, ylab, main,
                                              geom = c("text",
                                                "segment",
                                                "point",
                                                "rect")){

  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  seqs <- getSeq(object, which, as.character = TRUE)
  seqs <- IRanges:::safeExplode(seqs)
  xs <- seq(start(which), length.out = width(which))
  df <- data.frame(x = xs, seqs = seqs)
  geom <- match.arg(geom)
  p <- ggplot(data = df, ...)
  if(!"color" %in% names(args.non))
    isDNABaseColor <- TRUE
  else
    isDNABaseColor <- FALSE
  baseColor <- getOption("biovizBase")$DNABasesColor
  p <- switch(geom,
              text = {
                if(isDNABaseColor){
                args.aes$x <- as.name("x")
                args.aes$y <- 0
                args.aes$label = as.name("seqs")
                args.aes$color = as.name("seqs")
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  p + do.call(geom_text, args.res)+
                    scale_color_manual(values = baseColor)
                }else{
                args.aes$x <- as.name("x")
                args.aes$y <- 0
                args.aes$label = as.name("seqs")
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                p + do.call(geom_text, args.res)
                }

              },
              segment = {
                if(isDNABaseColor){
                args.aes$x <- as.name("x")
                args.aes$y <- -1
                args.aes$xend <- as.name("x")
                args.aes$yend <- 1                
                args.aes$color = as.name("seqs")
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  
                  p + do.call(ggplot2::geom_segment, args.res) + 
                                         scale_color_manual(values = baseColor)+
                                           scale_y_continuous(limits = c(-10, 10))
                }else{
                args.aes$x <- as.name("x")
                args.aes$y <- -1
                args.aes$xend <- as.name("x")
                args.aes$yend <- 1                
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  
                  p + do.call(ggplot2::geom_segment, args.res) + 
                                         scale_y_continuous(limits = c(-10, 10))
                }
              },
              point = {
                if(isDNABaseColor){
                args.aes$x <- as.name("x")
                args.aes$y <- 0
                args.aes$color = as.name("seqs")
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  p + do.call(geom_point, args.res) +
                    scale_color_manual(values = baseColor)
                }else{
                  args.aes$x <- as.name("x")
                args.aes$y <- 0
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  p + do.call(geom_point, args.res)
                }
                
              },
              rect = {
                if(isDNABaseColor){
                args.aes$xmin <- as.name("x")
                args.aes$ymin <- -1
                args.aes$xmax <- substitute(x + 0.9)
                args.aes$ymax <- 1
                args.aes$color = as.name("seqs")
                args.aes$fill = as.name("seqs")                
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  p + do.call(ggplot2::geom_rect, args.res) +
                                      scale_y_continuous(limits = c(-10, 10))+
                                        scale_color_manual(values = baseColor)+
                                          scale_fill_manual(values = baseColor)
                }else{
                args.aes$xmin <- as.name("x")
                args.aes$ymin <- -1
                args.aes$xmax <- substitute(x + 0.9)                
                args.aes$ymax <- 1
                aes.res <- do.call(aes, args.aes)
                args.res <- c(list(aes.res), args.non)
                  p + do.call(ggplot2::geom_rect, args.res) +
                                      scale_y_continuous(limits = c(-10, 10))
                }

              })
  if(missing(xlab)){
    chrs <- unique(seqnames(which))
    gms <- genome(object)
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
  p <- p + scale_y_continuous(breaks = NULL)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})


## ======================================================================
##        For "Rle"
## ======================================================================
## geom: ... color = I("red"), doesn't work
## FIXME: idenity
setMethod("autoplot", "Rle", function(object, lower, ...,
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
  
  ## args.dots <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args <- list(...)
  args.slice <- args[names(args) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")]
  args <- args[!names(args) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")]

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  
  if(!"geom" %in% names(args))
    args$geom <- geom  
  if(!missing(lower))
    args.slice <- c(list(x = object,
                         lower = lower), args.slice)
  df <- switch(type,
               raw = {
                 x <- 1:length(object)                
                 y <- as.numeric(object)
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
setMethod("autoplot", "RleList", function(object, lower, ...,
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

  ## args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args <- list(...)
  args <- args[names(args) %in% c("size", "shape", "color", "alpha", "geom")]
  if(!"geom" %in% names(args))
    args$geom <- geom
  
  ## args <- c(geom = geom, args)

  args.slice <- args[names(args) %in%
                     c("upper", "includeLower",
                       "includeUpper", "rangesOnly")]
  if(!missing(lower))
    args.slice <- c(list(x = object,
                         lower = lower), args.slice)

  df <- switch(type,
               raw = {
                 x <- do.call(c,lapply(elementLengths(object),function(n) 1:n))
                 y <- as.numeric(unlist(object))
                 if(is.null(names(object)))
                   nms <- rep(1:length(object), times = elementLengths(object))
                 else
                   nms <- rep(names(object), times = elementLengths(object))
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



##======================================================================
##  For ExpressionSet/eSet??
##======================================================================
setMethod("autoplot", "ExpressionSet", function(object, ..., type = c("none", "heatmap",
                                    "matrix", "parallel", "MA",
                                    "mean-sd", "volcano",
                                    "NUSE", "RLE"),
                     test.method = "t"){

  args <- as.list(match.call()[-1])
  type <- match.arg(type)
  df.exp <- exprs(object)
  df <- as.data.frame(df.exp)
  if(type == "matrix"){
    p <- plotmatrix(df, ...)
  }
  if(type == "heatmap"){
    df.m <- melt(df.exp)
    colnames(df.m) <- c("probe", "sampleNames", "value")
    head(df.m)
    res <- ddply(df.m, .(sampleNames), transform, rescale = rescale(value))

    p <- ggpcp(df) + geom_line()
  }
  if(type == "boxplot"){
    p <- ggpcp(res) + geom_boxplot(aes(group=variable))    
  }
  if(type == "NUSE"){
    require(affyPLM)
    message("fit PLM model...")
    dataPLM <- fitPLM(object)
    message("compute NUSE(Normalized Unscaled Standard Error)...")
    res <- getNR(dataPLM, type = "NUSE")
    res.m <- melt(res)
    colnames(res.m) <- c("probe", "sampleNames", "value")
    message("plotting...")
    p <- ggplot(res.m, aes(x = sampleNames, y = value)) + geom_boxplot(...)
  }
  if(type == "RLE"){
    require(affyPLM)
    message("fit PLM model...")    
    dataPLM <- fitPLM(object)
    message("compute RLE(Relative Log Expression)...")    
    res <- getNR(dataPLM, type = "RLE")
    res.m <- melt(res)
    colnames(res.m) <- c("probe", "sampleNames", "value")
    message("plotting...")    
    p <- ggplot(res.m, aes(x = sampleNames, y = value)) + geom_boxplot(...)
  }
  if(type == "mean-sd"){
    require("vsn")
    rk <- TRUE
    if("ranks" %in% names(args))
      rk <- args$ranks
    res <- vsn::meanSdPlot(object, ranks = rk, plot = FALSE)
    print(rk)
    if(rk)
      xlabs <- "rank(mean)"
    else
      xlabs <- "mean"
    p <- qplot(x = res$px, y = res$py, geom = "point") +
      geom_point(aes(x = res[[1]], y = res$sd), color = "red") +
        xlab(xlabs) + ylab("sd")
  }
  if(type == "volcano"){
    require(genefilter)
    if(!"fac" %in% names(args))
      stop("argument fac must be provided to make t-test,
            check genefilter::rowttests for details")
    message("genefilter::rowttests used")
    tt <- rowttests(object, args$fac)
    p <- qplot(tt$dm, -log10(tt$p.value), geom = "point") +
      xlab(expression(mean~log[2]~fold~change)) +
        ylab(expression(-log[10](p)))

  }
  if(type == "none"){
    df.l <- transform(objct)    
    p <- qplot(data = df.l, ...)
  }
  p
})



##======================================================================
##  For GenomicRangesList, for circular view
##======================================================================
## TODO for circular layout first
## need to name the aes list otherwise following the order
setMethod("autoplot", "GenomicRangesList", function(object, args = list(),
                                                    trackWidth,
                                                    radius = 10,
                                                    grid = FALSE,
                                                    trackSkip = 1,
                                                    layout = c("circle")){
  
  if(missing(trackWidth)){
    trackWidth <- rep(5, length(object))
    idx <- which(unlist(lapply(args, function(arg){
      arg$geom == "link"
    })))
    trackWidth[1] <- 1
  }else{
    if(length(trackWidth) > length(object)){
      warning("Unequal lengths of trackWidth, removing extra track width")
      trackWidth <- trackWidth[1:length(object)]

    }
    if(length(trackWidth) < length(object)){
      warning("Unequal lengths of trackWidth, adding default 5 to  extra track width")
      trackWidth <- c(trackWidth, rep(5, length(object) - length(trackWidth)))
    }
  }
    if(length(trackSkip) == 1){
      trackSkip <- rep(trackSkip, length(object))
    }else{
      if(length(trackSkip) != length(object))
        stop("trackSkip must be of length 1 or of the same length
              as object")
    }
  
    if(length(radius) == 1){
      radius <- radius + c(0, cumsum(trackWidth)[-length(trackWidth)]) +
        cumsum(trackSkip)
    }else{
      if(length(radius) != length(object))
        stop("radius must be of length 1 showing innermost radius or of the same length
              as object")
    }

  
    if(length(grid) == 1){
      grid <- rep(grid, length(object))
    }else{
      if(length(grid) != length(object))
        stop("grid must of length 1 or of the same length
              as object")
    }
  
    p <- ggplot()
  
    for(i in 1:length(object)){
      p <- p + do.call(layout_circle, c(list(data = object[[i]]), radius = radius[i],
                                        trackWidth = trackWidth[i], grid = grid[i],
                                        args[[i]]))
    }
    p
}) 

##======================================================================
##  For VCF
##======================================================================
setMethod("autoplot", "VCF", function(object, ..., xlab, ylab, main,
                                      type = c("geno", "info", "fixed"),
                                      ylabel = TRUE){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  type <- match.arg(type)
  hdr <- exptData(object)[["header"]]
  if(type == "geno"){
    nms <- rownames(geno(hdr))
    if("GT" %in% nms){
      message("use GT for type geno as default")
      gt <- geno(object)[["GT"]]
    }else{
      nm <- nms[1]
      message("use ", nm, " for type geno as default")
      gt <- geno(object)[[nm]]
    }
    sts <- start(rowData(object))
    idx <- !duplicated(sts)
    if(sum(!idx))
      warning("remove ", sum(!idx), " snp with duplicated position, only keep first one")
    gt <- gt[idx,]
    df <- melt(gt)
    df$start <- start(rowData(object)[idx])
    df$y <- as.integer(df$Var2)
    .y <- unique(df$y)
    .label <- df$Var2[match(.y, df$y)]
    p <- qplot(data = df, ..., x = start, y = y,  fill = value, geom = "raster") +
      scale_y_continuous(breaks = .y, label = .label)
    
  }
  if(type == "info"){
    df <- fortify(info(object))
    if(!"y" %in% names(args.aes)){
      hdr.i <- rownames(info(hdr))
      if("AF" %in% hdr.i){
        args.aes$y <- as.name("AF")
        message("use AF for type info as default")
      }else{
        nm <- hdr.i[i]
        message("use ", nm, " for type info as default")
        args.aes$y <- as.name(nm)
      }
    }
    if(!"x" %in% names(args)){
      args.aes$x <- as.name("start")
    }
    if(!"color" %in% names(args)){
      args.non$color <- "black"
    }
    if(!"fill" %in% names(args)){
      args.non$fill <- "black"
    }
    p <- ggplot(data = df) + do.call(geom_bar, c(list(stat = "identity"),
                                   list(do.call(aes, args.aes)),
                                   args.non))
  }
  if(type == "fixed"){
    fix <- fixed(object)
    fix <- fix[, !colnames(values(fix)) %in% c("ALT", "REF")]
    values(fix)$ALT <- unlist(values(alt(object))[, "ALT"])
    idx <- width(values(fix)$ALT) > 1
    type <- vector("character", length  = length(fix))
    type[idx] <- "I"
    type[!idx] <- as.character(values(fix[!idx])$ALT)
    values(fix)$type <- type
    id <- start(fix) < 25238400 & start(fix) > 25238100
    if(!"color" %in% names(args.non))
      isDNABaseColor <- TRUE
    else
      isDNABaseColor <- FALSE
    baseColor <- getOption("biovizBase")$DNABasesColor
    .i <- "black"
    names(.i) <- "I"
    baseColor <- c(baseColor, .i)
    fix <- addStepping(fix)
    id <- start(fix) < 25238400 & start(fix) > 25238100
    ## only show SNP
    df <- fortify(fix)
    df$type <- factor(df$type, levels = c(names(baseColor)))
    ylim <- range(df$stepping)
    ylim <- scales::expand_range(ylim, mul = 0.5)
    p <- ggplot() + geom_text(data = df,
                         aes(x = start, label = type, color = type,  y = stepping)) +
                          scale_color_manual(values = baseColor) +  
                            scale_y_continuous(breaks = unique(sort(values(fix)$stepping)),
                                               labels = unique(sort(values(fix)$stepping)),
                                               limits = ylim)
  }
  if(!ylabel)
    p <- p + scale_y_continuous(breaks = NULL)
  if(missing(xlab))
    xlab <- "Genomic Coordinates"
  p <- p + ggplot2::xlab(xlab)
  if(missing(ylab))
    ylab <- ""
  p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})

