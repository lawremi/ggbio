setGeneric("autoplot")

formals.qplot <- getFormalNames(qplot)
formals.facet_grid <- getFormalNames(facet_grid)
formals.facet_wrap <- getFormalNames(facet_wrap)
formals.facets <- union(formals.facet_grid, formals.facet_wrap)

.ggbio.geom <- c("rect", "chevron", "alignment", "arrowrect", "arrow",
                 "segment", "arch", "bar")
.ggbio.stat <- c("identity", "coverage", "stepping", "aggregate", "table",
                 "gene", "mismatch", "reduce", "bin", "slice")

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
    object <- biovizBase:::rescaleGr(object)
  }
  formals.cur <- c("object", "stat", "geom", "legend",
                   "xlab", "ylab", "main")
  ## truncate
  if(truncate.gaps | coord == "truncate_gaps"){
    if(is.null(truncate.fun)){
      grl <- split(object, seqnames(object))
      lst <- endoapply(grl, function(gr){
        object.s <- reduce(gr, ignore.strand = TRUE)
        gps <- gaps(object.s, min(start(object.s)), max(end(object.s)))
        gps <- gps[strand(gps) == "*"]
        truncate.fun <- shrinkageFun(gps, maxGap(gps, ratio = ratio))
        res <- truncate.fun(gr)
        res
      })
      object <- unlist(lst)
    }else{
      object <- truncate.fun(object)
    }
  }
  ## ------------------------------
  ## geom/stat check
  ## ------------------------------
  ## if(is.null(geom) & layout = "circle")
  ##   geom <- "ideo"
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
    args.non$data <- mold(object)
    if(!"x" %in% names(args.aes))
      args.aes$x <- as.name("midpoint")
  }

  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  ## ------------------------------
  ## layout check
  ## ------------------------------
  layout <- match.arg(layout)
  ## treak with facet
  if(layout == "linear")
      facet <- .buildFacetsFromArgs(object, args.facets)
  else
    facet <- NULL
  
  if(layout == "linear" & coord == "genome")
    facet <- facet_null()
  
  ## use the default x
  ## since some of the geom or stat are not fully supported by all layout
  if(layout == "linear"){
  ## ------------------------------
  ##   get the right function
  ## ------------------------------
    aes.res <- do.call(aes, args.aes)
    ## args.res <- c(list(aes.res), args.non)
    .fun <- getDrawFunFromGeomStat(geom, stat)
    .xlim <- c(start(range(object, ignore.strand = TRUE)),
               end(range(object, ignore.strand = TRUE)))
    p <- list(do.call(.fun, c(args.non, list(aes.res))))
    if(!is.null(stat) && stat != "aggregate")
      p <- c(p, list(scale_by_xlim(.xlim)))
    if(!legend)
      p <- c(p, list(theme(legend.position = "none")))
    if(missing(xlab)){
        xlab <- ""
    }
    p <- c(p, list(xlab(xlab)))
    ## tweak with default y lab
    if(!missing(ylab))
      p <- c(p,list(ylab(ylab)))
    if(!missing(main))
      p <- c(p, list(labs(title = main)))
    ## if("x" %in% names(args.aes))
    args.aes <- args.aes[names(args.aes) %in% c("x", "y")]
    aes.res <- do.call(aes, args.aes)
    p <- do.call(ggplot, c(list(data = object),
                            list(aes.res))) + p
  }  
  if(layout == "karyogram"){
    p <- plotStackedOverview(object, ...,  geom = geom)
    if(missing(xlab)){
        xlab <- ""
    }
    p <- p + ggplot2::xlab(xlab)
    ## FIXME: xlab/ylab/main
  }
  if(layout == "circle"){
    p <- ggplot(object) + layout_circle(object, geom = geom, space.skip = space.skip, ...) 
  }
  ## test scale
  if(is_coord_truncate_gaps(object) | is_coord_genome(object)){
    ss <- getXScale(object)
    p <- p + scale_x_continuous(breaks = ss$breaks,
                                labels = ss$labels)
  }
  if(length(stat) && stat != "aggregate")
    p <- p + facet
  p$.data <- object
  p <- ggbio(p)
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
    if(is.null(geom) & is.null(stat))
      geom <- "alignment"
    args.non$geom <- geom
    gr <- flatGrl(object, indName)
    if(!"group" %in% names(args.aes))
      args.aes$group <- as.name(indName)
    aes.res <- do.call(aes, args.aes)
    args.non$object <- gr
    args.res <- c(args.non, list(aes.res))
    p <- do.call(autoplot, args.res)
    p
  }
  if(type == "sashimi"){
    message("type:sashimi will be dropped in next release of bioc")
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
  if(missing(xlab)) {
        xlab <- ""
  }
  p <- p + ggplot2::xlab(xlab)
  if(!missing(ylab))
    p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
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
    p <- p + labs(title = main) +
      theme(strip.background = element_rect(colour = 'NA', fill = 'NA'))+ 
        theme(strip.text.y = element_text(colour = 'white')) 
  p
})


## ======================================================================
##        For "GappedAlignments"
## ======================================================================
setMethod("autoplot", "GappedAlignments", function(object, ...,
                                                   xlab, ylab, main,
                                                   which,
                                                   geom = NULL, stat = NULL){

  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(object, args.facets)
  if(is.null(stat)){
  if(is.null(geom))
    geom <- "alignment"
  #### going to be droped next release of bioc
  if("show.junction" %in% names(args.non)){
    message("show.junction is going to be dropped, geom:alignment will contain gaps")
  show.junction <- args.non$show.junction
  if(show.junction){
    geom <- "alignment"
  }else{
    geom <- "rect"
  }}
  ####
  if(geom == "gapped.pair"){
    message("geom:gapped.pair is going to be dropped next release,
             if geom = NULL, it's going to use grglist(object) and show as
             alignemnts")
    geom <- "alignment"
  }}else{
    args.non$stat <- stat
    args.non$geom <- geom
  }
  aes.res <- do.call(aes, args.aes)  
  if(!is.null(geom)  && geom == "alignment"){
    args.non$object <- grglist(object)
    p <- do.call(autoplot, c(list(aes.res), args.non))
  }else{
    if(!missing(which))
      gr <- biovizBase:::fetch(object, which)
    else
      gr <- biovizBase:::fetch(object)
    args.non$object <- gr
    p <- do.call(autoplot, c(list(aes.res), args.non))
  }
  if(missing(xlab)) {
        xlab <- ""
  }
  p <- p + ggplot2::xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  p <- p + facet
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
                                          coord = c("linear", "genome"),
                                          resize.extra = 10,
                                          space.skip = 0.1,
                                          show.coverage = TRUE){

  coord <- match.arg(coord)
  if(missing(xlab))
    xlab <- NULL
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
      if(method == "estimate"){
      if(missing(which)){
        seq.nm <- names(scanBamHeader(object)[[1]])[1]
      }else{
        if(is(which, "GRanges")){
          seq.nm <- unique(as.character(seqnames(which)))
        } else if(is(which, "character")){
          seq.nm <- which
        }else{
          stop("which must be missing, GRanges or character(for seqnames)")
        }
      }
      xlab <- ""
    }
      
      if(!missing(which))
        p <- ggplot() + stat_coverage(bf, ..., method = method, coord = coord,
                                      space.skip = space.skip,
                                      geom  =  geom, which = which)
      else
        p <- ggplot() + stat_coverage(bf, ..., method = method, coord = coord,
                                      space.skip = space.skip,
                                      geom  =  geom)
    }else if(stat == "mismatch"){
      if(geom %in% c("bar", "segment")){
        p <- ggplot() + stat_mismatch(bf, ..., bsgenome = bsgenome, which = which, geom = "bar")
      }else{
        p <- ggplot() + stat_mismatch(bf, ..., bsgenome = bsgenome, which = which)
      }
    }else{
      ga <- readBamGappedAlignments(bf,
                                    param = ScanBamParam(which = which),
                                    use.name = TRUE)
      gr <- biovizBase:::fetch(ga, type = "raw")
      p <- autoplot(gr, ..., geom = geom, stat = stat)
    }
  }
  if(length(xlab) >=1){
    p <- p + ggplot2::xlab(xlab)
  }else{
    p <- p + ggplot2::xlab("")
  }
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main) 
  p
})

## ======================================================================
##  For "character" need to check if it's path including bamfile or not
## ======================================================================
setMethod("autoplot", "character", function(object, ..., xlab, ylab, main,
                                            which,
                                            asRangedData = FALSE){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(!missing(which))
    args.non$which <- which

  .ext <- tools::file_ext(object)
  if(.ext == "bam"){
    message("reading in as Bamfile")
    obj <- BamFile(object)
  }else{
    message("reading in")
    obj <- import(object, asRangedData = asRangedData)
    if(!missing(which) && is(which, "GRanges"))
      obj <- subsetByOverlaps(obj, which)
  }
  if(is(obj, "GRanges")){
    if(missing(xlab))
      xlab <- ""
    if(!"geom" %in% names(args.non)){
    if("y" %in% names(args.aes) | "score" %in% colnames(values(obj))){
      args.non$geom <- "bar"
    }else{
      args.non$geom <- "rect"
    }}
  }
  args.non$object <- obj
  aes.res <- do.call(aes, args.aes)
  p <- do.call(autoplot, c(list(aes.res), args.non))
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main) 
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
                                               geom = c("alignment"),
                                               stat = c("identity", "reduce"),
                                               names.expr = "tx_name(gene_id)"){

  stat <- match.arg(stat)
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- object  
  args.non$truncate.gaps <- truncate.gaps
  args.non$truncate.fun <- truncate.fun
  args.non$ratio <- ratio
  args.non$geom <- geom
  args.non$stat <- stat
  args.non$names.expr <- names.expr
  if(!missing(which))
    args.non$which <- which
  aes.res <- do.call(aes, args.aes)
  args.res <- c(args.non,list(aes.res))
  p <- ggplot() + do.call(geom_alignment, args.res)
  if(!missing(xlab))
    p <- p + xlab(xlab)
  else
    p <- p + ggplot2::xlab("")
  if(!missing(ylab))
    p <- p + ylab(ylab)
  else
    p <- p + ggplot2::ylab("")
  if(!missing(main))
    p <- p + labs(title = main)
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
    ## chrs <- unique(seqnames(which))
    ## gms <- genome(object)
    ## gm <- unique(gms[chrs])
    ## chrs.tx <- paste(chrs, sep = ",")    
    ## if(is.na(gm)){
    ##   xlab <- chrs.tx
    ## }else{
    ##   gm.tx <- paste(gm)
    ##   xlab <- paste(gm.tx,"::",chrs.tx, sep = "")      
    ## }
    xlab <- ""
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
    p <- p + labs(title = main)
  p
})


## ======================================================================
##        For "Rle"
## ======================================================================
## geom: ... color = I("red"), doesn't work
## FIXME: idenity
setMethod("autoplot", "Rle", function(object, ...,
                                      xlab, ylab, main,
                                      binwidth, nbin = 30,
                                      geom = NULL,
                                      stat = c("bin", "identity", "slice"),
                                      type = c("viewSums","viewMins",
                                        "viewMaxs", "viewMeans")){

  stat <- match.arg(stat)
  type <- match.arg(type)

  args <- list(...)

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- object
  args.non$geom <- geom

  if(stat == "identity"){
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_identity, args.res)
  }
  if(stat == "bin"){
    args.non$nbin <- nbin
    args.non$type <- type
    aes.res <- do.call(aes, args.aes)
    if(!missing(binwidth))
      args.non$binwidth <- binwidth
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_bin, args.res)
  }
  if(stat == "slice"){
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_slice, args.res)
  }
  
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("")    
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  p
})


## ======================================================================
##        For "RleList"
## ======================================================================
## 1. facet by list
## 2. support as what has been supported in Rle.
setMethod("autoplot", "RleList", function(object, ...,
                                          xlab , ylab, main,
                                          nbin = 30,
                                          binwidth,
                                          facetByRow = TRUE,
                                          stat = c("bin", "identity", "slice"),
                                          geom = NULL,
                                          type = c("viewSums","viewMins",
                                            "viewMaxs", "viewMeans")){
  stat <- match.arg(stat)
  
  type <- match.arg(type)
  ## if(stat == "slice" &&
  ##    type %in% c("viewMaxs", "viewMeans", "viewMins", "viewSums") && missing(lower))
  ##   stop("please at least specify the value of lower, you could pass
  ##         extra parameters to slice too")

  ## args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- object
  args.non$geom <- geom
  if(stat == "identity"){
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_identity, args.res)
  }
  if(stat == "bin"){
    args.non$nbin <- nbin
    aes.res <- do.call(aes, args.aes)
    if(!missing(binwidth))
      args.non$binwidth <- binwidth
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_bin, args.res)
  }
  if(stat == "slice"){
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- ggplot() + do.call(stat_slice, args.res)
  }
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  else
    p <- p + ggplot2::xlab("")
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  p
  ## if(facetByRow)
  ##   facets <- listName ~ .
  ## else
  ##   facets <- . ~ listName
})



##======================================================================
##  For ExpressionSet/eSet??
##======================================================================

.ggpcp <- function(data, vars = names(data), ...){
  scaled <- as.data.frame(lapply(data[, vars], ggplot2:::rescale01))
  data <- ggplot2:::cunion(scaled, data)
  data$ROWID <- 1:nrow(data)
  molten <- reshape2::melt(data, m = vars)
  ggplot(molten, aes_string(x = "variable", y = "value", group = "ROWID"), 
         ...)
}

## setGeneric("phenoPlot", "")
## phenoPlot <- function(){
  
## }

setMethod("autoplot", "ExpressionSet", function(object, ...,
                                                type = c("heatmap","none",
                                                  "scatterplot.matrix", "pcp", "MA", "boxplot",
                                                  "mean-sd", "volcano",
                                                  "NUSE", "RLE"),
                                                test.method = "t",
                                                rotate = FALSE,
                                                pheno.plot = FALSE,
                                                main_to_pheno = 4.5,
                                                padding = 0.2){

  args <- list(...)
  type <- match.arg(type)
  df.exp <- exprs(object)
  df <- as.data.frame(df.exp)
  if(type == "scatterplot.matrix"){
    p <- plotmatrix(df, ...)
  }
  if(type == "heatmap"){
    ## add pheno type data
    if(!pheno.plot){
    colnames(df.exp) <- rownames(pData(object))
    p <- autoplot(df.exp, ...) + ylab("Features") + xlab("Samples")
    }else{
      colnames(df.exp) <- rownames(pData(object))
      p <- autoplot(t(df.exp), ...) + xlab("Features") + ylab("Samples")
      pd <- pData(object)
      s <- list(theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank()) ,
                theme(legend.position = "top",
                      plot.margin = unit(c(1, padding, 0.5, padding), "lines")),
                guides(fill = guide_legend(bycol = TRUE,
                         byrow = FALSE, ncol =  1,
                         title.theme = element_blank())))

      
      N <- ncol(pd)
      hts <- rep(1/N, N)
      hts <- c(hts, main_to_pheno)
      l <- lapply(1:N, function(i){
        autoplot(as.matrix(pd[, i, drop  = FALSE])) + s
      })
      ry <- c(rep(TRUE, N), FALSE)
      l <- c(l, list(p))
      return(do.call(alignPlots, c(l, list(vertical = FALSE, remove.y.axis = ry,
                                           widths = hts))))
    }
  }
  if(type == "pcp"){
    p <- .ggpcp(df) + geom_line(...) + xlab("Sample Name")
  }
  if(type == "boxplot"){
    p <- .ggpcp(df) + geom_boxplot(aes(group=variable), ...)+ xlab("Sample Name")
  }
  if(type == "MA"){
    stop("not impleenmted yet")
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
    if(rk)
      xlabs <- "rank(mean)"
    else
      xlabs <- "mean"
    p <- qplot(x = res$px, y = res$py, geom = "point") +
      geom_point(aes(x = res[[1]], y = res$sd), color = "red") +
        xlab(xlabs) + ylab("sd")
    p
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
    df.l <- mold(object)
    p <- qplot(data = df.l, ...)
  }
  p
})

getNR <- function(x, type = c("NUSE", "RLE"),range = 0, ...){
  compute.nuse <- function(which) {
    nuse <- apply(x@weights[[1]][which, ], 2, sum)
    1/sqrt(nuse)
  }

  type <- match.arg(type)
  model <- x@model.description$modelsettings$model

  if (type == "NUSE") {
    if (x@model.description$R.model$which.parameter.types[3] == 
        1 & x@model.description$R.model$which.parameter.types[1] == 
        0) {
      grp.rma.se1.median <- apply(se(x), 1, median, 
                                  na.rm = TRUE)
      res <- grp.rma.rel.se1.mtx <- sweep(se(x), 1, grp.rma.se1.median, 
                                          FUN = "/")

    }
    else {
      which <- indexProbesProcessed(x)
      ses <- matrix(0, length(which), 4)
      if (x@model.description$R.model$response.variable == 
          1) {
        for (i in 1:length(which)) ses[i, ] <- compute.nuse(which[[i]])
      }
      else {
        stop("Sorry I can't currently impute NUSE values for this PLMset object")
      }
      grp.rma.se1.median <- apply(ses, 1, median)
      res <- grp.rma.rel.se1.mtx <- sweep(ses, 1, grp.rma.se1.median, 
                                          FUN = "/")
      
    }
  }
  if(type == "RLE"){
    if (x@model.description$R.model$which.parameter.types[3] == 
        1) {
      medianchip <- apply(coefs(x), 1, median)
      res <- sweep(coefs(x), 1, medianchip, FUN = "-")
    }
    else {
      stop("It doesn't appear that a model with sample effects was used.")
    }  
  }
  res
}



##======================================================================
##  For GenomicRangesList, for circular view
##======================================================================
## TODO for circular layout first
## need to name the aes list otherwise following the order
## setMethod("autoplot", "GenomicRangesList", function(object, args = list(),
##                                                     trackWidth,
##                                                     radius = 10,
##                                                     grid = FALSE,
##                                                     trackSkip = 1,
##                                                     layout = c("circle")){

##   layout <- match.arg(layout)
##   message("Take 'genome' coordinate transformation")
##   if(layout == "circle"){
##   if(missing(trackWidth)){
##     trackWidth <- rep(5, length(object))
##     idx <- which(unlist(lapply(args, function(arg){
##       arg$geom == "link"
##     })))
##     trackWidth[1] <- 1
##   }else{
##     if(length(trackWidth) > length(object)){
##       warning("Unequal lengths of trackWidth, removing extra track width")
##       trackWidth <- trackWidth[1:length(object)]

##     }
##     if(length(trackWidth) < length(object)){
##       warning("Unequal lengths of trackWidth, adding default 5 to  extra track width")
##       trackWidth <- c(trackWidth, rep(5, length(object) - length(trackWidth)))
##     }
##   }
##     if(length(trackSkip) == 1){
##       trackSkip <- rep(trackSkip, length(object))
##     }else{
##       if(length(trackSkip) != length(object))
##         stop("trackSkip must be of length 1 or of the same length
##               as object")
##     }
  
##     if(length(radius) == 1){
##       radius <- radius + c(0, cumsum(trackWidth)[-length(trackWidth)]) +
##         cumsum(trackSkip)
##     }else{
##       if(length(radius) != length(object))
##         stop("radius must be of length 1 showing innermost radius or of the same length
##               as object")
##     }

  
##     if(length(grid) == 1){
##       grid <- rep(grid, length(object))
##     }else{
##       if(length(grid) != length(object))
##         stop("grid must of length 1 or of the same length
##               as object")
##     }
  
##     p <- ggplot()
  
##     for(i in 1:length(object)){
##       p <- p + do.call(layout_circle, c(list(data = object[[i]]), radius = radius[i],
##                                         trackWidth = trackWidth[i], grid = grid[i],
##                                         args[[i]]))
##     }}
##     p
## }) 

##======================================================================
##  For VCF
##======================================================================

setMethod("autoplot", "VCF", function(object, ...,
                                      xlab, ylab, main,
                                      assay.id,                                      
                                      type = c("geno", "info", "fixed"),
                                      full.string = FALSE,
                                      ref.show = TRUE,
                                      genome.axis = TRUE,
                                      transpose = TRUE){
  
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  type <- match.arg(type)
  hdr <- exptData(object)[["header"]]
  
  if(type == "geno"){
    nms <- rownames(geno(hdr))
    message(paste(nms, collapse = ","), " could be used for 'geno' type")
    if(missing(assay.id)){
      if("GT" %in% nms){
        message("use GT for type geno as default")
        gt <- geno(object)[["GT"]]
      }else{
        nm <- nms[1]
        message("use ", nm, " for type geno as default")
        gt <- geno(object)[[nm]]
      }}else{
        if(is.numeric(assay.id))
          nm <- nms[assay.id]
        if(is.character(assay.id)){
          if(!assay.id %in% nms){
            stop(assay.id, " is not in ", nms)
          }
          nm <- assay.id
        }
        gt <- geno(object)[[nm]]
      }
    sts <- start(rowData(object))
    idx <- !duplicated(sts)
    ## is this a right thing to do ?
    if(sum(!idx))
      message("Index: ", paste(which(!idx), collapse = ","),
              " snp with duplicated start position may be masked by each other")
    ## gt <- gt[idx,]
    rownames(gt) <- start(rowData(object))    
    ## rownames(gt) <- start(rowData(object)[idx])

    if(!"color" %in% names(args.aes) && !"color" %in% names(args.non))
      args.aes$color <- as.name("value")
    if(!"colnames.label" %in% names(args.non)){
      if(transpose)
        args.non$colnames.label <- FALSE
      else
        args.non$colnames.label <- TRUE
    }
    if(genome.axis){
      gt <- t(gt)
      args.aes$x <- substitute(as.numeric(colnames))    
      aes.args <- do.call(aes, args.aes)
      p <- do.call(autoplot, c(list(object = gt),
                               list(aes.args),
                               args.non))
    }else{
      if(transpose)
        gt <- t(gt)
      aes.args <- do.call(aes, args.aes)
      p <- do.call(autoplot, c(list(object = gt),
                                 list(aes.args),
                                 args.non))
    }

  }
  if(type == "info"){
    colClasses <- function(x){
      sapply(values(info(x))@listData, class)
    }
    cls <- colClasses(object)
    idx.cls <- which(cls %in% c("numeric", "integer", "character", "factor"))
    
    df <- mold(info(object))
    
    if(!"y" %in% names(args.aes)){
      hdr.i <- rownames(info(hdr))
      if(missing(assay.id)){
        if("AF" %in% hdr.i){
          message("use AF for type info as default")
          args.aes$y <- as.name("AF")
        }else{
          nm <- hdr.i[idx[1]]
          message("use ", nm, " for type info as default")
          args.aes$y <- as.name(nm)
        }
      }else{
        if(is.numeric(assay.id))
          assay.id <- hdr.i[assay.id]
        message("use ", assay.id, " for type info as default")
        args.aes$y <- as.name(assay.id)
      }
    }
    if(!"x" %in% names(args.aes)){
      args.aes$x <- as.name("start")
    }
    if(!"colour" %in% names(args.aes) && !"colour" %in% names(args.non) &&
       !"color" %in% names(args.aes) && !"color" %in% names(args.non) ){
      args.non$colour <- "black"
    }
    message("Other options for potential mapping(only keep numeric/integer/character/factor variable): ")

      p <- ggplot(data = df) + do.call(ggplot2::geom_bar, c(list(stat = "identity"),
                    list(do.call(aes, args.aes)),
                    args.non))
  }
  if(type == "fixed"){
    fix <- fixed(object)
    fix <- fix[, !colnames(values(fix)) %in% c("ALT", "REF")]
    values(fix)$ALT <- as.character(unlist(alt(object)))
    values(fix)$REF <- as.character(ref(object))
    fix2 <- fix        
    type2 <- vector("character", length  = length(fix))      
    idx <- width(values(fix)$ALT) > 1    
    type2[idx] <- "I"
    type2[!idx] <- as.character(values(fix[!idx])$ALT)
    values(fix)$type <- type2

    if(!"color" %in% names(args.non))
      isDNABaseColor <- TRUE
    else
      isDNABaseColor <- FALSE
    baseColor <- getOption("biovizBase")$DNABasesColor
    .i <- "black"
    names(.i) <- "I"
    baseColor <- c(baseColor, .i)
    ir <- IRanges(start = start(fix), width = width(values(fix)$ALT))
    if(!full.string)
      width(ir[idx,]) <- 1
    steps <- disjointBins(ir)      
    values(fix)$stepping <- steps      
    values(fix)$value <- values(fix)$ALT
    values(fix)$group <- "ALT"    
    fix2 <- addStepping(fix2)
    idx <- width(values(fix2)$REF) > 1        
    ir <- IRanges(start = start(fix), width = width(values(fix2)$REF))
    if(!full.string)
      width(ir[idx,]) <- 1
    steps <- disjointBins(ir)      
    values(fix2)$stepping <- steps      
    type2 <- vector("character", length  = length(fix2))      
    type2[idx] <- "I"
    type2[!idx] <- as.character(values(fix[!idx])$REF)
    values(fix2)$type <- type2
    values(fix2)$value <- values(fix2)$REF
    values(fix2)$group <- "REF"
    .nms <- colnames(values(fix))
    fix <- c(fix, fix2[, .nms])
    if(ref.show){
      facet <- facet_grid(group ~ ., scales = "free_y")
      }else{
        fix <- fix[values(fix)$group == "ALT"]
        facet <- NULL
      }
    if(!full.string){
      ## only show SNP
      df <- mold(fix)
      p <- ggplot() + geom_text(data = df, ..., 
                                aes(x = start, label = type, color = type,  y = stepping)) +
                                  scale_color_manual(values = baseColor) +  facet
                              
    }else{
      df <- mold(fix)
      df$type <- factor(df$type, levels = c(names(baseColor)))
      p <- ggplot() + geom_text(data = df, ..., 
                                aes(x = start, label = value, color = type, y = stepping)) +
                                scale_color_manual(values = baseColor) + facet
      
    }
  }
  if(missing(xlab))
    xlab <- ""
  p <- p + ggplot2::xlab(xlab)
  if(missing(ylab))
    ylab <- ""
  p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  p
})



colorizeArgs <- function(args.non, args.aes){
  if(!"color" %in% names(args.non) && !"color" %in% names(args.aes)){
    if("fill" %in% names(args.aes)){
      args.aes$color <- args.aes$fill
    }else if("fill" %in% names(args.non)){
      args.non$color <- args.non$fill
    }else{
      args.non$color <- "black"
    }
  }
  list(args.aes = args.aes, args.non = args.non)
}

setMethod("autoplot", "matrix", function(object, ...,
                                         xlab, ylab, main,
                                         geom = c("tile", "raster"),
                                         axis.text.angle = NULL,
                                         hjust = 0.5,
                                         na.value = NULL,
                                         rownames.label = TRUE,
                                         colnames.label = TRUE,
                                         axis.text.x = TRUE,
                                         axis.text.y = TRUE){

  
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
   
  if(!"x" %in% names(args.aes))
    args.aes$x <- as.name("x")

  if(!"y" %in% names(args.aes))
    args.aes$y <- as.name("y")

  if(!"fill" %in% names(args.aes))
    args.aes$fill <- as.name("value")

  if(!"width" %in% names(args.aes))
    args.aes$width <- 1

  if(!"height" %in% names(args.aes))
    args.aes$height <- 1
   
  ## args2 <- colorizeArgs(args.non, args.aes)
  ## args.aes <- args2$args.aes
  ## args.non <- args2$args.non

  aes.args <- do.call(aes, args.aes)
  max.c <- 10
  geom <- match.arg(geom)
  df <- mold(object)
   
  if(geom == "raster"){
    p <- ggplot(data = df) + do.call(geom_raster, c(args.non, list(aes.args)))
    p <- p + theme_noexpand()
    if("rownames" %in% colnames(df) && rownames.label){
      y.lab <- rownames(object)
      y <- seq_len(nrow(object))
      p <- p + scale_y_continuous(breaks = y, label = y.lab, expand = c(0, 0))
    }
    if("colnames" %in% colnames(df) && colnames.label){
      x.lab <- colnames(object)
      if(max(sapply(x.lab, nchar))>max.c){
        if(is.null(axis.text.angle))
          axis.text.angle <- -90
      }
      x <- seq_len(ncol(object))
      p <- p + scale_x_continuous(breaks = x, label = x.lab, expand = c(0, 0))
    }

  }
  if(geom == "tile"){
    p <- ggplot(data = df) + do.call(geom_tile, c(args.non, list(aes.args)))
    p <- p + theme_noexpand()
    if("rownames" %in% colnames(df) && rownames.label){
      y.lab <- rownames(object)
      y <- seq_len(nrow(object))
      p <- p + scale_y_continuous(breaks = y, label = y.lab, expand = c(0, 0))
    }
    if("colnames" %in% colnames(df) && colnames.label){
      x.lab <- colnames(object)
      if(max(sapply(x.lab, nchar))> max.c){
        if(is.null(axis.text.angle))
          axis.text.angle <- -90
      }
      x <- seq_len(ncol(object))
      idx <- match(x, df$x)
      x <- eval(args.aes$x, df)[idx]
      x <- eval(args.aes$x, df)[idx]      
      p <- p + scale_x_continuous(breaks = x, label = x.lab, expand = c(0, 0))
    }
    p
  }
  if(!axis.text.x)
    p <- p + scale_x_continuous(breaks = NULL, label = NULL, expand = c(0, 0))
  if(!axis.text.y)
    p <- p + scale_y_continuous(breaks = NULL, label = NULL, expand = c(0, 0))    
  if(missing(xlab))
    xlab <- ""
  p <- p + ggplot2::xlab(xlab)
  if(missing(ylab))
    ylab <- ""
  p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  if(is.null(axis.text.angle))
    axis.text.angle <- 0
  p <- p + theme(axis.text.x=element_text(angle = axis.text.angle, hjust = hjust))
  if(!is.null(na.value)){
    p <-  p + scale_fill_discrete(na.value = na.value)
  }
  p
})

setMethod("autoplot", "Views", function(object, ...,
                                         xlab, ylab, main,
                                         geom = c("raster", "tile", "line"),
                                         axis.text.angle = NULL,
                                         hjust = 0,
                                         na.value = NULL,
                                         facets = row ~ .){

  geom <- match.arg(geom)
  p <- switch(geom,
              raster = {
                p <- ggplot(object, aes(group = row, x = x, y = y, fill = value)) +
                  geom_raster(...)
                p <- p + theme_noexpand()
                if(!is.null(names(object))){
                  y.lab <- names(object)
                  y <- seq_len(length(object))
                  p <- p + scale_y_continuous(breaks = y, label = y.lab, expand = c(0, 0))
                }
                p
              },
              tile = {
                p <- ggplot(object, aes(group = row, x = x, y = y, fill = value)) +
                  geom_tile(...)
                p <- p + theme_noexpand()                
                if(!is.null(names(object))){
                  y.lab <- names(object)
                  y <- seq_len(length(object))
                  p <- p + scale_y_continuous(breaks = y, label = y.lab, expand = c(0, 0))
                }
                p
              },
              line = {
                p <- ggplot(object, aes(group = row, x = x, y = value)) +
                  geom_line(...)
                 if(!is.null(facets)){
                  p <- p + facet_grid(facets)
                  p <- p + theme_pack_panels()
                }
                p
              })
  if(missing(xlab))
    xlab <- ""
  p <- p + ggplot2::xlab(xlab)
  if(missing(ylab))
    ylab <- ""
  p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  if(is.null(axis.text.angle))
    axis.text.angle <- 0
  p <- p + theme(axis.text.x=element_text(angle = axis.text.angle, hjust = hjust))
  if(!is.null(na.value)){
    p <-  p + scale_fill_discrete(na.value = na.value)
  }
  p
})


setMethod("autoplot", "Seqinfo", function(object, single.ideo = TRUE, ... ){
  obj <- .transformSeqinfo(object)
  if(length(obj) > 1)
    p <- ggplot() + layout_karyogram(obj)
  if(length(obj) == 1){
    if(single.ideo)
      p <- plotSingleChrom(obj, as.character(seqnames(obj)))
    else
      p <- ggplot() + layout_karyogram(obj)
  }
  p
})


## SummarizedExperiment
setMethod("autoplot", "SummarizedExperiment", function(object, ...,
                                                       type = c("heatmap", "link",
                                                         "pcp", "boxplot",
                                                         "scatterplot.matrix"),
                                                       pheno.plot = FALSE,
                                                       main_to_pheno = 4.5,
                                                       padding = 0.2,
                                                       assay.id = 1){
  
  type <- match.arg(type)
  ays <- assays(object)
  stopifnot(length(assay.id) == 1 || length(assay.id) <= length(ays))
  if(length(ays) > 1)
    message("Assay index: ", assay.id, " used")
  res <- ays[[assay.id]]  
  if(type == "heatmap"){
    res <- ays[[assay.id]]
    if(!pheno.plot){
      colnames(res) <- colnames(object)
      p <- autoplot(res, ...) + ylab("Features") + xlab("Samples")
    }else{
      colnames(res) <- colnames(object)
      p <- autoplot(t(res), ...) + xlab("") + ylab("Samples")
      object <- sset
      pd <- colData(object)
      s <- list(theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank()) ,
                theme(legend.position = "top",
                      plot.margin = unit(c(1, padding, 0.5, padding), "lines")),
                guides(fill = guide_legend(bycol = TRUE,
                         byrow = FALSE, ncol =  1,
                         title.theme = element_blank())))

      N <- ncol(pd)
      hts <- rep(1/N, N)
      hts <- c(hts, main_to_pheno)
      l <- lapply(1:N, function(i){
        autoplot(as.matrix(pd[, i, drop  = FALSE])) + s
      })
      ry <- c(rep(TRUE, N), FALSE)
      l <- c(l, list(p))
      return(do.call(alignPlots, c(l, list(vertical = FALSE, remove.y.axis = ry,
                                           widths = hts))))
    }
    
  }
  if(type == "link"){
    ## res <- rowData(object)
    ## values(res) <- assay(object)
    ## plotRangesLinkedToData(res[seqnames(res) == "chr1"],
    ## stat.col = seq_len(length(values(res))))
    stop("not implemented yet")
  }
  if(type == "pcp"){
    df <- as.data.frame(res)
    p <- .ggpcp(df) + geom_line(...) + xlab("Sample Name")
  }
  if(type == "boxplot"){
    df <- as.data.frame(res)
    p <- .ggpcp(df, ...) + geom_boxplot(aes(group=variable))+ xlab("Sample Name")
  }
  if(type == "scatterplot.matrix"){
    df <- as.data.frame(res)
    p <- plotmatrix(df, ...)    
  }
  p
})


