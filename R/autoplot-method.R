## TODO: Priority this week til Wednesday
## 5. GappedAlignments
## 6. Bam/BamFile/character
## 7. IRanges
## 8. factorize statistics out
## 10. autoplot,GRanges, geom = "area" is slow
## 11. autoplot,GRanges, geom = "histogram"
## 12. autoplot,SummarizedExpriment
## 13. txdb y label
## 14. implement layout htere

setGeneric("autoplot")

formals.qplot <- getFormalNames(qplot)
formals.facet_grid <- getFormalNames(facet_grid)
formals.facet_wrap <- getFormalNames(facet_wrap)
formals.facets <- union(formals.facet_grid, formals.facet_wrap)

getGeomFun <- function(geom){
  match.fun(paste("geom_", geom, sep = ""))
}
getStatFun <- function(stat){
  match.fun(paste("stat_", stat, sep = ""))
}
getDrawFunFromGeomStat <- function(geom, stat){
  ## how about allways start from geom??
  if(!is.null(stat)){
    .fun <- getStatFun(stat)      
  }else{
    .fun <- getGeomFun(geom)
  }
  .fun
}


## ======================================================================
##        For "Granges"
## ======================================================================

setMethod("autoplot", "GRanges", function(object, ...,
                                          xlab, ylab, main,
                                          legend = TRUE,
                                          ## stat = c("identity", "coverage",
                                          ##   "stepping", "aggregate"),
                                          ## geom = c("rect", "segment","alignment",
                                          ##   "line","point", "area"),
                                          geom = NULL,
                                          stat = NULL,
                                          layout = c("linear", "stacked", "circle")
                                          ){
  formals.cur <- c("object", "stat", "geom", "legend",
                   "xlab", "ylab", "main")
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  ## args.facets <- args[names(args) %in% formals.facets]
  args <- args[!(names(args) %in% formals.cur)]
  args$data <- object


  .ggbio.geom <- c("rect", "chevron", "alignment", "5poly", "arrow", "segment")
  .ggbio.stat <- c("identity", "coverage", "stepping", "aggregate")

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


  ## ------------------------------
  ##   get the right function
  ## ------------------------------
  
  .fun <- getDrawFunFromGeomStat(geom, stat)
  p <- list(do.call(.fun, args))
  ## ------------------------------
  ## layout check
  ## ------------------------------
  layout <- match.arg(layout)
  ## since some of the geom or stat are not fully supported by all layout

  if(layout == "linear"){
    if(!legend)
      p <- c(p, list(opts(legend.position = "none")))
    
    if(missing(xlab)){
      chrs <- unique(seqnames(object))
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
    p <- c(p, list(xlab(xlab)))
    ## tweak with default y lab
    if(!missing(ylab))
      p <- c(p,list(ylab(ylab)))

    if(!missing(main))
      p <- c(p, list(opts(title = main)))

}  
  if(layout == "karyogram"){
    stop("layout karyogram is not implemented")
  }
  
  if(layout == "circle"){
    stop("layout circle is not implemented")
  }
  ggplot() + p
})



## ======================================================================
##        For "GRangesList"
## ======================================================================
flatGrl <- function(object, indName = ".grl.name"){
  idx <- togroup(object)
  indName <- ".grl.name"
  gr <- stack(object, indName)
  values(gr) <-   cbind(values(gr), values(object)[idx,,drop = FALSE])
  gr
}

setMethod("autoplot", "GRangesList", function(object, ..., indName = ".grl.name",
                                              geom = NULL, stat = NULL){
  
  ## geom <- match.arg(geom)
  if(is.null(geom) & is.null(stat))
    geom <- "alignment"
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non <- args.non[!names(args.non) %in% c("object", "indName")]
  args.non$geom <- geom
  gr <- flatGrl(object, indName)
  ## default or not?
  ## args.aes$group <- substitute(.grl.name)
  aes.res <- do.call(aes, args.aes)
  args.res <- c(args.non, list(aes.res), list(object = gr))
  p <- do.call(autoplot, args.res)
  p
})          

## ======================================================================
##        For "IRanges"
## ======================================================================

setMethod("autoplot", "IRanges", function(object, ...,
                                          legend = TRUE,
                                          xlab, ylab, main,
                                          facets, facet, 
                                          stat = c("identity", "coverage", "step"),
                                          geom = c("rect", "segment","alignment",
                                            "line","point", "area"),
                                          coord = "linear"){

  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "geom.engine",
                                    "object",  "legend"))]
  geom <- match.arg(geom)
  p <- switch(geom,
              full = {
                df <- as.data.frame(object)
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(object))
                p <- ggplot(df)
                args <- args[names(args) != "y"]
                args <- c(args, list(xmin = substitute(start),
                                     xmax = substitute(end),
                                     ymin = substitute(y - 0.4),
                                     ymax = substitute(y + 0.4)))
                p + geom_rect(do.call("aes", args))
              },
              segment = {
                df <- as.data.frame(object)
                df$midpoint <- (df$start+df$end)/2
                df$y <- as.numeric(disjointBins(object))
                p <- ggplot(df)
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(start), xend = substitute(end),
                                     y = substitute(y), yend = substitute(y)))
                p + geom_segment(do.call("aes", args))
              },
              coverage.line = {
                df <- as.data.frame(object)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                st <- min(start(object))
                cv <- coverage(object)
                vals <- as.numeric(cv)
                seqs <- seq.int(from = st, length.out = length(vals))
                ## remove x
                args <- args[!(names(args) %in% c("x", "y"))]
                args <- c(args, list(x = substitute(seqs),
                                     y = substitute(vals)))
                p + geom_line(do.call("aes", args))+
                  ylab("coverage")
              },
              coverage.area = {
                df <- as.data.frame(object)
                df$midpoint <- (df$start+df$end)/2
                p <- ggplot(df)
                st <- min(start(object))
                cv <- coverage(object)
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
setMethod("autoplot", "GappedAlignments", function(object, ...,
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
    gr <- biovizBase:::fetch(object, which)
  else
    gr <- biovizBase:::fetch(object)
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
setMethod("autoplot", "BamFile", function(object, ..., which,
                                          bsgenome,
                                          resize.extra = 10,
                                          show.coverage = TRUE,
                                          geom = c("gapped.pair",
                                            "full",
                                            "coverage.line",
                                            "coverage.area",
                                            "mismatch.summary")){
  
  args <- as.list(match.call(call = sys.call(1)))[-1]
  args <- args[!(names(args) %in% c("geom", "which", "heights"))]
  geom <- match.arg(geom)
  bf <- open(object)
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
  if(geom %in% c("coverage.line", "coverage.area", "full")){
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
setMethod("autoplot", "character", function(object, ...,
                                            which, 
                                            bsgenome,
                                            resize.extra = 10,
                                            show.coverage = TRUE,
                                            geom = c("gapped.pair",
                                              "full",
                                              "coverage.line",
                                              "coverage.area",
                                              "mismatch.summary")){
  geom <- match.arg(geom)
  if(tools::file_ext(object) == "bam")
    bf <- BamFile(object)
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
setMethod("autoplot", "TranscriptDb", function(object, which, ...,
                                               xlab, ylab, main,
                                               xlim, ylim, 
                                               geom = c(
                                                 "gene",
                                                 "reduced_gene"),
                                               names.expr = expression(paste(tx_name,
                                                   "(", gene_id,")", sep = ""))){
  if(missing(which))
    stop("missing which is not supported yet")
  if(missing(xlim))
    xlim <- c(start(range(which)),
              end(range(which)))
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent(2))))[-1]
  .args <- args[!(names(args) %in% c("geom", "which","object", "xlim", "ylim", "xlab", "ylab",
                                     "main", "names.expr"))]
  if(geom == "gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(object, which)
    ## gr <- fetch(object, which)
    message("Constructing graphics...")
    values(gr)$.levels <-  as.numeric(values(gr)$tx_id)
    ## drawing
    ## hard coded width of rect
    ## just cds, gaps and utrs
    df <- as.data.frame(gr)
    df.cds <- df[df$type == "cds",]
    p <- ggplot(df.cds)
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start),
                         xmax = substitute(end),
                         ymin = substitute(.levels - 0.4),
                         ymax = substitute(.levels + 0.4)))
    p <- p + geom_rect(do.call("aes", args))
    p
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
    .df.sub <- df[, c(".levels", "tx_id", "tx_name", "gene_id")]
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
    p <- p + scale_y_continuous(breaks = .df.sub$.levels,
                                labels = .labels)
    p
  }
  if(geom == "reduced_gene"){
    message("Aggregating TranscriptDb...")
    gr <- biovizBase:::fetch(object, which, type = "single")
    ## gr <- fetch(object, which, type = "single")
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
    ## only for rect 
    p <- p + geom_rect(do.call("aes", args))
    ## utrs
    df.utr <- df[df$type == "utr",]
    args <- .args[names(.args) != "y"]
    args <- c(args, list(xmin = substitute(start), xmax = substitute(end),
                         ymin = substitute(.levels - 0.2),
                         ymax = substitute(.levels + 0.2)))
    p <- p + geom_rect(data = df.utr, do.call("aes", args))
    ## gaps
    gr.rr <- reduce(ranges(gr[(values(gr)$type %in%  c("utr", "cds"))]))
    df.gaps <- gaps(gr.rr, start = min(start(gr.rr)), end = max(end(gr.rr)))
    chrs <- unique(as.character(seqnames(gr)))
    df.gaps <- GRanges(chrs, df.gaps)
    args <- .args[!(names(.args) %in% c("x", "y", "fill"))]
    .df.lvs <- unique(df$.levels)
    p <- p + scale_y_continuous(breaks = NA) + opts(axis.text.y = theme_blank())
    p <- p + geom_chevron(data = df.gaps, do.call("aes", args))
  }
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
  if(missing(ylab)){
    ## if(geom == "reduced_gene")
    ##   p <- p + ggplot2::ylab("Reduced Gene Model")
    ## if(geom == "gene")
    ##   p <- p + ggplot2::ylab("Gene Model")
    p <- p + ggplot2::ylab(getYLab(object))
  }else{
    p <- p + ylab(ylab)
  }
  if(!missing(main))
    p <- p + opts(title = main)
  p + coord_cartesian(xlim = xlim, wise = TRUE)
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
  dots <- list(...)
  if(!"color" %in% names(dots))
    isDNABaseColor <- TRUE
  else
    isDNABaseColor <- FALSE
  seqs <- getSeq(object, which, as.character = TRUE)
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
              rect = {
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
  p <- p + scale_y_continuous(breaks = NA)
  if(!missing(main))
    p <- p + opts(title = main)
  p
})


## ======================================================================
##        For "Rle"
## ======================================================================
## geom: ... color = I("red"), doesn't work
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

  args.dots <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.slice <- args.dots[names(args.dots) %in%
                          c("upper", "includeLower",
                            "includeUpper", "rangesOnly")] 
  args <- args.dots[names(args.dots) %in% c("size", "shape", "color", "alpha", "geom")]
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

autoplot <- function(object, ..., type = c("none", "heatmap",
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
}

