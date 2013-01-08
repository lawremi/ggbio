plotIdeogram <- function(obj, subchr, zoom.region,
                            xlab, ylab, main, xlabel = FALSE,
                            color = "red", fill = "red", alpha = 0.7,
                            zoom.offset = 0.1, size = 1, 
                            cytoband = TRUE, aspect.ratio = 1/20, genome){
  if(missing(obj)){
    obj <- getIdeogram(genome = genome, subchr = subchr, cytoband = cytoband)
  }
  ## do we need subchr here
  obj.ori <- obj
  if(!missing(subchr)){
    obj <- obj[seqnames(obj) == subchr]
    obj <- keepSeqlevels(obj, subchr)
  }else{
    subchr <- sort(unique(as.character(seqnames(obj))))[1]
    message("use ", subchr, " automatically")
    obj <- obj[seqnames(obj) == subchr]
    obj <- keepSeqlevels(obj, subchr)
  }
  if(length(unique(as.character(seqnames(obj))))>1)
    stop("Mulptiple chromosome information found")
  if(!isIdeogram(obj))
    cytoband <- FALSE
  p <- ggplot() + layout_karyogram(obj, cytoband = cytoband)
  p.ideo <- p
  if(!missing(zoom.region)){
    if(length(zoom.region) != 2)
      stop("zoom.region must be a numeric vector of length 2")
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          seqnames = unique(as.character(seqnames(obj))))
    p <- p + ggplot2::geom_rect(data = zoom.df,
                                do.call(aes, list(xmin = substitute(x1),
                                                  xmax = substitute(x2),
                                                  ymin = substitute(0 - zoom.offset),
                                                  ymax = substitute(10 + zoom.offset))),
                                color = color, fill = fill, size = size,
                                alpha = alpha)
  }
  p <- p + theme_alignment(grid = FALSE, ylabel = TRUE, border = FALSE) +
    scale_y_continuous(breaks = 5, label = subchr) +
      theme(strip.background = element_rect(colour = 'NA', fill = 'NA'))+ 
        theme(strip.text.y = element_text(colour = 'white'))   + theme(legend.position = "none")+
          ggplot2::xlab("")

  
  if(!missing(xlab)){
    p <- p + ggplot2::xlab(xlab)
    attr(p, "xlab") <- xlab
  }else{
    attr(p, "xlab") <- ""
  }
  if(!missing(ylab)){
    p <- p + ggplot2::ylab(ylab)
    attr(p, "ylab") <- ylab
  }else{
    p <- p + ggplot2::ylab(subchr)
  }
  if(!missing(main)){
    p <- p + labs(title = main)
    attr(p, "main") <- main 
  }else{
    attr(p, "main") <- ""
  }
  p <- p + theme(aspect.ratio = aspect.ratio, axis.ticks.y = element_blank())

  if(!xlabel)
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  attr(p, "ideogram") <- p.ideo
  attr(p, "xlabel") <- xlabel
  attr(p, "ideogram.data") <- obj.ori
  attr(p, "subchr") <- subchr
  attr(p, "aspect.ratio") <- aspect.ratio
  attr(p, "color") <- color
  attr(p, "fill") <- fill
  attr(p, "alpha") <- alpha
  p <- ideogram(p)
  p
}

plotSingleChrom <- plotIdeogram
setMethod("+", c("ideogram"), function(e1, e2){
  ## p <- attr(e1, "ideogram")
  obj <- attr(e1, "ideogram.data")
  if("chr" %in% names(attributes(e2))){
    subchr <- attr(e2, "chr")
  }else{
    subchr <- attr(e1, "subchr")
  }
  aspect.ratio <- attr(e1, "aspect.ratio")
  xlabel <- attr(e1, "xlabel")
  xlab <- attr(e1, "xlab") 
  ylab <- attr(e1, "ylab")
  main <- attr(e1, "main")
  color <- attr(e1, "color") 
  fill <- attr(e1, "fill")
  alpha <- attr(e1, "alpha")
  
  zoom.region <- e2$limits$x
  if(length(zoom.region)){
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          seqnames = base::unique(as.character(GenomicRanges::seqnames(obj))))
    p <- plotSingleChrom(obj, subchr, zoom.region,
                         xlab = xlab, ylab = ylab, main = main, xlabel = xlabel,
                         color = color, fill = fill, alpha = alpha,
                         cytoband = TRUE, aspect.ratio = aspect.ratio)  
  }else{
    p <- e1
  }
  p
})

## "+.ideogram" <- function(e1, e2){
## }

## setMethod("+", c("ideogram"), function(e1, e2){
##     p <- attr(e1, "ideogram")
##   obj <- attr(e1, "ideogram.data")
##   if("chr" %in% names(attributes(e2))){
##     subchr <- attr(e2, "chr")
##   }else{
##     subchr <- attr(e1, "subchr")
##   }
##   aspect.ratio <- attr(e1, "aspect.ratio")
##   xlabel <- attr(e1, "xlabel")
##   xlab <- attr(p, "xlab") 
##   ylab <- attr(p, "ylab")
##   main <- attr(p, "main")
##   if(is.null(xlab))
##     xlab <- ""
##   if(is.null(ylab))
##     ylab <- ""
##   if(is.null(main))
##     main <- ""
##   zoom.region <- e2$limits$x
##   zoom.df <- data.frame(x1 = zoom.region[1],
##                         x2 = zoom.region[2],
##                         seqnames = base::unique(as.character(GenomicRanges::seqnames(obj))))
##   p <- plotSingleChrom(obj, subchr, zoom.region,
##                        xlab = xlab, ylab = ylab, main = main, xlabel = xlabel,
##                        cytoband = TRUE, aspect.ratio = aspect.ratio)  
##   p
## })



## ## ======================================================================
## ##        For "Overview"
## ## ======================================================================
plotStackedOverview <- function(obj, ..., xlab, ylab, main, geom = "rect",
                         cytoband = FALSE, rescale = TRUE, rescale.range = c(0, 10)){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  facets <- seqnames ~ .
  if(missing(obj)){
    obj <- getIdeogram(cytoband = cytoband)
    cat("-------get following seqnames------\n")
    message(paste(seqnames(seqinfo(obj)), collapse = "\n"))
    ## obj <- keepSeqlevels(obj, unique(seqnames()))
    idx <- order(seqlengths(obj), decreasing = TRUE)
    nms <- names(seqlengths(obj))[idx]
    obj <- keepSeqlevels(obj, nms)
    p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, facets = facets)
  }else{
  if(!is(obj, "GRanges"))
    stop("only GRanges supported now")
  ## tweak with y
  if(rescale){
  if("y" %in% names(args.aes)){
    values(obj)[, as.character(args.aes$y)] <-
      rescale(values(obj)[, as.character(args.aes$y)],rescale.range)

  }}
  p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, facets = facets)
  args.non$geom <- geom
  args.non$facets <- facets
  if(!cytoband){
    args.res <- c(list(data = obj), list(do.call(aes, args.aes)),args.non)
    p <- p + do.call(layout_karyogram,args.res)
  }
}
  if(!missing(xlab))
    p <- p + xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)
  
  p
}

plotKaryogram <- plotStackedOverview

