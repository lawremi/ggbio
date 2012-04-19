## ======================================================================
##        For "Overview"
## ======================================================================
plotStackedOverview <- function(obj, ..., xlab, ylab, main, geom = "rect",
                         cytoband = FALSE, rescale = TRUE, rescale.range = c(0, 10)){

  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  facets <- seqnames ~ .
  if(missing(obj)){
    obj <- getIdeogram(cytobands = cytobands)
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
    p <- p + opts(title = main)
  
  p
}


plotSingleChrom <- function(obj, subchr, zoom.region,
                            xlab, ylab, main, xlabel = FALSE){
  ## do we need subchr here
  if(!missing(subchr)){
    obj <- obj[seqnames(obj) == subchr]
    obj <- keepSeqlevels(obj, subchr)
  }
  if(length(unique(as.character(seqnames(obj))))>1)
    stop("Mulptiple chromosome information found")
  p <- ggplot() + layout_karyogram(obj, cytoband = TRUE) 
  if(!missing(zoom.region)){
    if(length(zoom.region) != 2)
      stop("zoom.region must be a numeric vector of length 2")
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          seqnames = unique(as.character(seqnames(obj))))
    p <- p + ggplot2::geom_rect(data = zoom.df, aes(xmin = x1,
                         xmax = x2, ymin = -3, ymax = 13), color = "red", fill = NA)
  }
  if(!xlabel)
    p <- p + opts(axis.text.x = theme_blank())
  
  p <- p + theme_alignment(grid = FALSE, label = TRUE, border = FALSE) +
    scale_y_continuous(breaks = 5, label = subchr) +
      opts(strip.background = theme_rect(colour = 'NA', fill = 'NA'))+ 
        opts(strip.text.y = theme_text(colour = 'white'))   + opts(legend.position = "none")+
          ggplot2::xlab("")

  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  else
    p <- p + ggplot2::ylab(subchr)
  if(!missing(main))
    p <- p + opts(title = main)
  p
}


