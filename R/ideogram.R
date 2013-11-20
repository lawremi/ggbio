## Ideogram has a special response to +xlim() method
setClass("Ideogram", contains = c("GGbio"))
Ideogram <- function(x){
  new("Ideogram", ggbio(x))  
}

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
  if(!biovizBase:::isIdeogram(obj))
    cytoband <- FALSE
  p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, geom = NULL)
  p.ideo <- p
  if(!missing(zoom.region)){
    if(length(zoom.region) != 2)
      stop("zoom.region must be a numeric vector of length 2")
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          y1 = 0 - zoom.offset,
                          y2 = 10 + zoom.offset,
                          seqnames = unique(as.character(seqnames(obj))))
    p <- p + ggplot2::geom_rect(data = zoom.df,
                                do.call(aes, list(xmin = substitute(x1),
                                                  xmax = substitute(x2),
                                                  ymin = substitute(y1),
                                                  ymax = substitute(y2))),
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
  attr(p, "size") <- size
  attr(p, "zoom.offset") <- zoom.offset
  p <- Ideogram(p)
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
  size <- attr(e1, "size")
  zoom.offset <- attr(e1, "zoom.offset")
  zoom.region <- e2$limits$x
  if(length(zoom.region)){
    p <- plotSingleChrom(obj, subchr, zoom.region,
                         xlab = xlab, ylab = ylab, main = main, xlabel = xlabel,
                         color = color, fill = fill, alpha = alpha,
                         size = size, zoom.offset = zoom.offset,
                         cytoband = TRUE, aspect.ratio = aspect.ratio)  
  }else{
    p <- e1
  }
  p
})

