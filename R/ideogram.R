## Ideogram has a special response to +xlim() method
setClass("Ideogram", contains = c("GGbio"),
         slots = list(xlabel = "logical",
             cytoband = "logical",
             subchr = "characterORNULL",
             aspect.ratio = "numeric",
             color = "character",
             fill = "character",
             alpha = "numeric",
             size = "numeric",
             zoom.region = "numericORNULL",
             zoom.offset = "numeric"))

Ideogram <- function(obj, subchr = NULL, which = NULL, xlabel = FALSE, cytoband = TRUE,
                     color = "red", fill = "red", alpha = 0.7,
                     zoom.region = NULL,
                     zoom.offset = 0.2, size = 1,
                     aspect.ratio = 1/20, ..., genome){
    if(missing(obj)){
        data(ideoCyto, package = "biovizBase")
        if(genome %in% names(ideoCyto)){
          obj <- ideoCyto[[genome]]
        }else{
          obj <- getIdeogram(genome = genome, subchr = subchr, cytobands = cytobands)
        }
    }
    ## do we need subchr here
    obj.ori <- obj
    if(length(subchr)){
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
    if(!biovizBase::isIdeogram(obj))
        cytoband <- FALSE

    p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, geom = NULL)
    ## p.ideo <- p

    if(length(zoom.region)){
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
    p <- p + theme(aspect.ratio = aspect.ratio, axis.ticks.y = element_blank())
    if(!xlabel)
        p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    new("Ideogram", ggbio(p, data = obj.ori, ...), subchr = subchr, xlabel = xlabel,
        cytoband = cytoband, color = color, fill = fill, alpha = alpha,
        zoom.offset = zoom.offset, size = size, aspect.ratio = aspect.ratio)
}

setMethod("print", "Ideogram", function(x){
    ## essentially a karyogram with single chrom
    obj <- x@data
    zoom.region <- x@zoom.region
    xlabel <- x@xlabel
    subchr <- x@subchr
    aspect.ratio <- x@aspect.ratio
    color <- x@color
    fill <- x@fill
    alpha <- x@alpha
    size <- x@size
    zoom.region <- x@zoom.region
    zoom.offset <- x@zoom.offset
    cytoband <- x@cytoband

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
    if(!biovizBase::isIdeogram(obj))
        cytoband <- FALSE

    p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, geom = NULL)
    ## p.ideo <- p

    if(length(zoom.region)){
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
    p <- p + theme(aspect.ratio = aspect.ratio, axis.ticks.y = element_blank())
    if(!xlabel)
        p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    x@ggplot <- p
    x
})

setMethod("show", "Ideogram", function(object){
    object <- print(object)
    print(object@ggplot)
})

plotIdeogram <- function(obj, subchr = NULL, zoom.region = NULL, which = NULL,
                         xlab, ylab, main, xlabel = FALSE,
                         color = "red", fill = "red", alpha = 0.7,
                         zoom.offset = 0.2, size = 1,
                         cytoband = TRUE, aspect.ratio = 1/20, genome){
    if(!is.null(which) && is(which, "GRanges")){
        if(length(which) > 1){
            message("only first region used")
            which <- which[1]
        }
        subchr <- as.character(seqnames(which))
        zoom.region <- c(start(which), end(which))
    }

    p <- Ideogram(obj,
                  xlabel = xlabel,
                  subchr = subchr,
                  aspect.ratio = aspect.ratio,
                  color = color,
                  fill = fill,
                  alpha = alpha,
                  size = size,
                  zoom.region = zoom.region,
                  zoom.offset = zoom.offset,
                  genome = genome)

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
    p
}

setMethod("+", c("Ideogram"), function(e1, e2){
    if(inherits(e2, "xlim")){
        if("chr" %in% names(attributes(e2))){
            subchr <- attr(e2, "chr")
            if(length(subchr))
                e1@subchr <- subchr
        }
    }
    if(inherits(e2, "cartesian")){
        zoom.region <- e2$limits$x
    }else{
        zoom.region <- NULL
    }
    if(length(zoom.region)){
        e1@zoom.region <- zoom.region
    }
    e1 <- print(e1)
    e1
})

