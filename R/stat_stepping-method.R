setGeneric("stat_stepping", function(data, ...) standardGeneric("stat_stepping"))

setMethod("stat_stepping", "GRanges", function(data, ...,
                                               xlab, ylab, main,
                                               facets = NULL,
                                               geom = c("rect",
                                                 "alignment", "segment")){


  geom <- match.arg(geom)
  args <- list(...)
  args$facets <- facets
  args$stat <- "stepping"
  args$data <- data
  if(length(data)){
    p <- switch(geom,
                rect = do.call(geom_rect, args),
                alignment = do.call(geom_alignment, args),
                segment = do.call(geom_segment, args))
  }else{
    p <- NULL
  }
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("Stepping")))
  
  if(!missing(main))
    p <- c(p, list(opts(title = main)))

  p
})



