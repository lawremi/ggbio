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
                rect = do.ggcall(geom_rect, args),
                alignment = do.call(geom_alignment, args),
                segment = do.ggcall(geom_segment, args))

  }else{
    p <- NULL
  }
  if(missing(xlab)) 
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("")))
  
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)
  p
})



