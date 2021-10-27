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
  labels <- Labels(xlab, ylab, main, fallback = c(x = "", y = ""))
  p <- c(p, labels)
  p <- setStat(p)
  p
})



