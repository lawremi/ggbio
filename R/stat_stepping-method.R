setGeneric("stat_stepping", function(data, ...) standardGeneric("stat_stepping"))

setMethod("stat_stepping", "GRanges", function(data, ..., facets = NULL,
                                               rect.height = 0.4,
                                               geom = c("rect",
                                                 "alignment", "segment")){

  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args$stat <- "stepping"
  ## p <- switch(geom,
  ##             rect = geom_rect(data, ..., facets = facets, rect.height = rect.height,
  ##               stat = "stepping"),
  ##             alignment = geom_alignment(data, ..., facets = facets, rect.height = rect.height,
  ##               stat = "stepping"),
  ##             segment = .geom_segment(data, ..., facets = facets, rect.height = rect.height,
  ##               stat = "stepping"))
  p <- switch(geom,
              rect = do.call(geom_rect, args),
              alignment = do.call(geom_alignment, args),
              segment = do.call(geom_segment, args))
  p
  
})






