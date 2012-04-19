setGeneric("stat_identity", function(data, ...) standardGeneric("stat_identity"))
setMethod("stat_identity", "data.frame", function(data, ...){
  ggplot2::stat_identity(data = data, ...)
})

setMethod("stat_identity", "GRanges", function(data, ..., geom = NULL){
  args <- list(...)
  gr.geoms <- c("chevron", "arrow", "arrowrect", "segment", "rect", "alignment")
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)
  if(is.null(geom))
    geom <- "segment"
  if(!geom %in% gr.geoms){
    args$geom <- geom
    data <- fortify(data)
    args$data <- data
    p <- do.call(ggplot2::stat_identity, args)
  }else{
    .geom.fun <- getGeomFun(geom)
    args$stat <- "identity"
    args$data <- data
    p <- do.call(.geom.fun, args)
  }
  p <- c(list(p), list(facet))
  p
})

