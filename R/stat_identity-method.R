setGeneric("stat_identity", function(data, ...) standardGeneric("stat_identity"))
setMethod("stat_identity", "data.frame", function(data, ...){
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  do.call(ggplot2::stat_identity, args)
})

setMethod("stat_identity", "GRanges", function(data, ..., geom = NULL){
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  gr.geoms <- c("chevron", "arrow", "arrowrect", "segment", "rect", "alignment")
  if(is.null(geom))
    geom <- "segment"
  if(!geom %in% gr.geoms){
    data <- fortify(data)
    args$data <- data
    p <- do.call(ggplot2::stat_identity, args)
  }else{
    .geom.fun <- getGeomFun(geom)
    args$stat <- "identity"
    args$data <- data
    p <- do.call(.geom.fun, args)
  }
})

