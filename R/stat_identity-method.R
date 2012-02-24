setGeneric("stat_identity", function(data, ...) standardGeneric("stat_identity"))
setMethod("stat_identity", "data.frame", function(data, ...){
  ggplot2::stat_identity(data = data, ...)
})

setMethod("stat_identity", "GRanges", function(data, ..., geom = NULL){
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  gr.geoms <- c("chevron", "arrow", "5poly", "segment", "rect", "alignment")
  if(is.null(geom))
    geom <- "segment"
  if(!geom %in% gr.geoms){
    data <- fortify(data = data)
    p <- ggplot2::stat_identity(data = data, ...)
  }else{
    .geom.fun <- getGeomFun(geom)
    args$stat <- "identity"
    args$data <- data
    p <- do.call(.geom.fun, args)
  }
})

