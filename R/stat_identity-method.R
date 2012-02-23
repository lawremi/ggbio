setGeneric("stat_identity", function(data, ...) standardGeneric("stat_identity"))
setMethod("stat_identity", "data.frame", function(data, ...){
  ggplot2::stat_identity(data = data, ...)
})

setMethod("stat_identity", "GRanges", function(data, ...){
  dots <- list(...)
  geom <- dots$geom
  gr.geoms <- c("chevron", "arrow", "5poly", "arch")
  if(!geom %in% gr.geoms){
    data <- fortify(data = data)
    p <- ggplot2::stat_identity(data = data, ...)
  }
  p
})

