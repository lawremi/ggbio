setGeneric("layout_linear", function(data,...)
           standardGeneric("layout_linear"))

setMethod("layout_linear", "GRanges", function(data, ...){
  p <- autoplot(data, ...)
  p <- facet_grid(scales = "free_x", space = "free_x") +
          scale_x_continuous(breaks = NULL, expand = c(0, 0))
  p
})






