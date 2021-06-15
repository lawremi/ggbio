setGeneric("geom_bar", function(data, ...) standardGeneric("geom_bar"))

setMethod("geom_bar", "ANY", function(data, ...) {
    ggplot2::geom_bar(data  = data, ...)
})

## alignment should be convenient toggle with chevron...
setMethod("geom_bar", "GRanges",
          function(data = NULL, mapping = NULL, ...,
                   facets = ~seqnames,
                   xlab = "Genomic Coordinates",
                   ylab, main) {
    if(!"y" %in% names(mapping)) {
        if("score" %in% colnames(values(data))) {
            message("use score as y by default")
            mapping$y <- as.name("score")
        } else {
            stop("missing y values in aes(), or please provide a column named 'score'",
                 call. = FALSE)
        }
    }

    y <- quo_name(mapping$y)
    mapping <- aes(xmin = start, xmax = end, ymin = 0, ymax = !!as.name(y))

    c(geom_rect(fortify(data), mapping, ...),
      facet_grid(facets),
      Labels(xlab, ylab, main, fallback = c(y = y)))
})
