GeomGSegment <- ggproto("GeomGSegment", GeomSegment,
    required_aes = c("x", "xend", "y", "yend")
)

geom_gsegmment <- function(data = NULL, mapping = NULL, stat = "identity",
                           position = "identity", ..., arrow = NULL,
                           arrow.fill = NULL, lineend = "butt",
                           linejoin = "round", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
    layer <- layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomGSegment,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            arrow.fill = arrow.fill,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
    .changeStrandColor(layer, mapping)
}

setGeneric("geom_segment", function(data, ...) standardGeneric("geom_segment"))

setMethod("geom_segment", "ANY", function(data, ...) {
    ggplot2::geom_segment(data  = data, ...)
})

setMethod("geom_segment", "GRanges",
          function(data = NULL, mapping = NULL, ...,
                   facets = ~seqnames,
                   stat = c("stepping", "identity"),
                   group.selfish = TRUE, extend.size = 0,
                   xlab = "Genomic Coordinates", na.rm = FALSE,
                   ylab, main) {
    stat <- match.arg(stat)
    y_scale <- NULL

    if (identical(stat, "identity")) {
        if (!"y" %in% names(mapping) &&
            !all(c("y","yend", "x", "xend") %in% names(mapping)))
            stop("aes(x =, xend= , y =, yend= ) is required for stat 'identity',
              you could also specify aes(y =) only as alternative", call. = FALSE)
        y <- quo_name(mapping$y)
        default <- aes(x = start, xend = end, y = !!mapping$y, yend = !!mapping$y)
        mapping <- aes_merge(default, mapping)
    } else if (identical(stat, "stepping")) {
        default <- aes(x = start, xend = end, y = stepping, yend = stepping)
        mapping <- aes_merge(default, mapping)
        group <- if (is.null(mapping$group)) NULL else quo_name(mapping$group)
        data <- data |> stepping(group = group, group.selfish = group.selfish,
                                 extend.size = extend.size)

        y <- "stepping"
        if (is.null(group))
            group <- "stepping"

        df <- elementMetadata(data)
        df <- group_df(df, group)
        y_scale <- scale_y_continuous_by_group(df, group, group.selfish)
    }

    c(geom_gsegmment(fortify(data), mapping, na.rm = na.rm, ...),
      facet_grid(facets),
      y_scale,
      Labels(xlab, ylab, main, fallback = c(y = y)))
})
