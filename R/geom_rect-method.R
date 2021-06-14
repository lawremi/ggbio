## TODO::
## Let's load a RefSeq data
## naming the interval
## two mode? packed, full with name (default)
## reduce is just a stat transformation at lower level
GeomGRect <- ggproto("GeomGRect", GeomRect,
    required_aes = c("xmin", "xmax", "ymin|y", "ymax|y"),

    extra_params = c("rect.height"),

    draw_key = GeomRect$draw_key,

    setup_data = function(data, params) {
        if (!is.null(data$y)) {
            params$rect.height <- params$rect.height %||%
                                  (diff(range(data$y))/10)
            data$ymin <- data$y + params$rect.height
            data$ymax <- data$y
        }
        data
    },

    draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
        ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
    }
)

geom_grect <- function(data = NULL, mapping = NULL, stat = "identity",
                       position = "identity", ..., na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE,
                       rect.height = NULL) {
    layer <- layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomGRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            rect.height = rect.height,
            na.rm = na.rm,
            ...
        )
    )
    .changeStrandColor(layer, mapping)
}

setGeneric("geom_rect", function(data, ...) standardGeneric("geom_rect"))

setMethod("geom_rect", "ANY", function(data, ...) {
    ggplot2::geom_rect(data = data, ...)
})

setMethod("geom_rect", "GRanges",
          function(data = NULL, mapping = NULL, ...,
                   facets = ~seqnames, rect.height = NULL,
                   stat = c("stepping", "identity"),
                   group.selfish = TRUE, na.rm = FALSE,
                   group = NULL, extend.size = 0,
                   xlab = "Genomic Coordinates",
                   ylab, main) {
    stat <- match.arg(stat)
    y_scale <- NULL

    if (identical(stat, "identity")) {
        if (!"y" %in% names(mapping) &&
            !all(c("xmin", "xmax", "ymin", "ymax") %in% names(mapping)))
            stop("aes(xmin =, xmax= , ymin =, ymax= ) is required for stat 'identity',
                  you could also specify aes(y =) only as alternative", .Call = FALSE)
        mapping <- aes_merge(aes(xmin = start, xmax = end), mapping)
        ylab <- quo_name(mapping$y %||% mapping$ymin)
    } else if (identical(stat, "stepping")) {
        mapping <- mapping %||% aes(y = stepping)
        mapping <- aes_merge(aes(xmin = start, xmax = end), mapping)
        rect.height <- rect.height %||% 0.8
        data <- data |> stepping(group = group, group.selfish = group.selfish,
                                 extend.size = extend.size)

        if (missing(ylab))
            ylab <- ""
        if (is.null(group))
            group <- "stepping"

        df <- elementMetadata(data)
        df <- group_df(df, group)
        y_scale <- scale_y_continuous_by_group(df, group, group.selfish)
    }

    c(geom_grect(fortify(data), mapping, rect.height = rect.height,
      na.rm = na.rm, ...),
      facet_grid(facets),
      y_scale,
      Labels(xlab, ylab, main))
})
