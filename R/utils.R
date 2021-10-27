setGeneric("getLimits", function(obj, ...) standardGeneric("getLimits"))

setMethod("getLimits", "GGbio", function(obj) {
    .getLimits(obj@ggplot)
})

setMethod("getLimits", "ggplotPlot", function(obj) {
    .getLimits(obj)
})

setMethod("getLimits", "ggbioPlot", function(obj) {
    .getLimits(obj@ggplot)
})

# SPECIAL operator to get values from the list(b) and
# assign those values to variables(created from (a) in the
# calling environment)
"%<->%" <- function(a, b) {
    lapply(seq_len(length(a)), function(x) {
              assign(a[x], b[[x]], envir = parent.frame(3))
    })
    invisible(b)
}

.getLimits <- function(obj) {
    x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
    names <- c("x", "y", "xmin", "ymin", "xmax", "ymax", "xend", "yend")
    mapped_list <- lapply(names ,function(x) {
        if (!is.null(obj$mapping[[x]]) && length(obj$data))
            eval_tidy(obj$mapping[[x]], obj$data)
    })
    # assign values of mapped_list to x,y,xmin etc
    names %<->% mapped_list

    # get limits from layer
    layer <- suppressWarnings(getLimitsFromLayer(obj))

    xlim <- suppressWarnings(c(min(c(layer$xmin, x, xmin), na.rm = TRUE),
                               max(c(layer$xmax,x, xmax, xend), na.rm = TRUE)))
    ylim <- suppressWarnings(c(min(c(layer$ymin, y, ymin), na.rm = TRUE),
                               max(c(layer$ymax, y, ymax, yend), na.rm = TRUE)))

    res <- list(xlim = xlim, ylim = ylim)

    if (any(unlist(res) %in% c(Inf, -Inf)))
        res <- evalLan(obj)

    if (length(obj$coordinates$limits$x) == 2)
        res$xlim <- obj$coordinates$limits$x

    if (length(obj$coordinates$limits$y) == 2)
        res$ylim <- obj$coordinates$limits$y

    scales <- suppressWarnings(getLimitsFromScales(obj))
    scales <- as.list(scales)
    scales <- lapply(scales, function(x) {
                        if (x %in% c(-Inf, Inf)) {
                            NULL
                        } else {
                            x
                        }
                    })
    if (!is.null(scales$xmin) & !is.null(scales$xmax))
        res$xlim <- c(scales$xmin, scales$xmax)
    if (!is.null(scales$ymin) & !is.null(scales$ymax))
        res$ylim <- c(scales$ymin, scales$ymax)
    res
}

getLimitsFromScales <- function(obj) {
    scal <- obj$scales$scales
    lst <- lapply(scal, function(x) {
      x <- scal[[1]]
      if (!is.null(x$limits)) {
          limits <- x$limits
          res <- NULL
          if (any(x$aesthetics %in% c("x", "xmin", "xmax", "xend", "xintercept",
                                      "xmin_final", "xmax_final"))) {
              res <- data.frame(xmin = limits[1], xmax = limits[2],
                                ymin = NA, ymax = NA)
          }
          if (any(x$aesthetics %in% c("y", "ymin", "ymax", "yend", "yintercept",
                                      "ymin_final", "ymax_final"))) {
              res <- data.frame(ymin = limits[1], ymax = limits[2],
                                xmin = NA, xmax = NA)
          }
      } else {
          res <- NULL
      }
      res
    })
    lst <- lst[!is.null(lst)]
    res <- do.call("rbind", lst)
    res <- data.frame(xmin = min(res$xmin, na.rm = TRUE),
                      xmax = max(res$xmax, na.rm = TRUE),
                      ymin = min(res$ymin, na.rm = TRUE),
                      ymax = max(res$ymax, na.rm = TRUE))
    res
}

getLimitsFromLayer <- function(obj) {
    layers <- obj$layer
    lst <- lapply(layers, function(layer) {
        if (length(obj$data) | length(layer$data)) {
            if(length(layer$data))
                dt <- layer$data
            else
                dt <- obj$data

            if (!is.null(layer$mapping)) {
                x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
                names <- c("x", "y", "xmin", "ymin", "xmax", "ymax", "xend", "yend")
                mapped_list <- lapply(names ,function(x) {
                    if (!is.null(layer$mapping[[x]]))
                        eval_tidy(layer$mapping[[x]], dt)
                })
                 # assign values of mapped_list to x,y,xmin etc
                names %<->% mapped_list

                res <- data.frame(xmin = min(c(x, xmin), na.rm = TRUE),
                                  xmax = max(c(x, xmax, xend), na.rm = TRUE),
                                  ymin = min(c(y, ymin), na.rm = TRUE),
                                  ymax = max(c(y, ymax, yend), na.rm = TRUE))
            } else {
                res <- NULL
            }
      } else {
          res <- NULL
      }
    })
    lst <- lst[!is.null(lst)]
    res <- do.call("rbind", lst)
    res
}

evalLan <- function(obj) {
    x <- obj$mapping$x
    y <- obj$mapping$y
    xlim <- ylim <- NULL
    if (is_quosure(x) & is_quosure(y)) {
        xlim <- range(eval_tidy(x))
        ylim <- range(eval_tidy(y))
    }
    list(xlim = xlim, ylim = ylim)
}

getGeomFun <- function(geom) {
    match.fun(paste("geom_", geom, sep = ""))
}
getStatFun <- function(stat) {
    match.fun(paste("stat_", stat, sep = ""))
}
getDrawFunFromGeomStat <- function(geom, stat) {
    ## how about allways start from geom??
    if (!is.null(stat)) {
      .fun <- getStatFun(stat)
    } else {
      .fun <- getGeomFun(geom)
    }
    .fun
}

.changeStrandColor <- function(p, args, fill = TRUE) {
    strandColor <- getOption("biovizBase")$strandColor
    isStrand.color <- FALSE
    isStrand.fill <- FALSE
    ## default with no color
    idx <- c("color", "colour") %in% names(args)
    if ((any(idx))) {
        nms <- c("color", "colour")[idx][1]
        if (quo_name(args[[nms]]) == "strand")
            isStrand.color <- TRUE
    }
    if (("fill" %in% names(args))) {
        if (quo_name(args$fill) == "strand")
            isStrand.fill <- TRUE
    }
    if (isStrand.color)
        p <- c(list(p), list(scale_color_manual(values = strandColor)))
    if (fill) {
        if (isStrand.fill)
            p <- c(p, list(scale_fill_manual(values = strandColor)))
    }
    p
}

## need to consider a length 1 facets formula
.buildFacetsFromArgs <- function(object, args) {
    isOneSeq <- length(unique(as.character(seqnames(object)))) == 1
    args.facets <- args
    args.facets$facets <- strip_formula_dots(args$facets)
    facets <- args.facets$facets
    facet.logic <- NULL

    if (length(facets)) {
        biovizBase:::.checkFacetsRestrict(facets, object)
        if (is(facets, "GRanges")) {
            args.facets$facets <- substitute(~.bioviz.facetid)
            if (!("scales" %in% names(args.facets)))
                args.facets$scales <- "free"
        } else {
            if (!("scales" %in% names(args.facets)))
                args.facets <- c(args.facets, list(scales = "fixed"))
            allvars <- all.vars(as.formula(args.facets$facets))
        }
    } else {
        if (!("scales" %in% names(args.facets)))
            args.facets <- c(args.facets, list(scales = "fixed"))
        args.facets$facets <- substitute(~seqnames)
        allvars <- all.vars(as.formula(args.facets$facets))
    }

    if (isOneSeq & biovizBase:::isFacetByOnlySeq(args.facets$facets)) {
        facet <- NULL
        return(facet)
    }

    facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                          TRUE, FALSE)

    if (facet.logic)
        facet <- do.call(facet_wrap, args.facets)
    else
        facet <- do.call(facet_grid, args.facets)
    facet
}

sub_names <- function(data, name.expr) {
    .res <- c()
    for (i in seq_len(nrow(data))) {
      res <- data[i,]
      res <- as.list(res)
      res <- lapply(res, function(x) {
        if (is.numeric(x))
            return(as.character(as.name(x)))
        else
            return(as.character(x))
      })
      subfun <- function(res, name.expr) {
          nm <- names(res[1])
          val <- res[[1]]
          name.expr <- gsub(nm, val, name.expr)
          if (!length(res) == 1)
            subfun(res[-1], name.expr)
          else
            return(name.expr)
      }
      .res <- c(.res, subfun(res, name.expr))
    }
    .res
}

setGeneric("subsetByChrs", function(obj, ...) starndardGeneric("subByChr"))

setMethod("subsetByChrs", "GRanges", function(obj, subchr) {
    if (missing(subchr))
        subchr <- as.character(seqnames(obj)[1])
    res <- obj[seqnames(obj) %in% subchr]
    res <- keepSeqlevels(res, subchr)
    res
})

setMethod("subsetByChrs", "Seqinfo", function(obj, subchr){
  if (missing(subchr))
      subchr <- as.character(seqnames(obj)[1])
  res <- obj[subchr]
  res
})

ggsave <- function (filename, plot = last_plot(),
                    device = NULL, path = NULL, scale = 1,
                    width = NA, height = NA, units = c("in", "cm", "mm"),
                    dpi = 300, limitsize = TRUE, ...) {
    # take backup of original plot
    original_last_plot <- plot

    if (!inherits(plot, "ggplot") && !is(plot, "Tracks"))
        stop("plot should be a ggplot2 plot or tracks object")

    # for compatibility with ggplot2::ggsave convert derivative plot to grob
    if (is(plot, "Tracks"))
        plot <- as(plot, "grob")

    tryCatch(
        {
            ggplot2::ggsave(filename, plot, device, path, scale, width,
                            height, units, dpi, limitsize, ...)
        },
        finally = {
            # this ensures last plot point to correct plot
            # even if ggplot2::ggsave fails during run-time
            ggplot2::set_last_plot(original_last_plot)
        }
    )
}

## combineAes(keep, lost)
combineAes <- function(keep, lose) {
    keep.nms <- names(keep)
    lose.nms <- names(lose)

    nms <- intersect(lose.nms, keep.nms)

    if (length(nms))
        return(c(keep, lose[setdiff(lose.nms, keep.nms)]))
    else
        return(c(keep, lose))
}

zoomLevelToGeom <- function(zoomLevel, track = c("BSgenome", "VRanges")) {
    track <- match.arg(track)
    .level1 <- 100 # text
    .level2 <- 500 # rect
    .level3 <- 2000 # segment
    geom <- switch(track,
                   "BSgenome" = {
                       if (zoomLevel < .level1) {
                           g <- "text"
                       } else if (zoomLevel >= .level1 && zoomLevel < .level2) {
                           g <- "rect"
                       } else if (zoomLevel >= .level2 && zoomLevel < .level3) {
                           g <- "segment"
                       } else {
                           g <- "none"
                       }
                   },
                   "VRanges" = {
                       if (zoomLevel < .level1) {
                           g <- "text"
                       } else if (zoomLevel >= .level1 && zoomLevel < .level3) {
                           g <- "rect"
                       } else {
                           g <- "none"
                       }
                   })
    geom
}

by2 <- function(...) {
    ans <- by(...)
    class(ans) <- "list"
    ans
}

# returns NULL_OR_list
# if x, y and main are missing then NULL will be returned.
Labels <- function(x, y, main, fallback) {
    labels <- c()
    xflag <- yflag <- mainflag <- FALSE
    if (!missing(fallback)) {
        if ("x" %in% names(fallback) && missing(x)) {
            xflag <- TRUE
            x <- fallback[["x"]]
        }
        if ("y" %in% names(fallback) && missing(y)) {
            yflag <- TRUE
            y <- fallback[["y"]]
        }
        if ("main" %in% names(fallback) && missing(main)) {
            mainflag <- TRUE
            main <- fallback[["main"]]
        }
    }

    # When flag is TRUE, label must be created
    if (!missing(x) || xflag) {
        stopifnot(is.character(x))
        labels <- c(labels, list(xlab(x)))
    }
    if (!missing(y) || yflag) {
        stopifnot(is.character(y))
        labels <- c(labels, list(ylab(y)))
    }
    if (!missing(main) || mainflag) {
        stopifnot(is.character(main))
        labels <- c(labels, list(labs(title = main)))
    }
    return(labels)
}

remove_args <- function(args, remove) {
    args[!names(args) %in% remove]
}

build_facet <- function(data, args) {
    args <- subsetArgsByFormals(args, facet_grid, facet_wrap)
    .buildFacetsFromArgs(data, args)
}

make_addStepping <- function(gr, args, group.selfish, ...) {
    if("group" %in% names(args)) {
        addStepping(gr, group.name = quo_name(args$group),
                    group.selfish = group.selfish, ...)
    } else {
        addStepping(gr, ...)
    }
}

group_df <- function(df, group) {
    .df.sub <- df[, c("stepping", group)]
    .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
}

scale_y_continuous_by_group <- function(df, group, group.selfish) {
     if(group != "stepping" & group.selfish) {
      list(scale_y_continuous(breaks = df$stepping,
                              labels = as.character(df[, group])))
    } else {
      list(scale_y_continuous(breaks = NULL))
    }
}

"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}
