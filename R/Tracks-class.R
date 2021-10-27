setClassUnion("character_OR_expression_OR_NULL",
              c("expression", "character_OR_NULL"))

setClass("Tracks", representation(grobs = "PlotList", # working plots, not reall 'Grob'
                                 plot = "list", # original plots passed into tracks
                                 backup = "list", # backup of the whole tracks object
                                 heights = "numericORunit",
                                 xlim = "numeric",
                                 ylim = "list",
                                 xlab = "character_OR_NULL",
                                 main = "character_OR_expression_OR_NULL",
                                 main.height =  "numericORunit",
                                 scale.height =  "numericORunit",
                                 xlab.height =  "numericORunit",
                                 theme = "theme_OR_NULL",
                                 fixed = "logical",
                                 labeled = "logical",
                                 mutable = "logical",
                                 hasAxis = "logical",
                                 padding = "numericORunit",
                                 label.bg.color = "character",
                                 label.bg.fill = "character",
                                 label.text.color = "character",
                                 label.text.cex = "numeric",
                                 label.text.angle = "numeric",
                                 track.plot.color = "character_OR_NULL",
                                 track.bg.color = "character_OR_NULL",
                                 label.width = "unit"))

.tracks.theme <- setdiff(slotNames("Tracks"), c("backup", "grobs"))

tracks <- function(..., heights, xlim, xlab = NULL, main = NULL,
                   title = NULL,
                   theme = NULL,
                   track.plot.color = NULL,
                   track.bg.color = NULL,
                   main.height = unit(1.5, "lines"),
                   scale.height = unit(1, "lines"),
                   xlab.height = unit(1.5, "lines"),
                   padding = unit(-1, "lines"),
                   label.bg.color =  "white",
                   label.bg.fill = "gray80",
                   label.text.color = "black",
                   label.text.cex = 1,
                   label.text.angle = 90,
                   label.width = unit(2.5, "lines")) {

    if (is.numeric(padding) && !is.unit(padding))
        padding <- unit(padding, "lines")

    if (is.numeric(main.height) && !is.unit(main.height))
        main.height <- unit(main.height, "lines")

    if (is.numeric(scale.height) && !is.unit(scale.height))
        scale.height <- unit(scale.height, "lines")

    if (is.numeric(xlab.height) && !is.unit(xlab.height))
        xlab.height <- unit(xlab.height, "lines")

    if (!is.null(title) && is.null(main))
        main <- title

    args <- list(...)
    dots <- reduceListOfPlots(args)
    ## return plots if not
    dots <- genPlots(dots)

    plotList <- do.call(plotList, dots)

    ## convert to Plot object with extra slots
    PlotList <- do.call(PlotList, dots)

    fixed <- vapply(PlotList, fixed, logical(1L))
    mutable <- vapply(PlotList, mutable, logical(1L))
    hasAxis <- vapply(PlotList, hasAxis, logical(1L))
    labeled <- vapply(PlotList, labeled, logical(1L))
    isIdeo <- vapply(PlotList, is, "Ideogram", FUN.VALUE = logical(1L))
    isBlank <- vapply(PlotList, function(x) x@blank, logical(1L))

    ## get height
    if (missing(heights)) {
        heights <- getHeight(PlotList)
    } else {
        heights <- parseHeight(heights, length(PlotList))
    }

    ## ylim
    ylim <- lapply(PlotList[!fixed & !isIdeo & !isBlank], function(grob) {
        scales::expand_range(getLimits(grob)$ylim, mul = 0.05)
    })

    wh <- NULL

    ## xlim
    if (missing(xlim)) {
      ### FIXME: this should just try to call range(unlist(x)) on each arg
      ###        and then call range(do.call(c, unname(r))) on the successful
      ###        results.
        idx <- vapply(args, function(x) is(x, "GenomicRanges_OR_GRangesList"), logical(1L))
        if (any(idx)) {
            grs <- args[idx]
            grs <- unlist(do.call(c, unname(grs)))
            chrs <- unique(as.character(seqnames(grs)))
            if (length(chrs) > 1) {
                stop("seqnames of passed GRanges has to be the same for tracks")
            }
            ir <- reduce(ranges(grs))
            wh <- GRanges(chrs, ir)
        }
        xid <- !fixed & !isIdeo & !isBlank
        if (sum(xid)) {
            lst <- lapply(PlotList[xid], function(obj) {
                res <- getLimits(obj)
                data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
            })
            res <- do.call(rbind, lst)
            xlim <- c(min(res$xmin), max(res$xmax))
            xlim <- scales::expand_range(xlim, mul = 0.1)
        } else {
            xlim <- c(0, 1)
        }
    } else {
      if (is(xlim, "IRanges")) {
          xlim <- c(start(xlim), end(xlim))
      }
      if (is(xlim,"GRanges")) {
          wh <- xlim
          xlim <- c(start(ranges(reduce(xlim, ignore.strand = TRUE))),
                    end(ranges(reduce(xlim, ignore.strand = TRUE))))
      }
      if (is.numeric(xlim)) {
          xlim <- range(xlim)
      }
    }

    ## sync xlim when construct them??
    if (!is.null(wh)) {
        PlotList <- lapply(PlotList, function(x) {
            x + xlim(wh)
        })
        PlotList <- do.call(PlotList, PlotList)
    }

    ## plot background
    N <- length(PlotList)
    if (is.null(track.plot.color)) {
        if (is.null(track.bg.color))
            track.plot.color <- vapply(PlotList, bgColor, character(1L))
        else
            track.plot.color <- rep(track.bg.color, length(PlotList))
    }
    stopifnot(length(track.plot.color) == N | length(track.plot.color) == 1)

    ## backup: record a state
    backup <- list(grobs = PlotList, plot = plotList, heights = heights,
                   xlim = xlim, ylim = ylim, xlab = xlab, main = main,
                   main.height = main.height, scale.height = scale.height,
                   xlab.height = xlab.height, theme = theme, mutable = mutable,
                   hasAxis = hasAxis, fixed = fixed, padding = padding,
                   labeled = labeled, label.bg.color = label.bg.color,
                   label.bg.fill = label.bg.fill,
                   label.text.color = label.text.color,
                   label.text.angle = label.text.angle,
                   track.plot.color = track.plot.color,
                   track.bg.color = track.bg.color,
                   label.text.cex = label.text.cex,
                   label.width = label.width)
    track_args <- list(backup = backup)
    track_args <- c("Tracks", track_args, backup)
    tracks <- do.call(new, track_args)
    ggplot2:::set_last_plot(tracks)
    tracks
}


setMethod("summary", "Tracks", function(object) {
    cat("-------------------------------------------\n")
    cat("Tracks contains: ", length(object@grobs), " graphic objects\n")
    cat("-------------------------------------------\n")
    cat("xlim:", object@xlim, "\n")
    cat("heights", object@heights, "\n")
    cat("fixed", object@fixed, "\n")
    cat("track.plot.color", object@track.plot.color, "\n")
    cat("-------------------------------------------\n")
})

setAs("Tracks", "grob", function(from) {
    grobs <- from@grobs
    N <- length(grobs)
    .scale.grob <- grobs[[N]] + xlim(from@xlim)

    if (any(from@labeled))
        nms <- names(from@grobs)
    else
        nms <- NULL

    lst <- lapply(seq_len(N), function(i) {
        if (i %in% which(from@mutable))
            grobs[[i]] <- grobs[[i]] + from@theme

        grobs[[i]] <- grobs[[i]] + ggplot2::xlab("") + labs(title = "")
        padding <- as.numeric(from@padding)
        grobs[[i]] <- grobs[[i]] +
                      theme(plot.margin = unit(c(padding, 1, padding, 0.5), "lines"))

        if (i %in% which(!from@hasAxis))
            grobs[[i]] <- grobs[[i]] + theme(axis.text.x = element_blank(),
                                             axis.ticks.x = element_blank())

        if (i %in% which(!from@fixed)) {
            s <- coord_cartesian(xlim = from@xlim)
            grobs[[i]] <- grobs[[i]] + s
        }

        grobs[[i]]
    })

    if (!is.null(nms))
        names(lst) <- nms

    if(any(from@labeled))
        do.call(alignPlots,
                c(lst, list(heights = from@heights,
                            padding = from@padding,
                            label.bg.color =  from@label.bg.color,
                            label.bg.fill = from@label.bg.fill,
                            label.text.color = from@label.text.color,
                            label.text.angle = from@label.text.angle,
                            label.text.cex = from@label.text.cex,
                            label.width = from@label.width,
                            track.plot.color = from@track.plot.color,
                            track.bg.color = from@track.bg.color,
                            main = from@main,
                            xlab = from@xlab,
                            main.height = from@main.height,
                            scale.height = from@scale.height,
                            xlab.height = from@xlab.height,
                            .scale.grob = .scale.grob
                            )))
    else
        do.call(alignPlots,
                c(lst, list(heights = from@heights,
                            padding = from@padding,
                            track.plot.color = from@track.plot.color,
                            track.bg.color = from@track.bg.color,
                            main = from@main,
                            xlab = from@xlab,
                            main.height = from@main.height,
                            scale.height = from@scale.height,
                            xlab.height = from@xlab.height,
                            .scale.grob = .scale.grob
                            )))
})

print.Tracks <- function(x) {
    grid.newpage()
    grid.draw(as(x, "grob"))
    ggplot2:::set_last_plot(x)
}

setMethod("show", "Tracks", function(object) {
    print(object)
    ggplot2:::set_last_plot(object)
})

setMethod("+", signature = c("Tracks", "ANY"), function(e1, e2) {
    N <- length(e1@grobs)
    .theme <- intersect(names(attributes(e2)), .tracks.theme)
    idx <- vapply(e1@grobs, mutable, logical(1L))
    for (i in seq_len(N)[idx]) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    if (length(.theme)) {
        for (z in seq_len(length(.theme))) {
            slot(e1, .theme[z]) <- attr(e2, .theme[z])
        }
    }
    e1
})

setMethod("+", signature = c("Tracks", "theme"), function(e1, e2) {
    N <- length(e1@grobs)
    .theme <- intersect(names(attributes(e2)), .tracks.theme)
    idx <- vapply(e1@grobs, mutable, logical(1L))
    for (i in seq_len(N)[idx]) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    if (length(.theme)) {
        for (z in seq_len(length(.theme))) {
            slot(e1, .theme[z]) <- attr(e2, .theme[z])
        }
    }
    e1@theme <- e2
    e1
})

setOldClass("zoom")
setMethod("+", signature = c("Tracks", "zoom"), function(e1, e2) {
    xlim <- e1@xlim
    e1@xlim <- .zoom(xlim, as.numeric(e2))$limits$x
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

setOldClass("position_c")
setMethod("+", signature = c("Tracks", "position_c"), function(e1, e2) {
    if ("x" %in% e2$aesthetics) {
        if (!is.null(e2$limits))
            e1@xlim <- e2$limits
    }
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

setOldClass("cartesian")
setMethod("+", signature = c("Tracks", "cartesian"), function(e1, e2) {
    if (!is.null(e2$limits$x))
        e1@xlim <- e2$limits$x
    if (!is.null(e2$limits$y)) {
        for (i in seq_len(length(e1@ylim))) {
            if (!fixed(e1@grobs[[i]]) && !is(e1@grobs[[i]], "Ideogram"))
                e1@ylim[[i]] <- e2$limits$y
        }
    }
    N <- length(e1@grobs)
    for (i in seq_len(N)) {
        if (!fixed(e1@grobs[[i]]))
            e1@grobs[[i]] <- e1@grobs[[i]] + e2
    }
    e1
})

xlim_car <- function(x) {
    class(x) <- c(class(x), "xlim")
    x
}

setMethod("xlim", "numeric", function(obj, ...) {
    if (length(list(...)))
        obj <- c(obj, ...)
    if (length(obj) > 2) {
        obj <- range(obj)
    }
    res <- ggplot2::coord_cartesian(xlim = obj)
    xlim_car(res)
})

setMethod("xlim", "IRanges", function(obj, ...) {
    xlim <- c(start(obj), end(obj))
    res <- ggplot2::coord_cartesian(xlim = xlim)
    xlim_car(res)
})

setMethod("xlim", "GRanges", function(obj, ...) {
    xlim <- c(start(ranges(reduce(obj, ignore.strand = TRUE))),
              end(ranges(reduce(obj, ignore.strand = TRUE))))
    res <- ggplot2::coord_cartesian(xlim = xlim)
    chr <- unique(as.character(seqnames(obj)))
    attr(res, "chr") <- chr
    attr(res, "ori") <- obj
    xlim_car(res)
})

setMethod("xlim", "Tracks", function(obj, ...) {
    obj@xlim
})

setReplaceMethod("xlim", c("Tracks", "IRanges"), function(x, value) {
    xlim <- c(start(value), end(value))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setReplaceMethod("xlim", c("Tracks", "GRanges"), function(x, value) {
    xlim <- c(start(ranges(reduce(value, ignore.strand = TRUE))),
              end(ranges(reduce(value, ignore.strand = TRUE))))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setReplaceMethod("xlim", c("Tracks", "numeric"), function(x, value) {
    xlim <- range(value)
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i) {
        ylim <- x@ylim[[i]]
        s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
        if (i %in% which(!x@fixed))
            x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setGeneric("reset", function(obj, ...) standardGeneric("reset"))
setMethod("reset", "Tracks", function(obj) {
    nms <- setdiff(slotNames(obj), "backup")
    for (nm in nms) {
        slot(obj, nm) <- obj@backup[[nm]]
    }
    xlim(obj) <- obj@xlim
    obj
})

setGeneric("backup", function(obj, ...) standardGeneric("backup"))
setMethod("backup", "Tracks", function(obj) {
    nms <- setdiff(slotNames(obj), "backup")
    for (nm in nms) {
        obj@backup[[nm]] <- slot(obj, nm)
    }
    obj
})

gtable_filter_grobs <- function(g, type) {
    rowSums(vapply(type, function(t) startsWith(g$layout$name, t),
                   logical(length(g$layout$name)))) > 0L
}

addLabel <- function(grobs, nms, lbs,
                     label.bg.color =  "white",
                     label.bg.fill = "gray80",
                     label.text.color = "black",
                     label.text.angle = 90,
                     label.text.cex = 1,
                     label.width = unit(2.5, "lines"),
                     direction = c("row", "col")) {
    direction <- match.arg(direction)
    if (length(label.text.angle) == 1)
        label.text.angle <- rep(label.text.angle, len = length(grobs))
    if (length(label.text.color) == 1)
        label.text.color <- rep(label.text.color, len = length(grobs))
    if (length(label.text.cex) == 1)
        label.text.cex <- rep(label.text.cex, len = length(grobs))
    if (length(label.bg.color) == 1)
        label.bg.color <- rep(label.bg.color, len = length(grobs))
    if (length(label.bg.fill) == 1)
        label.bg.fill <- rep(label.bg.fill, len = length(grobs))

    lapply(seq_len(length(grobs)), function(i) {
        if (identical(direction, "row")) {
            rot <- label.text.angle[i]
            width <- unit.c(label.width, unit(1, "null"))
            height <- unit(1, "null")
            l <- 2
            t <- 1
        } else {
            rot <- (90 - label.text.angle[i]) %% 360
            width <- unit(1, "null")
            height <- unit.c(label.width, unit(1, "null"))
            l <- 1
            t <- 2
        }

        grob <- grobs[[i]]

        if (lbs[i]) {
            rect <- rectGrob(gp = gpar(fill = label.bg.fill[i],
                             col = label.bg.color[i]))
            label <- textGrob(nms[i], rot = rot,
                              gp = gpar(col = label.text.color[i],
                              cex = label.text.cex[i]))
            x.grob <- grobTree(gTree(children = gList(rect, label)))
        } else {
            x.grob <- ggplot2::zeroGrob()
        }

        gt <- gtable(widths = width, heights = height)
        gt <- gtable_add_grob(gt, x.grob, l = 1, t = 1)
        gt <- gtable_add_grob(gt, grob, l = l, t = t)
    })
}

## TODO: adust due to left/right legend
alignPlots <- function(..., vertical = TRUE, widths = NULL,
                       heights = NULL, height = NULL, width = NULL,
                       padding = NULL,
                       track.plot.color = NULL,
                       track.bg.color = NULL,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.angle = 90,
                       label.text.cex = 1,
                       label.width = unit(2.5, "lines"),
                       main.height = unit(1.5, "lines"),
                       scale.height = unit(1, "lines"),
                       xlab.height = unit(1, "lines"),
                       main = NULL,
                       xlab = NULL,
                       remove.y.axis = FALSE,
                       remove.x.axis = FALSE,
                       .scale.grob = NULL
                       ) {

  if (is.numeric(scale.height) && !is.unit(scale.height))
    scale.height <- unit(scale.height, "lines")

  if (is.numeric(main.height) && !is.unit(main.height))
    main.height <- unit(main.height, "lines")

  if (!is.null(height) && is.null(heights))
    heights <- height

  if (!is.null(width) && is.null(widths))
    widths <- width

  ggl <- list(...)

  if (length(ggl)) {
      if (length(ggl) == 1  && !is.ggplot(ggl[[1]]) && is.list(ggl[[1]])) {
        ggl <- ggl[[1]]
      }
  } else {
      return(ggplot())
  }

  label.name <- names(ggl)
  N <- length(ggl)

  if (length(track.plot.color) == 1) {
      track.plot.color <- rep(track.plot.color, N)
  }

  ## add a plot with axis and remove later
  if (vertical) {
    idx.fix <- which(!vapply(ggl, fixed, logical(1L)) & !vapply(PlotList, is, "Ideogram", FUN.VALUE = logical(1L)))[1]
    if (is.na(idx.fix))
        idx.fix <- length(ggl)
    ggl <- c(ggl, list(.scale.grob))
  }

    ## parse grobs
    ## a little slow
    grobs <- do.call(GrobList, ggl)

    if (vertical)
        grobs <- do.call(uniformAroundPanel, grobs)
    else
        grobs <- do.call(uniformAroundPanel, c(grobs,list(direction = "col")))

    .nms <- names(grobs)

    ## change background color
    grobs <- lapply(seq_len(length(grobs)), function(i) {
        ## better figure out a better idea
        .grob <- grobs[[i]]
        .col <- track.plot.color[i]
        gt.temp <- grobs[[i]]$grobs[[1]]$children[[1]]$children$layout
        ## edit background
        gt.temp$grobs[[1]] <- editGrob(gt.temp$grobs[[1]], gp = gpar(alpha = 0))
        idx <- which(gtable_filter_grobs(gt.temp, "guide-box"))
        if (length(idx) == 1L) {
            if (gtable_filter_grobs(gt.temp$grobs[[idx]]$grobs[[1]], "background")[1L]) {
                gt.temp$grobs[[idx]]$grobs[[1]]$grobs[[1]] <-
                    editGrob(gt.temp$grobs[[idx]]$grobs[[1]]$grobs[[1]],
                             gp = gpar(alpha = 0))
            }
        }
        grobs[[i]]$grobs[[1]]$children[[1]]$children$layout <- gt.temp
        grobs[[i]]$grobs[[1]] <- editGrob(grobs[[i]]$grobs[[1]], "bgColor",
                                          grep = TRUE, global = TRUE,
                                          gp  = gpar(fill = .col, col = .col))
        grobs[[i]]
    })

    names(grobs) <- .nms

    if (vertical) {
        g.last <- grobs[[length(grobs)]]
        grobs <- grobs[-length(grobs)]
        g <- g.last$grobs[[1]]$children[[1]]$children$layout
        g.s <- scaleGrob(g)
        if (length(track.bg.color)) {
            rect.grob <- rectGrob(gp=gpar(col = track.bg.color, fill = track.bg.color))
            g.s <- grobTree(gTree(children = gList(rect.grob, g.s)))
        }
        grobs <- c(grobs, list(g.s))
        if (length(main)) {
            text.grob <- textGrob(main)
            if (length(track.bg.color)) {
                rect.grob <- rectGrob(gp=gpar(col = track.bg.color, fill = track.bg.color))
                text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
            }
            grobs <- c(list(text.grob), grobs)
        }

        if (length(xlab)) {
            text.grob <- textGrob(xlab)
            if (length(track.bg.color)) {
                rect.grob <- rectGrob(gp=gpar(col = track.bg.color, fill = track.bg.color))
                text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
            }
            grobs <- c(grobs, list(text.grob))
        }
    }

    if (any(remove.y.axis)) {
        for (i in which(remove.y.axis))
            grobs[[i]] <- removeYAxis(grobs[[i]])
    }

    if (any(remove.x.axis)) {
        for (i in which(remove.x.axis))
            grobs[[i]] <- removeXAxis(grobs[[i]])
    }

    ## FIXME:
    lbs <- vapply(grobs, labeled, logical(1L))
    nms <- names(lbs)

    label_args <- list(grobs, nms, lbs,
                       label.bg.color =  label.bg.color,
                       label.bg.fill = label.bg.fill,
                       label.text.color = label.text.color,
                       label.text.cex = label.text.cex,
                       label.text.angle = label.text.angle,
                       label.width = label.width)

    if (!vertical)
        label_args <- c(label_args, list(direction = "col"))

    if (any(!is.null(nms)))
        grobs <- do.call(addLabel, label_args)

    ## reduce to normal grob
    grobs_back <- grobs
    grobs <- lapply(grobs, function(g) {
        if (is(g, "Grob")) {
            suppressWarnings(class(g) <- g@.S3Class)
            return(g)
        } else {
            return(g)
        }
    })

    if (vertical) {
        if (!length(widths)) {
            widths <- unit(1, "null")
        } else if (is.numeric(widths) && !is.unit(widths)) {
            widths <- unit(widths, "null")
        } else if (!is.unit(widths)) {
            stop("widths must be unit or numeric value")
        }
        if (!length(heights)) {
            heights <- unit(rep(1, N), "null")
        } else if (is.numeric(heights) && !is.unit(heights)) {
            heights <- unit(heights, "null")
        } else if (!is.unit(heights)) {
            stop("heights must be unit or numeric value")
        }
        ## TODO check main later
        if (length(main))
            heights <- unit.c(main.height, heights)
        if (vertical)
            heights <- unit.c(heights, scale.height)
        if (length(xlab))
            heights <- unit.c(heights, xlab.height)
            tab <- gtable(widths, heights)
            for (i in 1:length(grobs)) {
            tab <- gtable_add_grob(tab, grobs[[i]], t = i, r = 1, l  = 1)
        }
        if (length(track.bg.color)) {
            rect.grob <- rectGrob(gp=gpar(col = track.bg.color,
                                  fill = track.bg.color))
            tab <- grobTree(gTree(children = gList(rect.grob, tab)))
        }
    } else {
        if (!length(widths)) {
            widths <- unit(rep(1, N), "null")
        } else if (is.numeric(widths) && !is.unit(widths)) {
            widths <- unit(widths, "null")
        } else if (!is.unit(width)) {
            stop("widths must be unit or numeric value")
        }

        if (!length(heights)) {
            heights <- unit(1, "null")
        } else if (is.numeric(heights) && !is.unit(heights)) {
            heights <- unit(heights, "null")
        } else if (!is.unit(heights)) {
            stop("heights must be unit or numeric value")
        }
        tab <- gtable(widths, heights)
        for (i in 1:N) {
            tab <- gtable_add_grob(tab, grobs[[i]], l = i, t = 1, b = 1)
        }
    }
    tab
}

spaceAroundPanel <- function(gtable, types = c("t", "l", "b", "r")) {
    spaces <- list()
    # get panel positions for gtable$layout
    panel_position <- gtable_filter_grobs(gtable, "panel")

    for (type in types) {
        all_type_values <- gtable$layout[[type]]

        if (type %in% c("b", "r")) {
            # Find the maximum panel value and compare it to other values
            # of gtable$layout[['type']]. It provides values that are
            # outside of the maximum panel value.
            max_panel_value <- max(gtable$layout[panel_position, ][[type]])
            position <- which(all_type_values > max_panel_value)
        } else if (type %in% c("t", "l")) {
            # why minimum ?
            min_panel_value <- min(gtable$layout[panel_position, ][[type]])
            position <- which(all_type_values < min_panel_value)
        }

        # To remove duplicate position values
        unique_position <- unique(gtable$layout[[type]][position])

        # get heights and width associated with unique_position
        # from the gtable
        if (length(position) && type %in% c("t", "b"))
            res <- sum(gtable$height[unique_position])
        else if (length(position) && type %in% c("l", "r"))
            res <- gtable$width[unique_position]
        else
            res <- unit(0, "inches")

        spaces[[type]] <- res
    }
    spaces
}

## return uniformed grobs
uniformAroundPanel <- function(..., direction = c("row", "col")) {
    dir <- match.arg(direction)
    args <- list(...)

    if (length(args) == 1 && is(args[[1]], "GrobList"))
        grobs <- args[[1]]
    else
        grobs <- lapply(args, function(p) Grob(p))

    get_uniform_grobs <- function(spaces, p1, max1, p2, max2, FUN) {
        for (i in 1:length(grobs)) {
            gt <- gtable(unit(1, "null"), unit(1, "null"), name = "panel.ori")
            rect.grob <- rectGrob(gp = gpar(fill = NA, color = NA), name = "bgColor")
            grobs[[i]] <- FUN(grobs[[i]], max1 - spaces[[i]][[p1]], pos = 0)
            grobs[[i]] <- FUN(grobs[[i]], max2 - spaces[[i]][[p2]], pos = -1)
            all.grob <- grobTree(gTree(children = gList(rect.grob, grobs[[i]])))
            grobs[[i]] <- gtable_add_grob(gt, all.grob, 1, 1)
          }
        grobs
    }

    if (dir == "row") {
        spaces <- lapply(grobs, spaceAroundPanel, c("l", "r"))
        low <- unit(-0.35, "cm")
        for (x in spaces) {
            if (length(x$r) > 3L && as.numeric(x$r[4L]) < as.numeric(low))
                low <- x$r[4L]
        }
        spaces <- lapply(spaces, function(x) {
            if (length(x$r) < 4L)
                x$r <- unit.c(x$r, low)
            x$l <- sum(x$l)
            x$r <- sum(x$r)
            x
        })
        lmax <- do.call(max, lapply(spaces, function(x) x$l))
        rmax <- do.call(max, lapply(spaces, function(x) x$r))
        grobs <- get_uniform_grobs(spaces, "l", lmax, "r", rmax, gtable_add_cols)
    } else if (dir == "col") {
      spaces <- lapply(grobs, spaceAroundPanel, c("t", "b"))
      tmax <- do.call(max, lapply(spaces, function(x) x$t))
      bmax <- do.call(max, lapply(spaces, function(x) x$b))
      grobs <- get_uniform_grobs(spaces, "t", tmax, "b", bmax, gtable_add_rows)
    }
    grobs
}

align.plots <- alignPlots

scaleGrob <- function(gtable){
    idx <- gtable_filter_grobs(gtable, "axis-b")
    idx <- unique(c(gtable$layout[idx, "t"], gtable$layout[idx, "b"]))
    res <- gtable[idx,]
    res
}

removeAxis <- function(g, remove, p1, p2) {
    if (g$name == "panel.ori") {
        gr <- g$grobs[[1]]$children[[1]]$children$layout
        gr <- removeAxis(gr, remove, p1, p2)
    } else {
        idx <- gtable_filter_grobs(g, remove)
        idx <- sort(unique(c(g$layout[[p1]][idx], g$layout[[p2]][idx])))
        idx <- setdiff(seq_len(ncol(g)), idx)
        g <- g[,idx]
    }
    g
}

removeXAxis <- function(gtable) {
    remove <- c("xlab", "axis-b", "title")
    removeAxis(gtable, remove, "t", "b")
}

removeYAxis <- function(gtable) {
    remove <- c("ylab", "axis-l")
    removeAxis(gtable, remove, "l", "r")
}

getHeight <- function(dts) {
    hts <- do.call(unit.c, lapply(dts, height))
    hts
}

parseHeight <- function(hts, n) {
    if (length(hts) != n && length(hts) != 1)
        stop("Heights must be of length 1 or numbers of graphics")
    if (is.numeric(hts) && !is.unit(hts)) {
        if (length(hts) == 1)
            res <- rep(unit(1, "null"), n)
        if (length(hts) == n)
            res <- unit(hts, "null")
    } else if (is.unit(hts)) {
        res <- hts
    }
    res
}

## combining
## do something fun here, make combination method for Tracks
## support
## 1. c(Tracks, Tracks)
## 2. Tracks + Tracks
## 3. Tracks(Tracks, Tracks)
## 4. Tracks + plot (not yet)
setMethod("+", signature = c("Tracks", "Tracks"), function(e1, e2) {
    e1 <- c(e1, e2)
    e1
})

setMethod("c", "Tracks",  function(x, ...) {
    if (missing(x)) {
        args <- unname(list(...))
        x <- args[[1L]]
    } else {
        args <- unname(list(x, ...))
    }

    if (length(args) == 1L)
        return(x)

    arg_is_null <- vapply(args, is.null, FUN.VALUE = logical(1L))
    isClassValid <- vapply(args, is, class(x), FUN.VALUE = logical(1L))

    if (any(arg_is_null))
        args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
    if (!all(isClassValid))
        stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")

    lst <- lapply(args, function(x) {
        x@grobs
    })
    ## FIXME: how to keep other attributes?
    res <- do.call(tracks, do.call(c, lst))
    res
})

setMethod("cbind", "Tracks",  function(...) {
    args <- list(...)
    isTrack <- vapply(args, is, "Tracks", FUN.VALUE = logical(1L))
    if (all(isTrack)) {
        lst <- lapply(args, as, "grob")
        res <- do.call(cbind, lst)
    } else {
        stop("need to be of class Tracks")
    }
    grid.draw(res)
})

setMethod("rbind", "Tracks",  function(...) {
    args <- list(...)
    isTrack <- vapply(args, is, "Tracks", FUN.VALUE = logical(1L))
    if (all(isTrack)) {
        lst <- lapply(args, as, "grob")
        res <- do.call(rbind, lst)
    } else {
        stop("need to be of class Tracks")
    }
    grid.draw(res)
})

setMethod("[", c("Tracks", "numeric", "missing", "ANY"),
          function(x, i, j, ..., drop=TRUE) {
              i <- as.integer(i)
              initialize(x,
                  grobs = x@grobs[i],
                  plot = x@plot[i],
                  labeled = x@labeled[i],
                  heights = x@heights[i],
                  xlim = x@xlim,
                  ylim = x@ylim,
                  xlab = x@xlab,
                  main = x@main,
                  main.height = x@main.height,
                  scale.height = x@scale.height,
                  xlab.height = x@xlab.height,
                  theme = x@theme,
                  mutable = x@mutable[i],
                  hasAxis = x@hasAxis[i],
                  fixed = x@fixed[i],
                  padding = x@padding,
                  label.bg.color = x@label.bg.color[i],
                  label.bg.fill = x@label.bg.fill[i],
                  label.text.color = x@label.text.color[i],
                  label.text.angle = x@label.text.angle[i],
                  track.plot.color = x@track.plot.color[i],
                  track.bg.color = x@track.bg.color[i],
                  label.text.cex = x@label.text.cex[i],
                  label.width = x@label.width)
          })

ggbioGrob <- function(x) {
    if (is(x, "GGbio"))
        ggplot2::ggplotGrob(x@ggplot)
    else
        ggplot2::ggplotGrob(x)
}

getLegendGrob <- function(plot) {
    gtable <- ggbioGrob(plot)
    gtable <- gtable_filter(gtable, "guide-box")
}

arrangeGrobByParsingLegend <- function(..., nrow = NULL, ncol = NULL,
                                       widths = c(4, 1), legend.idx = NULL) {
    lst <- list(...)
    if (length(lst) == 1 && is.list(lst[[1]]))
        lst <- lst[[1]]

    legends <- lapply(lst, getLegendGrob)

    plots <- lapply(lst, function(x) {
        x <- x + theme(legend.position = "none", aspect.ratio = 1)
        ggbioGrob(x)
    })

    if (!is.null(legend.idx))
        legends <- legends[legend.idx]
    legends <- do.call(gridExtra::arrangeGrob, c(legends, list(ncol = 1)))
    plots <- do.call(gridExtra::arrangeGrob, c(plots, list(nrow = nrow, ncol = ncol)))
    print(gridExtra::grid.arrange(plots, legends, ncol = 2, widths = widths))
}
