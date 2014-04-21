tracks.gen <- setClass("Tracks",
                       representation(grobs = "PlotList", # working plots, not reall 'Grob'
                                      plot = "list", # original plots passed into tracks
                                      backup = "list", # backup of the whole tracks object
                                      heights = "numericORunit",
                                      xlim = "numeric",
                                      ylim = "list",
                                      xlab = "characterORNULL",
                                      main = "characterORNULL",
                                      main.height =  "numericORunit",
                                      scale.height =  "numericORunit",
                                      xlab.height =  "numericORunit",
                                      theme = "themeORNULL",
                                      fixed = "logical",
                                      labeled = "logical",
                                      mutable = "logical",
                                      hasAxis = "logical",
                                      padding = "numericORunit",
                                      label.bg.color = "character",
                                      label.bg.fill = "character",
                                      label.text.color = "character",
                                      label.text.cex = "numeric",
                                      track.plot.color = "characterORNULL",
                                      track.bg.color = "characterORNULL",
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
                   label.width = unit(2.5, "lines")){




  if(is.numeric(padding) && !is.unit(padding))
    padding <- unit(padding, "lines")


  if(is.numeric(main.height) && !is.unit(main.height))
    main.height <- unit(main.height, "lines")

  if(is.numeric(scale.height) && !is.unit(scale.height))
    scale.height <- unit(scale.height, "lines")

  if(is.numeric(xlab.height) && !is.unit(xlab.height))
    xlab.height <- unit(xlab.height, "lines")



  if(!is.null(title) && is.null(main))
    main <- title

  dots <- list(...)

  ## reduce plots
  dots <- reduceListOfPlots(dots)



  ## return plots if not
  dots <- genPlots(dots)


  ## original plots
  ppl.ori <- do.call(plotList, dots)

  ## ## Make sure Tracks are combined later
  ## isTracks <- lapply(dots, is, "Tracks")
  ## dots <- dots[!isTracks]
  ## tks.addon <- dots[isTracks]

  ## convert to Plot object with extra slots
  dots <- do.call(PlotList, dots)

  ## fixed
  fixed <- sapply(dots, fixed)

  ## mutable
  mts <- sapply(dots, mutable)

  ## hasAxis
  axs <- sapply(dots, hasAxis)

  ## get height
  if(missing(heights)){
    heights <- getHeight(dots)
  }else{
    heights <- parseHeight(heights, length(dots))
  }

  ## labeld
  labeled <- sapply(dots, labeled)

  ## Ideo
  isIdeo <- sapply(dots, is, "Ideogram")

  ## is blank
  isBlank <- sapply(dots, function(x) x@blank)


#   if(length(label.text.color) == 1)
#       label.text.color <- rep(label.text.color, len = length(dots))
#   if(length(label.text.cex) == 1)
#       label.text.cex <- rep(label.text.cex, len = length(dots))
#   if(length(label.bg.color) == 1)
#       label.bg.color <- rep(label.bg.color, len = length(dots))
#   if(length(label.bg.fill) == 1)
#       label.bg.fill <- rep(label.bg.fill, len = length(dots))


  ## ylim
  ylim <- lapply(dots[!fixed & !isIdeo & !isBlank], function(grob){
     scales::expand_range(getLimits(grob)$ylim, mul = 0.05)
  })

  wh <- NULL
  ## xlim
  if(missing(xlim)){
      idx <- sapply(list(...), function(x){is(x, "GRanges")})
      if(any(idx)){
          grs <- list(...)[idx]
          grs <- do.call(c, grs)
          chrs <- unique(as.character(seqnames(grs)))
          if(length(chrs) > 1){
              stop("seqnames of passed GRanges has to be the same for tracks")
          }
          ir <- reduce(ranges(grs))
          wh <- GRanges(chrs, ir)
      }
      xid <- !fixed & !isIdeo & !isBlank
      if(sum(xid)){
          lst <- lapply(dots[xid], function(obj){
              res <- getLimits(obj)
              data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
          })
          res <- do.call(rbind, lst)
          xlim <- c(min(res$xmin), max(res$xmax))
          xlim <- scales::expand_range(xlim, mul = 0.1)
      }else{
          xlim <- c(0, 1)
      }
  }else{
    if(is(xlim, "IRanges")){
      xlim <- c(start(xlim), end(xlim))
    }
    if(is(xlim,"GRanges")){
      wh <- xlim
      xlim <- c(start(ranges(reduce(xlim, ignore.strand = TRUE))),
                end(ranges(reduce(xlim, ignore.strand = TRUE))))
    }
    if(is.numeric(xlim)){
      xlim <- range(xlim)
    }
  }

  ## sync xlim when construct them??
  if(!is.null(wh)){
    dots <- lapply(dots, function(x){
        x + xlim(wh)
    })
    dots <- do.call(PlotList, dots)
  }

  ## track bround
  ##   if(is.null(track.bg.color))
  ##     track.plot.color <- sapply(dots, bgColor)
  ##   else
  ##     track.plot.color <- rep(track.bg.color, length(dots))
  ## stopifnot(length(track.bg.color) == N | length(track.bg.color) == 1)

  ## plot background
  N <- length(dots)
  if(is.null(track.plot.color)){
    if(is.null(track.bg.color))
      track.plot.color <- sapply(dots, bgColor)
    else
      track.plot.color <- rep(track.bg.color, length(dots))
  }
  stopifnot(length(track.plot.color) == N | length(track.plot.color) == 1)


  ## backup: record a state
  backup <- list(grobs = dots,
                 plot = ppl.ori,
                 heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab,
                 main = main,
                 main.height = main.height,
                 scale.height = scale.height,
                 xlab.height = xlab.height,
                 theme = theme, mutable = mts, hasAxis = axs,
                 fixed = fixed, padding = padding,
                 labeled = labeled, label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
                 label.text.color = label.text.color,
                 track.plot.color = track.plot.color,
                 track.bg.color = track.bg.color,
                 label.text.cex = label.text.cex,
                 label.width = label.width)

  obj <- new("Tracks", grobs = dots, plot = ppl.ori, backup = backup, labeled = labeled,
      heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, main = main,
      main.height = main.height,
      scale.height = scale.height,
      xlab.height = xlab.height,
      theme = theme,  mutable = mts, hasAxis = axs,
      fixed = fixed, padding = padding,
      label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
      label.text.color = label.text.color,
      track.plot.color = track.plot.color,
      track.bg.color = track.bg.color,
      label.text.cex = label.text.cex,
      label.width = label.width)
  ## obj <- c(obj, tks.addon)
  ggplot2:::set_last_plot(obj)
  obj
}


setMethod("summary", "Tracks", function(object){
  cat("-------------------------------------------\n")
  cat("Tracks contains: ", length(object@grobs), " graphic objects\n")
  cat("-------------------------------------------\n")
  cat("xlim:", object@xlim, "\n")
  cat("heights", object@heights, "\n")
  cat("fixed", object@fixed, "\n")
  cat("track.plot.color", object@track.plot.color, "\n")
  cat("-------------------------------------------\n")
})


setMethod("print", "Tracks", function(x){
    grobs <- x@grobs
    N <- length(grobs)
    if(any(x@labeled))
      nms <- names(x@grobs)
    else
      nms <- NULL
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(i %in% which(x@mutable))
                      grobs[[i]] <- grobs[[i]] + x@theme
                    grobs[[i]] <- grobs[[i]] + ggplot2::xlab("")  + labs(title = "")
#                     if(i == 1 && !is.null(x@main)){
#                       grobs[[i]] <- grobs[[i]] + theme(plot.margin = unit(c(1, 1,
#                               as.numeric(x@padding),  0.5), "lines"))
#                     }else{
                     grobs[[i]] <- grobs[[i]] + theme(plot.margin = unit(c(as.numeric(x@padding), 1,
                              as.numeric(x@padding),  0.5), "lines"))
                    #}
                    if(i %in% which(!x@hasAxis))
                      grobs[[i]] <- grobs[[i]] + theme(axis.text.x = element_blank(),
                                                       axis.ticks.x = element_blank())

                    if(i %in% which(!x@fixed)){
                      s <- coord_cartesian(xlim = x@xlim)
                      grobs[[i]] <- grobs[[i]] + s
                    }
                    grobs[[i]]
                  })

    if(!is.null(nms))
      names(lst) <- nms
    grid.newpage()
    if(any(x@labeled))
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             padding = x@padding,
                                             label.bg.color =  x@label.bg.color,
                                             label.bg.fill = x@label.bg.fill,
                                             label.text.color = x@label.text.color,
                                             label.text.cex = x@label.text.cex,
                                             label.width = x@label.width,
                                             track.plot.color = x@track.plot.color,
                                             track.bg.color = x@track.bg.color,
                                             main = x@main,
                                             xlab = x@xlab,
                                             main.height = x@main.height,
                                             scale.height = x@scale.height,
                                             xlab.height = x@xlab.height
                                             )))
    else
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             padding = x@padding,
                                             track.plot.color = x@track.plot.color,
                                             track.bg.color = x@track.bg.color,
                                             main = x@main,
                                             xlab = x@xlab,
                                             main.height = x@main.height,
                                             scale.height = x@scale.height,
                                             xlab.height = x@xlab.height
                                             )))

    grid.draw(res)
    ggplot2:::set_last_plot(x)
})

setGeneric("get_gtable", function(x, ...) standardGeneric("get_gtable"))

setMethod("get_gtable", "Tracks", function(x){
    grobs <- x@grobs
    N <- length(grobs)
    if(any(x@labeled))
      nms <- names(x@grobs)
    else
      nms <- NULL
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(i %in% which(x@mutable))
                      grobs[[i]] <- grobs[[i]] + x@theme
                    grobs[[i]] <- grobs[[i]] + ggplot2::xlab("")  + labs(title = "") +
                      theme(plot.margin = unit(c(as.numeric(x@padding), 1,
                              as.numeric(x@padding),  0.5), "lines"))
                    if(i %in% which(!x@hasAxis))
                      grobs[[i]] <- grobs[[i]] + theme(axis.text.x = element_blank(),
                                                       axis.ticks.x = element_blank())

                    if(i %in% which(!x@fixed)){
                      s <- coord_cartesian(xlim = x@xlim)
                      grobs[[i]] <- grobs[[i]] + s
                    }
                    grobs[[i]]
                  })

    if(!is.null(nms))
      names(lst) <- nms
    if(any(x@labeled))
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             padding = x@padding,
                                             label.bg.color =  x@label.bg.color,
                                             label.bg.fill = x@label.bg.fill,
                                             label.text.color = x@label.text.color,
                                             label.text.cex = x@label.text.cex,
                                             label.width = x@label.width,
                                             track.plot.color = x@track.plot.color,
                                             track.bg.color = x@track.bg.color,
                                             main = x@main,
                                             xlab = x@xlab,
                                             main.height = x@main.height,
                                             scale.height = x@scale.height,
                                             xlab.height = x@xlab.height
                                             )))
    else
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             padding = x@padding,
                                             track.plot.color = x@track.plot.color,
                                             track.bg.color = x@track.bg.color,
                                             main = x@main,
                                             xlab = x@xlab,
                                             main.height = x@main.height,
                                             scale.height = x@scale.height,
                                             xlab.height = x@xlab.height
                                             )))

    res
})

setMethod("show", "Tracks", function(object){
  print(object)
  ggplot2:::set_last_plot(object)
})


setMethod("Arith", signature = c("Tracks", "ANY"), function(e1, e2) {
     switch(.Generic,
            "+"= {
              N <- length(e1@grobs)
              ## get attributes
              .theme <- intersect(names(attributes(e2)),  .tracks.theme)
              idx <- sapply(e1@grobs, mutable)
              for(i in (1:N)[idx]){
                  e1@grobs[[i]] <- e1@grobs[[i]] + e2
                }
              if(length(.theme)){
              for(z in seq_len(length(.theme))){
                slot(e1, .theme[z]) <- attr(e2, .theme[z])
              }}
            },
            stop("unhandled 'Arith' operator '", .Generic, "'"))
    e1
})

setMethod("Arith", signature = c("Tracks", "theme"), function(e1, e2) {
     switch(.Generic,
            "+"= {
              N <- length(e1@grobs)
              ## get attributes
              .theme <- intersect(names(attributes(e2)),  .tracks.theme)
              idx <- sapply(e1@grobs, mutable)
              for(i in (1:N)[idx]){
                  e1@grobs[[i]] <- e1@grobs[[i]] + e2
                }
              if(length(.theme)){
              for(z in seq_len(length(.theme))){
                slot(e1, .theme[z]) <- attr(e2, .theme[z])
              }}
              e1@theme <- e2
            },
            stop("unhandled 'Arith' operator '", .Generic, "'"))
    e1
})

setOldClass("zoom")
setMethod("Arith", signature = c("Tracks", "zoom"), function(e1, e2) {

  xlim <- e1@xlim

e1@xlim <- .zoom(xlim, as.numeric(e2))$limits$x
  N <- length(e1@grobs)
  for(i in 1:N){
   e1@grobs[[i]] <- e1@grobs[[i]] + e2
  }
  e1
})

setOldClass("position_c")
setMethod("Arith", signature = c("Tracks", "position_c"), function(e1, e2) {
  if("x" %in% e2$aesthetics){
    if(!is.null(e2$limits))
        e1@xlim <- e2$limits
  }
  N <- length(e1@grobs)
  for(i in 1:N){
      e1@grobs[[i]] <- e1@grobs[[i]] + e2
  }
  e1
})


setOldClass("cartesian")
setMethod("Arith", signature = c("Tracks", "cartesian"), function(e1, e2) {
  if(!is.null(e2$limits$x))
    e1@xlim <- e2$limits$x
  if(!is.null(e2$limits$y)){
    for(i in seq_len(length(e1@ylim))){
      if(!fixed(e1@grobs[[i]]) && !is(e1@grobs[[i]], "Ideogram"))
        e1@ylim[[i]] <- e2$limits$y
    }
  }
  N <- length(e1@grobs)
  for(i in 1:N){
    if(!fixed(e1@grobs[[i]]))
      e1@grobs[[i]] <- e1@grobs[[i]] + e2
  }
  e1
})


xlim_car <- function(x){
  class(x) <- c(class(x), "xlim")
  x
}

setMethod("xlim", "numeric", function(obj, ...){
  if(length(list(...)))
    obj <- c(obj, ...)
  if(length(obj) > 2){
    obj <- range(obj)
  }
  res <- ggplot2::coord_cartesian(xlim = obj)
  xlim_car(res)
})


setMethod("xlim", "IRanges", function(obj, ...){
    xlim <- c(start(obj), end(obj))
    res <- ggplot2::coord_cartesian(xlim = xlim)
    xlim_car(res)
})

setMethod("xlim", "GRanges", function(obj, ...){
  xlim <- c(start(ranges(reduce(obj, ignore.strand = TRUE))),
            end(ranges(reduce(obj, ignore.strand = TRUE))))
  res <- ggplot2::coord_cartesian(xlim = xlim)
  chr <- unique(as.character(seqnames(obj)))
  attr(res, "chr") <- chr
  attr(res, "ori") <- obj
  xlim_car(res)
})

setMethod("xlim", "Tracks", function(obj, ...){
  obj@xlim
})



setReplaceMethod("xlim", c("Tracks", "IRanges"), function(x, value){
    xlim <- c(start(value), end(value))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i){
      ylim <- x@ylim[[i]]
      s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
      if(i %in% which(!x@fixed))
        x@grobs[[i]] <- x@grobs[[i]] + s
    })
    x
})

setReplaceMethod("xlim", c("Tracks", "GRanges"), function(x, value){
  xlim <- c(start(ranges(reduce(value, ignore.strand = TRUE))),
            end(ranges(reduce(value, ignore.strand = TRUE))))
  x@xlim <- xlim
  lapply(1:length(x@grobs), function(i){
    ylim <- x@ylim[[i]]
    s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
    if(i %in% which(!x@fixed))
      x@grobs[[i]] <- x@grobs[[i]] + s
  })
  x
})

setReplaceMethod("xlim", c("Tracks", "numeric"), function(x, value){
  xlim <- range(value)
  x@xlim <- xlim
  lapply(1:length(x@grobs), function(i){
    ylim <- x@ylim[[i]]
    s <- coord_cartesian(xlim = x@xlim, ylim = ylim)
    if(i %in% which(!x@fixed))
      x@grobs[[i]] <- x@grobs[[i]] + s
  })
  x
})

## setMethod("update", "Tracks", function(object, xlim){
##   xlim(object) <- xlim
##   object@xlim <- xlim
##   print(object)
## })

setGeneric("reset", function(obj, ...) standardGeneric("reset"))
setMethod("reset", "Tracks", function(obj){
  nms <- setdiff(slotNames(obj), "backup")
  for(nm in nms){
    slot(obj, nm) <- obj@backup[[nm]]
  }
  xlim(obj) <- obj@xlim
  obj
})

setGeneric("backup", function(obj, ...) standardGeneric("backup"))
setMethod("backup", "Tracks", function(obj){
  nms <- setdiff(slotNames(obj), "backup")
  for(nm in nms){
    obj@backup[[nm]] <- slot(obj, nm)
  }
  obj
})


## TODO: adust due to left/right legend
alignPlots <- function(..., vertical = TRUE, widths = NULL,
                       heights = NULL, height = NULL, width = NULL,
                       padding = NULL,
                       track.plot.color = NULL,
                       track.bg.color = NULL,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,
                       label.width = unit(2.5, "lines"),
                       main.height = unit(1.5, "lines"),
                       scale.height = unit(1, "lines"),
                       xlab.height = unit(1, "lines"),
                       main = NULL,
                       xlab = NULL,
                       remove.y.axis = FALSE,
                       remove.x.axis = FALSE
                       ){

  if(is.numeric(scale.height) && !is.unit(scale.height))
    scale.height <- unit(scale.height, "lines")

  if(is.numeric(main.height) && !is.unit(main.height))
    main.height <- unit(main.height, "lines")

  ## check
  if(!is.null(height) && is.null(heights))
    heights <- height

  if(!is.null(width) && is.null(widths))
    widths <- width

  ggl <- list(...)

  if(length(ggl)){
    if(length(ggl) == 1  && !is.ggplot(ggl[[1]]) && is.list(ggl[[1]])){
      ggl <- ggl[[1]]
    }}else{
      return(ggplot())
    }
  label.name <- names(ggl)
  N <- length(ggl)

  if(length(track.plot.color) == 1){
      track.plot.color <- rep(track.plot.color, N)
  }

  ## add a plot with axis and remove later
  if(TRUE){
    idx.fix <- which(!sapply(ggl, fixed) & !sapply(ggl, is, "Ideogram"))[1]
    if(is.na(idx.fix))
      idx.fix <- 1
    ggl <- c(ggl, list(ggl[[idx.fix]] + theme_gray()))
  }

  ## parse grobs
  ## a little slow
  grobs <- do.call(GrobList, ggl)

  addLabel <- function(grobs, nms, lbs,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,
                       label.width = unit(2.5, "lines"),
                       direction = c("row", "col")
                       ){

    if(length(label.text.color) == 1)
        label.text.color <- rep(label.text.color, len = length(grobs))
    if(length(label.text.cex) == 1)
        label.text.cex <- rep(label.text.cex, len = length(grobs))
    if(length(label.bg.color) == 1)
        label.bg.color <- rep(label.bg.color, len = length(grobs))
    if(length(label.bg.fill) == 1)
        label.bg.fill <- rep(label.bg.fill, len = length(grobs))

    direction <- match.arg(direction)

    if(direction == "row"){
      res <- lapply(1:length(grobs), function(i){
        grob <- grobs[[i]]
        if(lbs[i]){
          rect <- rectGrob(gp = gpar(fill = label.bg.fill[i],
                             col = label.bg.color[i]))
          label <- textGrob(nms[i], rot = 90,
                            gp = gpar(col = label.text.color[i],
                              cex = label.text.cex[i]))
          left.grob <- grobTree(gTree(children = gList(rect, label)))
        }else{
          left.grob <- ggplot2:::zeroGrob()
        }
        gt <- gtable(widths = unit.c(label.width,unit(1, "null")),
                     heights = unit(1, "null"))
        gt <- gtable_add_grob(gt, left.grob,l = 1, t = 1)
        gt <- gtable_add_grob(gt, grob, l = 2, t =1 )
      })
    }else{
      res <- lapply(1:length(grobs), function(i){
        if(lbs[i]){
          grob <- grobs[[i]]
          rect <- rectGrob(gp = gpar(fill = label.bg.fill[i],
                             col = label.bg.color[i]))
          label <- textGrob(nms[i],
                            gp = gpar(col = label.text.color[i],
                              cex = label.text.cex[i]))
          top.grob <- grobTree(gTree(children = gList(rect, label)))
        }else{
          top.grob <- ggplot2:::zeroGrob()
        }
        gt <- gtable(widths = unit(1, "null"),
                     heights = unit.c(label.width,unit(1, "null")))
        gt <- gtable_add_grob(gt, top.grob,l = 1, t = 1)
        gt <- gtable_add_grob(gt, grob, l = 1, t =2 )
      })
    }
  }

  if(vertical)
    grobs <- do.call(uniformAroundPanel, grobs)
  else
    grobs <- do.call(uniformAroundPanel, c(grobs,list(direction = "col")))

  .nms <- names(grobs)

  ## change background color
  grobs <- lapply(1:length(grobs), function(i){
    ## better figure out a better idea
    .grob <- grobs[[i]]
    .col <- track.plot.color[i]
    gt.temp <- grobs[[i]]$grobs[[1]]$children[[1]]$children$layout
    ## edit background
    gt.temp$grobs[[1]] <- editGrob(gt.temp$grobs[[1]],
                                   gp = gpar(alpha = 0))
    if("guide-box" %in% gt.temp$layout$name){
      idx <- which("guide-box" == gt.temp$layout$name)
      if(gt.temp$grobs[[idx]]$grobs[[1]]$layout$name[1] == "background"){
        gt.temp$grobs[[idx]]$grobs[[1]]$grobs[[1]] <- editGrob(gt.temp$grobs[[idx]]$grobs[[1]]$grobs[[1]], gp = gpar(alpha = 0))
      }
    }
    grobs[[i]]$grobs[[1]]$children[[1]]$children$layout <- gt.temp
    grobs[[i]]$grobs[[1]] <- editGrob(grobs[[i]]$grobs[[1]], "bgColor", grep = TRUE, global = TRUE,
                       gp  = gpar(fill = .col, col = .col))
    grobs[[i]]
  })

  names(grobs) <- .nms

  if(TRUE){
    g.last <- grobs[[length(grobs)]]
    grobs <- grobs[-length(grobs)]
    g <- g.last$grobs[[1]]$children[[1]]$children$layout
    g.s <- scaleGrob(g)
    if(length(track.bg.color)){
        rect.grob <- rectGrob(gp=gpar(col = track.bg.color,
                                fill = track.bg.color))
        g.s <- grobTree(gTree(children = gList(rect.grob, g.s)))
      }
    grobs <- c(grobs, list(g.s))
    if(length(main)){
      text.grob <- textGrob(main)
      if(length(track.bg.color)){
        rect.grob <- rectGrob(gp=gpar(col = track.bg.color, fill = track.bg.color))
        text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
      }
    grobs <- c(list(text.grob), grobs)

    }
    if(length(xlab)){
      text.grob <- textGrob(xlab)
      if(length(track.bg.color)){
        rect.grob <- rectGrob(gp=gpar(col = track.bg.color, fill = track.bg.color))
        text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
      }
      grobs <- c(grobs, list(text.grob))

    }
  }



  if(any(remove.y.axis)){
    for(i in which(remove.y.axis))
      grobs[[i]] <- removeYAxis(grobs[[i]])
  }
  if(any(remove.x.axis)){
    for(i in which(remove.x.axis))
      grobs[[i]] <- removeXAxis(grobs[[i]])
  }

  ## nms <- names(grobs)
#   nms <- label.name
  ## FIXME:
  lbs <- sapply(grobs, labeled)
  nms <- names(lbs)
#   print(grid.draw(gTree(grobs[3])))
#   dev.off()
# browser()
#   ## add lables
# length(grobs)
# length(nms)
# nms
# lbs
  if(vertical){
    if(any(!is.null(nms)))
      grobs <- addLabel(grobs, nms, lbs,
                        label.bg.color =  label.bg.color,
                       label.bg.fill = label.bg.fill,
                       label.text.color = label.text.color,
                       label.text.cex = label.text.cex,
                       label.width = label.width)
  }else{
    if(any(!is.null(nms)))
      grobs <- addLabel(lgrobs, nms, lbs,
                        label.bg.color =  label.bg.color,
                       label.bg.fill = label.bg.fill,
                       label.text.color = label.text.color,
                       label.text.cex = label.text.cex,
                       label.width = label.width,
                        direction = "col")
  }

  ## reduce to normal grob
  grobs_back <- grobs
  grobs <- lapply(grobs, function(g){
    if(is(g, "Grob")){
      suppressWarnings(class(g) <- g@.S3Class)
      return(g)
    }else{
      return(g)
    }
  })


  if(vertical){
    if(!length(widths)){
      widths <- unit(1, "null")
    }else if(is.numeric(widths) && !is.unit(widths)){
      widths <- unit(widths, "null")
    }else if(!is.unit(widths)){
      stop("widths must be unit or numeric value")
    }
    if(!length(heights)){
      heights <- unit(rep(1, N), "null")
    }else if(is.numeric(heights) && !is.unit(heights)){
      heights <- unit(heights, "null")
    }else if(!is.unit(heights)){
      stop("heights must be unit or numeric value")
    }
    ## TODO check main later
    if(length(main))
      heights <- unit.c(main.height, heights)
    if(TRUE)
      heights <- unit.c(heights, scale.height)
    if(length(xlab))
      heights <- unit.c(heights, xlab.height)
    tab <- gtable(widths, heights)
    for(i in 1:length(grobs)){
      tab <- gtable_add_grob(tab, grobs[[i]], t = i, r = 1, l  = 1)
    }
    if(length(track.bg.color)){
      rect.grob <- rectGrob(gp=gpar(col = track.bg.color,
                              fill = track.bg.color))
      tab <- grobTree(gTree(children = gList(rect.grob, tab)))
    }

  }else{

    if(!length(widths)){
      widths <- unit(rep(1, N), "null")
    }else if(is.numeric(widths) && !is.unit(widths)){
      widths <- unit(widths, "null")
    }else if(!is.unit(width)){
      stop("widths must be unit or numeric value")
    }

    if(!length(heights)){
      heights <- unit(1, "null")
    }else if(is.numeric(heights) && !is.unit(heights)){
      heights <- unit(heights, "null")
    }else if(!is.unit(heights)){
      stop("heights must be unit or numeric value")
    }
    tab <- gtable(widths, heights)
    for(i in 1:N){
      tab <- gtable_add_grob(tab, grobs[[i]], l = i, t = 1, b = 1)
    }
  }
    tab
}



getPanelIndex <- function(g){
  if(inherits(g, "ggplot"))
    g <- Grob(g)
  idx <- which("panel" == g$layout$name)
  idx
}

spaceAroundPanel <- function(g, type = c("t", "l", "b", "r")){
  if(inherits(g, "ggplot"))
    g <- Grob(g)
  idx <- getPanelIndex(g)
  rsl <- list()
  for(tp in type){
    rsl[[tp]] <- switch(tp,
                t = {
                  id <- which(g$layout$t < min(g$layout[idx, ]$t))
                  ## id <- id[!duplicated(g$layout$name[id])]
                  if(length(id))
                    res <- sum(g$height[unique(g$layout$t[id])])
                  else
                    res <- unit(0, "inches")
                  res
                },
                l = {
                  id <- which(g$layout$l < min(g$layout[idx, ]$l))
                  ## id <- id[!duplicated(g$layout$name[id])]
                  if(length(id))
                    res <- sum(g$width[unique(g$layout$l[id])])
                  else
                    res <- unit(0, "inches")
                  res
                },
                b = {
                  id <- which(g$layout$b > max(g$layout[idx, ]$b))
                  ## id <- id[!duplicated(g$layout$name[id])]
                  if(length(id))
                    res <- sum(g$height[unique(g$layout$b[id])])
                  else
                    res <- unit(0, "inches")
                  res

                },
                r = {
                  id <- which(g$layout$r > max(g$layout[idx, ]$r))
                  ## id <- id[!duplicated(g$layout$name[id])]
                  if(length(id))
                    res <- sum(g$width[unique(g$layout$r[id])])
                  else
                    res <- unit(0, "inches")
                  res
                })

  }
  rsl
}


## return uniformed grobs

uniformAroundPanel <- function(..., direction = c("row", "col")){
  dir <- match.arg(direction)
  lst <- list(...)
  if(length(lst) == 1 && is(lst[[1]], "GrobList")){
    grobs <- lst[[1]]
  }else{
    grobs <- lapply(lst, function(p){
      if(inherits(p, "ggplot"))
        g <- Grob(p)
      else
        g <- p
      g
    })
  }
  if(dir == "row"){
    slst <- lapply(grobs, spaceAroundPanel, c("l", "r"))
    lmx <- do.call(max, lapply(slst, function(lst) lst$l))
    rmx <- do.call(max, lapply(slst, function(lst) lst$r))
    for(i in 1:length(grobs)){
      .grob <- grobs[[i]]
      gt <- gtable(unit(1, "null"), unit(1, "null"), name = "panel.ori")
      grobs[[i]] <- gtable_add_cols(grobs[[i]], lmx - slst[[i]]$l, pos = 0)
      grobs[[i]] <- gtable_add_cols(grobs[[i]], rmx - slst[[i]]$r, pos = -1)
      rect.grob <- rectGrob(gp = gpar(fill = NA, col = NA), name = "bgColor")
      all.grob <- grobTree(gTree(children = gList(rect.grob, grobs[[i]])))
      grobs[[i]] <- gtable_add_grob(gt, all.grob, 1, 1)
    }
  }
  if(dir == "col"){

    slst <- lapply(grobs, spaceAroundPanel, c("t", "b"))
    tmx <- do.call(max, lapply(slst, function(lst) lst$t))
    bmx <- do.call(max, lapply(slst, function(lst) lst$b))
    for(i in 1:length(grobs)){
      .grob <- grobs[[i]]
      gt <- gtable(unit(1, "null"),unit(1, "null"), name = "panel.ori")
      grobs[[i]] <- gtable_add_rows(grobs[[i]], tmx - slst[[i]]$t, pos = 0)
      grobs[[i]] <- gtable_add_rows(grobs[[i]], bmx - slst[[i]]$b, pos = -1)
      rect.grob <- rectGrob(gp = gpar(fill = NA, color = NA), name = "bgColor")
      all.grob <- grobTree(gTree(children = gList(rect.grob, grobs[[i]])))
      grobs[[i]] <- gtable_add_grob(gt, all.grob, 1, 1)
    }
  }
  grobs
}

align.plots <- alignPlots

theme_onlyXaxis <- function(){
  res <- theme_null()
  res <- res + theme(
                 panel.border = element_rect(fill = NA, color = NA),
                 axis.text.x = element_text(vjust = 1),
                 axis.ticks = element_line(colour = "grey50"),
                 axis.ticks.y = element_blank(),
                 axis.title.x = element_text(),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 axis.line = element_line(color = "gray50"))

  list(res,xlab(""))
}

ScalePlot <- function(x, color = "black", fill = NA){
  df <- data.frame(x =x, y = 1)
  p <- qplot(data = df, x = x, y = y, geom = "blank") +
    theme_onlyXaxis() + theme(aspect.ratio = 1/1000) +
      theme(panel.border = element_rect(color = color, fill = fill))
  p <- new("ggplotPlot", p)
  mutable(p) <- FALSE
  hasAxis(p) <- TRUE
  ## height(p) <- unit(4, "lines")
  p
}

## always comes last

scaleGrob <- function(g){
  idx <- g$layout$name %in% c("axis-b")
  idx <- unique(c(g$layout[idx, "t"], g$layout[idx, "b"]))
  res <- g[idx,]
  res
}

## always comes first
titleGrob <- function(g){
  idx <- g$layout$name %in% c("title")
  idx <- unique(c(g$layout[idx, "t"], g$layout[idx, "b"]))
  res <- g[idx,]
  attr(res, "track_name") <- "title"
  res

}




## cannot figure out right range when zoom in/out
ScalePlot2 <- function(xlim, format = scientific_format(),
                      aspect.ratio = 1/15, tick.length = 0.4,
                      text.offset = 0.5,
                      pos = c("bottom", "top", "inter")){

  pos <- match.arg(pos)
  p <- switch(pos,
              bottom = {
                y <- 1
                cbs <- as.data.frame(cbreaks(range(xlim), labels = format))
                idx <- rep(-1, length = nrow(cbs))
                cbs$y <- y
                cbs$y.text <- y + tick.length * idx + text.offset*idx
                cbs$yend <- y + tick.length * idx

                ylim <- scales::expand_range(range(c(cbs$y.text,y,cbs$yend)), mul = 0.3)
                p <- qplot(x = range(cbs$breaks), y = y, geom = "lines") +
                  geom_segment(data = cbs, aes(x = breaks, y = y, yend = yend, xend = breaks)) +
                   geom_text(data = cbs, aes(x = breaks, y = y.text, label = labels), vjust = 0.5,
                             size = 4) + coord_cartesian(ylim = ylim)+
                      theme_null() + theme(aspect.ratio = aspect.ratio)
                p
              },

              top = {
                y <- 1
                cbs <- as.data.frame(cbreaks(range(xlim), labels = format))
                idx <- rep(1, length = nrow(cbs))
                cbs$y <- y
                cbs$y.text <- y + tick.length * idx + text.offset*idx
                cbs$yend <- y + tick.length * idx
                ylim <- scales::expand_range(range(c(cbs$y.text,y,cbs$yend)), mul = 0.3)
                p <- qplot(x = range(cbs$breaks), y = y, geom = "lines") +
                  geom_segment(data = cbs, aes(x = breaks, y = y, yend = yend, xend = breaks)) +
                   geom_text(data = cbs, aes(x = breaks, y = y.text, label = labels), vjust = 0.5,
                             size = 4) + coord_cartesian(ylim = ylim)+
                      theme_null() + theme(aspect.ratio = aspect.ratio)
                p
              },
              inter = {
                y <- 1
                cbs <- as.data.frame(cbreaks(range(xlim), labels = format))
                idx <- rep(c(1, -1), length = nrow(cbs))
                cbs$y <- y
                cbs$y.text <- y + tick.length * idx + text.offset*idx
                cbs$yend <- y + tick.length * idx
                ylim <- scales::expand_range(range(cbs$y.text), mul = 0.3)
                p <- qplot(x = range(cbs$breaks), y = y, geom = "lines") +
                  geom_segment(data = cbs, aes(x = breaks, y = y, yend = yend, xend = breaks)) +
                   geom_text(data = cbs, aes(x = breaks, y = y.text, label = labels), vjust = 0.5,
                             size = 4) + coord_cartesian(ylim = ylim)+
                      theme_null() + theme(aspect.ratio = aspect.ratio)
                p
              })
  p
}


textPlot <- function(lb="", ut = unit(4, "lines")){
  df <- data.frame(x =1:10, y = 1)
  p <- qplot(x = 1, y = 1, geom = "text", label = lb) + theme_null()+
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.margin = unit(c(0, 0, 0, 0), "lines"))
  p <- new("ggplotPlot", p)
  mutable(p) <- FALSE
  fixed(p) <- TRUE
  height(p) <- ut
  p
}

removeXAxis <- function(g){
  if(g$name == "panel.ori"){
    .g <- g$grobs[[1]]$children[[1]]$children$layout
    idx <- which(.g$layout$name %in% c("xlab", "axis-b", "title"))
    idx <- sort(unique(c(.g$layout$t[idx], .g$layout$b[idx])))
    idx <- setdiff(1:nrow(.g), idx)
    g$grobs[[1]]$children[[1]]$children$layout <- .g[idx,]
  }else{
    idx <- which(g$layout$name %in% c("xlab", "axis-b", "title"))
    idx <- sort(unique(c(g$layout$t[idx], g$layout$b[idx])))
    idx <- setdiff(1:nrow(g), idx)
    g <- g[idx, ]
  }
  g
}

removeYAxis <- function(g){
  if(g$name == "panel.ori"){
    .g <- g$grobs[[1]]$children[[1]]$children$layout
    idx <- which(.g$layout$name %in% c("axis-l", "ylab"))
    idx <- sort(unique(c(.g$layout$l[idx], .g$layout$r[idx])))
    idx <- setdiff(1:ncol(.g), idx)
    g$grobs[[1]]$children[[1]]$children$layout <- .g[,idx]
  }else{
    idx <- which(g$layout$name %in% c("axis-l", "ylab"))
    idx <- sort(unique(c(g$layout$l[idx], g$layout$r[idx])))
    idx <- setdiff(1:ncol(g), idx)
    g <- g[,idx]
  }
  g
}

getAxisHeight <- function(p, base){
  .base <- as.numeric(base)
  p2 <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  h1 <- sum(Grob(p)$height)
  h2 <- sum(Grob(p2)$height)
  .h <- convertUnit(h1, "cm", valueOnly = TRUE)/convertUnit(h2, "cm", valueOnly = TRUE) * .base
  unit(.h, "null")
}

getHeight <- function(dts){
  hts <- do.call(unit.c, lapply(dts, height))
  hts
}

parseHeight <- function(hts, n){
  if(length(hts) != n && length(hts) != 1)
    stop("Heights must be of length 1 or numbers of graphics")
  if(is.numeric(hts) && !is.unit(hts)){
    if(length(hts) == 1)
      res <- rep(unit(1, "null"), n)
    if(length(hts) == n)
      res <- unit(hts, "null")
  }else if(is.unit(hts)){
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
setMethod("Arith", signature = c("Tracks", "Tracks"), function(e1, e2) {
     switch(.Generic,
            "+"= {
              e1 <- c(e1, e2)
            },
            stop("unhandled 'Arith' operator '", .Generic, "'"))
    e1
})

setMethod("c", "Tracks",  function(x, ...){
        if (missing(x)) {
            args <- unname(list(...))
            x <- args[[1L]]
        } else {
            args <- unname(list(x, ...))
        }
        if (length(args) == 1L)
            return(x)
        arg_is_null <- sapply(args, is.null)
        if (any(arg_is_null))
            args[arg_is_null] <- NULL  # remove NULL elements by setting them to NULL!
        if (!all(sapply(args, is, class(x))))
            stop("all arguments in '...' must be ", class(x), " objects (or NULLs)")
        lst <- lapply(args, function(x){
          x@grobs
        })
        ## FIXME: how to keep other attributes?
        res <- do.call(tracks, do.call(c, lst))
        res
})

setMethod("cbind", "Tracks",  function(...){
  args <- list(...)
  if(all(sapply(args, is, "Tracks"))){
    lst <- lapply(args, get_gtable)
    res <- do.call(cbind, lst)
  }else{
    stop("need to be of class Tracks")
  }
   grid.draw(res)
})


setMethod("rbind", "Tracks",  function(...){
  args <- list(...)
  if(all(sapply(args, is, "Tracks"))){
    lst <- lapply(args, get_gtable)
    res <- do.call(rbind, lst)
  }else{
    stop("need to be of class Tracks")
  }
   grid.draw(res)
})





setMethod("[", c("Tracks", "numeric", "missing", "ANY"),
          function(x, i, j, ..., drop=TRUE){
              i <- as.integer(i)
              initialize(x,
                  grobs = x@grobs[i],
                  plot = x@plot[i],
                  ## backup = backup,
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
                  track.plot.color = x@track.plot.color[i],
                  track.bg.color = x@track.bg.color[i],
                  label.text.cex = x@label.text.cex[i],
                  label.width = x@label.width)
          })


