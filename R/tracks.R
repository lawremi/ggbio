setOldClass("ggplot")
setOldClass("options")
setOldClass("unit")
setClassUnion("optionsORNULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
setClass("ggplotGrobList", prototype = prototype(elementType = "ggplot"),
         contains = "list")

ggplotGrobList <- function(...){
  items <- list(...)
  if (length(items) == 1 && is.list(items[[1L]]))
    items <- items[[1L]]
  if (!all(sapply(items, is, "ggplot")))
    stop("all elements in '...' must be Item objects")
  new("ggplotGrobList", items)
}


tracks.gen <- setClass("Tracks",
                       representation(grobs = "ggplotGrobList",
                                      backup = "list",
                                      heights = "numericORunit",
                                      xlim = "numeric",
                                      ylim = "list",
                                      xlab = "characterORNULL",
                                      opts = "optionsORNULL",
                                      track.skip = "numeric",
                                      xlim.change = "logical",
                                      track.plot.col = "character"))


tracks <- function(..., heights, xlim, xlab = NULL,                
                   opts = NULL, track.skip = -1,
                   xlim.change = rep(TRUE, length(list(...))),
                   track.plot.col = rep("white", nrow)){

  dots <- list(...)
  nrow <- length(dots)
  if(missing(heights))
    heights <- unit(rep(1, nrow), "null")
  grobs <- dots
  if(missing(xlim)){
    lst <- lapply(grobs[xlim.change], function(obj){
      res <- getLimits(obj)
      data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
    })
    res <- do.call(rbind, lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    xlim <- scales::expand_range(xlim, mul = 0.05)
  }else{
    if(is(xlim, "IRanges")){
      xlim <- c(start(xlim), end(xlim))
    }
    if(is(xlim,"GRanges")){
      xlim <- c(start(ranges(reduce(xlim, ignore.strand = TRUE))),
                end(ranges(reduce(xlim, ignore.strand = TRUE))))
    }
    if(is.numeric(xlim)){
      xlim <- range(xlim)
    }
  }
  ylim <- lapply(grobs, function(grob) getLimits(grob)$ylim)
  backup <- list(grobs = ggplotGrobList(...), 
      heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, opts = opts,
      track.skip = track.skip, xlim.change = xlim.change,
      track.plot.col = track.plot.col)
  new("Tracks", grobs = ggplotGrobList(...), backup = backup,
      heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, opts = opts,
      track.skip = track.skip, xlim.change = xlim.change,
      track.plot.col = track.plot.col)
}


setMethod("summary", "Tracks", function(object){
  cat("-------------------------------------------\n")
  cat("Tracks contains: ", length(object@grobs), " graphic objects\n")
  cat("-------------------------------------------\n")
  cat("xlim:", object@xlim, "\n")
  cat("heights", object@heights, "\n")
  cat("track.skip", object@track.skip, "\n")
  cat("xlim.chage", object@xlim.change, "\n")
  cat("track.plot.col", object@track.plot.col, "\n")
  cat("-------------------------------------------\n")
})


setMethod("print", "Tracks", function(x){
    grobs <- x@grobs
    N <- length(grobs)
    lst <- lapply(seq_len(N),
                  function(i) {
                    ylim <- x@ylim[[i]]
                    s <- coord_cartesian(xlim = x@xlim, ylim = ylim, wise = TRUE)
                    grobs[[i]] <- grobs[[i]] + opts(plot.background = theme_rect(colour = NA, fill = x@track.plot.col[i]))
                    if(i %in% which(x@xlim.change))
                      grobs[[i]] <- grobs[[i]] + s
                    if(!is.null(opts))
                      grobs[[i]] <- grobs[[i]] + x@opts
                    ## margin
                    if(!is.null(x@track.skip))
                      if(i < N){  s <- coord_cartesian(xlim = x@xlim, wise = TRUE)
                        grobs[[i]] <- grobs[[i]] +
                          opts(plot.margin = unit(c(1, 1, x@track.skip, 0.5), "lines"))
                      }
                    ## xlab
                    if(i %in% 1:(N-1)){
                      grobs[[i]] <- grobs[[i]] + 
                          opts(axis.text.x = theme_blank()) + xlab("") 
                    }else{
                      if(!is.null(xlab))
                        grobs[[i]] <- grobs[[i]] + xlab(x@xlab)
                      grobs[[i]] <- grobs[[i]] +
                        opts(axis.title.x = theme_text(vjust = 0))
                    }
                    grobs[[i]] 
                  })
  res <- do.call(align.plots, c(lst, list(heights = x@heights)))
  res
})

setMethod("show", "Tracks", function(object){
  print(object)
})


setMethod("Arith", c("Tracks", "ANY"), function(e1, e2) {
     switch(.Generic,
            "+"= {
                N <- length(e1@grobs)
                for(i in 1:N){
                  e1@grobs[[i]] <- e1@grobs[[i]] + e2
                }
            },
            stop("unhandled 'Arith' operator '", .Generic, "'"))
     e1
})

setGeneric("xlim",function(obj, ...) standardGeneric("xlim"))

setMethod("xlim", "numeric", function(obj, ...){
  ggplot2::xlim(obj, ...)
})
setMethod("xlim", "Tracks", function(obj, ...){
  obj@xlim
})
setGeneric("xlim<-", function(x, value) standard("xlim<-"))

setReplaceMethod("xlim", c("Tracks", "IRanges"), function(x, value){
    xlim <- c(start(value), end(value))
    x@xlim <- xlim
    lapply(1:length(x@grobs), function(i){
      ylim <- x@ylim[[i]]
      s <- coord_cartesian(xlim = x@xlim, ylim = ylim, wise = TRUE)
      if(i %in% which(x@xlim.change))
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
    s <- coord_cartesian(xlim = x@xlim, ylim = ylim, wise = TRUE)
    if(i %in% which(x@xlim.change))
      x@grobs[[i]] <- x@grobs[[i]] + s
  })
  x
})

setReplaceMethod("xlim", c("Tracks", "numeric"), function(x, value){
  xlim <- range(value)
  x@xlim <- xlim
  lapply(1:length(x@grobs), function(i){
    ylim <- x@ylim[[i]]
    s <- coord_cartesian(xlim = x@xlim, ylim = ylim, wise = TRUE)
    if(i %in% which(x@xlim.change))
      x@grobs[[i]] <- x@grobs[[i]] + s
  })
  x
})


setMethod("update", "Tracks", function(object, xlim){
  xlim(object) <- xlim
  print(object)
})

setGeneric("reset", function(obj, ...) standardGeneric("reset"))
setMethod("reset", "Tracks", function(obj){
  nms <- setdiff(slotNames(obj), "backup")
  for(nm in nms){
    slot(obj, nm) <- obj@backup[[nm]]
  }
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

##
## TODO: adust due to left/right legend
align.plots <- function (..., vertical = TRUE,
                         heights = unit(rep(1, nrow), "null")) 
{

  if (!vertical) stop("only vertical alignment implemented")
  dots0 <- list(...)
  dots0 <- lapply(dots0, function(.g){
    if(!"y" %in% names(.g$options$labels)){
      .g$options$labels$y <- ""
    }
    .g
  })
  nrow <- length(dots0)
  dots <- lapply(dots0, ggplotGrob)
  legend.pos <- lapply(dots0,
                       function(x) {
                         if (is.null(x$options$legend.pos)) "right"
                         else x$options$legend.pos })

  ytitles <- lapply(dots, function(.g){
    grob.y.text <- getGrob(.g, "axis.title.y.text", grep = TRUE)
    if(!is.null(grob.y.text))
      editGrob(grob.y.text, vp = NULL)
    else
      ggplot2:::zeroGrob()
  })
  
  ylabels <- lapply(dots, function(.g){
    grob.y.text <- getGrob(.g, "axis.text.y.text", grep = TRUE)
    if(!is.null(grob.y.text))
      editGrob(grob.y.text, vp = NULL)
    else
      ggplot2:::zeroGrob()
  })

  legends <- lapply(dots0,function(.g){
    gt <- ggplot_gtable(ggplot_build(.g))
    idx <- grep("guide-box", gt$layout$name)
    l.pos <- ggplot2:::plot_theme(.g)$legend.position
    if(length(idx)){
      if(l.pos == "right"){
        gt$widths[gt$layout[idx,"r"]]
      }else{
        warning("only support right legend now")
        grobWidth(ggplot2:::zeroGrob())
      }
    }else{
      grobWidth(ggplot2:::zeroGrob())
    }
  })


  strips <- lapply(dots, function(.g) {
    cc <- .g$children
    vstrips <- cc[grepl("^strip-right",names(cc))]
    if (length(vstrips)>0) 
      editGrob(vstrips[[1]],vp=NULL)
    else ggplot2:::zeroGrob()
  })
  gl <- grid.layout(nrow = length(dots), heights=heights)
  vp <- viewport(layout = gl)
  pushViewport(vp)

  widths.left <- mapply(`+`, e1 = lapply(ytitles, grobWidth), 
                        e2 = lapply(ylabels, grobWidth), SIMPLIFY = FALSE)

  widths.right <- mapply(function(g,lp,s) {
    g + grobWidth(s)
  }, legends,legend.pos,strips,  SIMPLIFY=FALSE)
  
  widths.left.max <- max(do.call(unit.c, widths.left))
  widths.right.max <- max(do.call(unit.c, widths.right))

  for (ii in seq_along(dots)) {
    pushViewport(viewport(layout.pos.row = ii))
    pushViewport(viewport(x = unit(0, "npc") + widths.left.max - 
                          widths.left[[ii]], width = unit(1, "npc") - widths.left.max + 
                          widths.left[[ii]] - widths.right.max + widths.right[[ii]], 
                          just = "left"))
    grid.draw(dots[[ii]])
    upViewport(2)
  }
}


