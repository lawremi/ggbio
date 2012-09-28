setOldClass("options")
setOldClass("unit")
setOldClass("gtable")
setOldClass("theme")
setOldClass("gTree")
setOldClass("grob")
setClassUnion("themeORNULL", c("theme", "NULL"))
setClassUnion("optionsORNULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
setClass("ideogram", contains = c("gg", "ggplot"))
ideogram <- function(x){
  new("ideogram", x)  
}

## suppose to store original graphics
setClass("ggplotGrobList", prototype = prototype(elementType = "ggplot"),
         contains = "list")


ggplotGrobList <- function(...){
  items <- list(...)
  if (length(items) == 1 && is.list(items[[1L]]) && !is(items[[1L]], "ggplot"))
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
                   fixed = NULL,
                   track.plot.color = NULL,
                   track.bg.color = NULL,
                   main.height = unit(2, "lines"),
                   scale.height = unit(2, "lines"),
                   xlab.height = unit(2, "lines"),
                   padding = unit(0, "lines"),
                   label.bg.color =  "white",
                   label.bg.fill = "gray80",
                   label.text.color = "black",
                   label.text.cex = 1,                   
                   label.width = unit(2.5, "lines")){

  if(is.numeric(padding) && !is.unit(padding))
    padding <- unit(padding, "lines")
  
  if(!is.null(title) && is.null(main))
    main <- title
  
  dots <- list(...)
  if(length(dots) == 1 && is.list(dots[[1]]) && !is(dots[[1]], "ggplot"))
    dots <- dots[[1]]

  nms <- names(dots)
  if(is.null(fixed)){
    fixed <- sapply(dots, fixed)
  }else{
    stopifnot(length(fixed) == length(dots))
    dots <- lapply(1:length(dots), function(i){
      fixed(dots[[i]]) <- fixed[i]
      dots[[i]]
    })
  }

  ideo <- !sapply(dots, is, "ideogram")
  
  if(missing(xlim)){
    lst <- lapply(dots[!fixed & ideo], function(obj){
      res <- getLimits(obj)
      data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
    })
    res <- do.call(rbind, lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    xlim <- scales::expand_range(xlim, mul = 0.1)
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

  mts <- sapply(dots, mutable)
  dots <- lapply(1:length(dots), function(i){
    mutable(dots[[i]]) <- mts[i]
    dots[[i]]
  })  
  axs <- sapply(dots, hasAxis)
  dots <- lapply(1:length(dots), function(i){
    hasAxis(dots[[i]]) <- axs[i]
    dots[[i]]
  })
  names(dots) <- nms
  ## get height
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

  if(missing(heights))
    heights <- getHeight(dots)
  else
    heights <- parseHeight(heights, length(dots))
  
  fixs <- sapply(dots, fixed)

  ylim <- lapply(dots[!fixs & ideo], function(grob){
     scales::expand_range(getLimits(grob)$ylim, mul = 0.05)
  })

  ## if(is.null(names(dots)))
  ##   labeled <- rep(FALSE
  ## else
  labeled <- sapply(dots, labeled)

  gbl <- do.call(ggplotGrobList, dots)
  
  backup <- list(grobs = gbl,
                 heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab,
                 main = main,
                 main.height = main.height,
                 scale.height = scale.height,
                 xlab.height = xlab.height,
                 theme = theme, mutable = mts, hasAxis = axs,
                 fixed = fixs, padding = padding,
                 labeled = labeled, label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
                 label.text.color = label.text.color,
                 track.plot.color = track.plot.color,
                 track.bg.color = track.bg.color,                 
                 label.text.cex = label.text.cex,
                 label.width = label.width)
  
  new("Tracks", grobs = gbl, backup = backup, labeled = labeled, 
      heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, main = main,
      main.height = main.height,
      scale.height = scale.height,
      xlab.height = xlab.height,
      theme = theme,  mutable = mts, hasAxis = axs,
      fixed = fixs, padding = padding,
      label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
      label.text.color = label.text.color,      
      track.plot.color = track.plot.color,
      track.bg.color = track.bg.color,                       
      label.text.cex = label.text.cex,
      label.width = label.width)
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
  grid.newpage()
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
                                             add.scale = TRUE,
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
                                             add.scale = TRUE,
                                             main = x@main,
                                             xlab = x@xlab,
                                             main.height = x@main.height,
                                             scale.height = x@scale.height,
                                             xlab.height = x@xlab.height
)))
})

setMethod("show", "Tracks", function(object){
  print(object)
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
      if(!fixed(e1@grobs[[i]]) && !is(e1@grobs[[i]], "ideogram"))
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

setGeneric("xlim",function(obj, ...) standardGeneric("xlim"))


setMethod("xlim", "numeric", function(obj, ...){
  if(length(list(...)))
    obj <- c(obj, ...)
  if(length(obj) > 2){
    obj <- range(obj)
  }
  ggplot2::coord_cartesian(xlim = obj)
})


setMethod("xlim", "IRanges", function(obj, ...){
    xlim <- c(start(obj), end(obj))
  ggplot2::coord_cartesian(xlim = xlim)
})

setMethod("xlim", "GRanges", function(obj, ...){
  xlim <- c(start(ranges(reduce(obj, ignore.strand = TRUE))),
            end(ranges(reduce(obj, ignore.strand = TRUE))))
  res <- ggplot2::coord_cartesian(xlim = xlim)
  chr <- unique(as.character(seqnames(obj)))
  attr(res, "chr") <- chr
  res
})

setMethod("xlim", "Tracks", function(obj, ...){
  obj@xlim
})

setGeneric("xlim<-", function(x, value) standardGeneric("xlim<-"))

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

setMethod("update", "Tracks", function(object, xlim){
  xlim(object) <- xlim
  object@xlim <- xlim
  print(object)
})

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
                       plot = TRUE,  padding = NULL, 
                       track.plot.color = NULL,
                       track.bg.color = NULL,                                             
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,                   
                       label.width = unit(2.5, "lines"),
                       add.scale = FALSE,
                       main.height = unit(2, "lines"),
                       scale.height = unit(2, "lines"),
                       xlab.height = unit(2, "lines"),
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
ggl[[1]]  
  if(length(ggl)){
    if(length(ggl) == 1 && is.list(ggl[[1]])){
      ggl <- ggl[[1]]
    }}else{
      return(ggplot())
    }

  N <- length(ggl)

  ## plot background 
  if(is.null(track.plot.color)){
    if(is.null(track.bg.color))
      track.plot.color <- sapply(ggl, bgColor)
    else
      track.plot.color <- rep(track.bg.color, length(ggl))
  }
  stopifnot(length(track.plot.color) != N || length(track.plot.color) != 1)
  
  if(length(track.plot.color) == 1){
      track.plot.color <- rep(track.plot.color, N)
  }

  ## add a plot with axis and remove later
  if(add.scale){
    idx.fix <- which(!sapply(ggl, fixed) & !sapply(ggl, is, "ideogram"))[1]
    if(is.na(idx.fix))
      idx.fix <- 1
    ggl <- c(ggl, list(ggl[[idx.fix]] + theme_gray()))
  }

  ## parse grobs
  grobs <- lapply(ggl, function(x){
    gg <- ggplotGrobAttr(x)
    gg
  })

  addLabel <- function(grobs, nms, lbs,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,                   
                       label.width = unit(2.5, "lines"),
                       direction = c("row", "col")
                       ){

    label.text.color <- rep(label.text.color, len = length(grobs))
    label.text.cex <- rep(label.text.cex, len = length(grobs))
    label.bg.color <- rep(label.bg.color, len = length(grobs))            
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
        gt <- gtable(width = unit.c(label.width,unit(1, "null")), height = unit(1, "null"))
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
        gt <- gtable(width = unit(1, "null"), height = unit.c(label.width,unit(1, "null")))
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
    grobs[[i]] <- copyAttr(.grob, grobs[[i]])
  })
  names(grobs) <- .nms
  if(add.scale){
    g.last <- grobs[[length(grobs)]]
    grobs <- grobs[-length(grobs)]
  }
  if(add.scale){
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
        rect.grob <- rectGrob(gp=gpar(col = track.bg.color,
                                fill = track.bg.color))
        text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
      }
    grobs <- c(list(text.grob), grobs)      
    }
    if(length(xlab)){
      text.grob <- textGrob(xlab)      
      if(length(track.bg.color)){
        rect.grob <- rectGrob(gp=gpar(col = track.bg.color,
                                fill = track.bg.color))
        text.grob <- grobTree(gTree(children = gList(rect.grob, text.grob)))
      }
      grobs <- c(grobs, list(text.grob))
    }
  }
  lbs <- sapply(grobs, labeled)
  nms <- names(grobs)

  if(any(remove.y.axis)){
    for(i in which(remove.y.axis))
      grobs[[i]] <- removeYAxis(grobs[[i]])
  }
  if(any(remove.x.axis)){
    for(i in which(remove.x.axis))
      grobs[[i]] <- removeXAxis(grobs[[i]])
  }

  ## add lables
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
      grobs <- addLabel(grobs, nms, lbs,
                        label.bg.color =  label.bg.color,
                       label.bg.fill = label.bg.fill,
                       label.text.color = label.text.color,
                       label.text.cex = label.text.cex,                   
                       label.width = label.width,
                        direction = "col")
  }

  

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
    if(add.scale)    
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
  if(plot){
    grid.newpage()
    grid.draw(tab)
  }else{
    tab
  }
}

getPanelIndex <- function(g){
  if(inherits(g, "ggplot"))
    g <- ggplotGrobAttr(g)
  idx <- which("panel" == g$layout$name)
  idx
}

spaceAroundPanel <- function(g, type = c("t", "l", "b", "r")){
  if(inherits(g, "ggplot"))
    g <- ggplotGrobAttr(g)
  idx <- getPanelIndex(g)
  rsl <- list()
  for(tp in type){
    rsl[[tp]] <- switch(tp,
                t = {
                  id <- which(g$layout$t < min(g$layout[idx, ]$t))
                  id <- id[!duplicated(g$layout$name[id])]                  
                  if(length(id))
                    res <- sum(g$height[unique(g$layout$t[id])])  
                  else
                    res <- unit(0, "inches")
                  res
                },
                l = {
                  id <- which(g$layout$l < min(g$layout[idx, ]$l))
                  id <- id[!duplicated(g$layout$name[id])]
                  if(length(id))
                    res <- sum(g$width[unique(g$layout$l[id])])  
                  else
                    res <- unit(0, "inches")
                  res
                },
                b = {
                  id <- which(g$layout$b > max(g$layout[idx, ]$b))
                  id <- id[!duplicated(g$layout$name[id])]                  
                  if(length(id))
                    res <- sum(g$height[unique(g$layout$b[id])])  
                  else
                    res <- unit(0, "inches")
                  res

                },
                r = {
                  id <- which(g$layout$r > max(g$layout[idx, ]$r))
                  id <- id[!duplicated(g$layout$name[id])]                  
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
  grobs <- lapply(lst, function(p){
    if(inherits(p, "ggplot"))
      g <- ggplotGrobAttr(p)
    else
      g <- p
    g
  })
  if(dir == "row"){
    slst <- lapply(grobs, spaceAroundPanel, c("l", "r"))
    lmx <- do.call(max, lapply(slst, function(lst) lst$l))
    rmx <- do.call(max, lapply(slst, function(lst) lst$r))
    for(i in 1:length(grobs)){
      .grob <- grobs[[i]]
      gt <- gtable(unit(1, "null"), unit(1, "null"), name = "panel.ori")
      grobs[[i]] <- gtable_add_cols(grobs[[i]], lmx - slst[[i]]$l ,
                                    pos = 0)
      grobs[[i]] <- gtable_add_cols(grobs[[i]], rmx - slst[[i]]$r, pos = -1)
      rect.grob <- rectGrob(gp = gpar(fill = NA, col = NA),
                            name = "bgColor")
      ## rect.grob <- rectGrob(gp = gpar(fill = NA, col = NA))
      all.grob <- grobTree(gTree(children = gList(rect.grob, grobs[[i]])))      
      grobs[[i]] <- gtable_add_grob(gt, all.grob, 1, 1)
      grobs[[i]] <- copyAttr(.grob, grobs[[i]])
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
      grobs[[i]] <- copyAttr(.grob, grobs[[i]])      
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
  mutable(p) <- FALSE
  hasAxis(p) <- TRUE
  ## height(p) <- unit(4, "lines")
  p
}

## always comes last
scaleGrob <- function(g){
  idx <- g$layout$name %in% c("axis-b", "xlab")
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

## xlabGrob <- function(p){
##   p <- ScalePlot(x)  
##   g <- ggplotGrobAttr(p)
##   idx <- g$layout$name %in% c("xlab")
##   idx <- unique(c(g$layout[idx, "t"], g$layout[idx, "b"]))
##   g[idx,]
## }



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

setGeneric("bgColor",  function(x, ...) standardGeneric("bgColor"))
setGeneric("bgColor<-",  function(x, value,  ...) standardGeneric("bgColor<-"))
setMethod("bgColor", "gg", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})
setReplaceMethod("bgColor", c("gg", "character"), function(x, value){
  attr(x, "bgcolor") <- value
  x
})

setMethod("bgColor", "gtable", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})

setReplaceMethod("bgColor", c("gtable", "character"), function(x, value){
  attr(x, "bgcolor") <- value
  x
})

setMethod("bgColor", "ideogram", function(x){
  bg <- attr(x, "bgcolor")
  if(is.null(bg))
    return("white")
  else
    return(bg)
})

setReplaceMethod("bgColor", c("ideogram", "character"), function(x, value){
  attr(x, "bgcolor") <- value
  x
})


setMethod("fixed", "gg", function(x){
  res <- attr(x, "fixed")
  if(is.null(res))
    return(FALSE)
  else
    return(res)
})


setReplaceMethod("fixed", c("gg", "logical"), function(x, value){
  attr(x, "fixed") <- value
  x
})

setMethod("fixed", "ideogram", function(x){
  res <- attr(x, "fixed")
  if(is.null(res))
    return(FALSE)
  else
    return(res)
})


setReplaceMethod("fixed", c("ideogram", "logical"), function(x, value){
  attr(x, "fixed") <- value
  x
})

setMethod("fixed", "gtable", function(x){
  res <- attr(x, "fixed")
  if(is.null(res))
    return(FALSE)
  else
    return(res)
})

setReplaceMethod("fixed", c("gtable", "logical"), function(x, value){
  attr(x, "fixed") <- value
  x
})


setGeneric("labeled",  function(x, ...) standardGeneric("labeled"))
setGeneric("labeled<-",  function(x, value,  ...) standardGeneric("labeled<-"))
setMethod("labeled", "gg", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("gg", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
})

setMethod("labeled", "ideogram", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("ideogram", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
})


setMethod("labeled", "gtable", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setReplaceMethod("labeled", c("gtable", "logical"), function(x, value){
  attr(x, "labeled") <- value
  x
}
)

setOldClass("text")
setMethod("labeled", "text", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setMethod("labeled", "gTree", function(x){
  bg <- attr(x, "labeled")
  if(is.null(bg))
    return(TRUE)
  else
    return(bg)
})

setGeneric("mutable",  function(x, ...) standardGeneric("mutable"))
setGeneric("mutable<-",  function(x, value,  ...) standardGeneric("mutable<-"))
setMethod("mutable", "gg", function(x){
  mt <- attr(x, "mutable")
  if(is.null(mt))
    return(TRUE)
  else
    return(mt)
})

setReplaceMethod("mutable", c("gg", "logical"), function(x, value){
  attr(x, "mutable") <- value
  x
})

setMethod("mutable", "ideogram", function(x){
  mt <- attr(x, "mutable")
  if(is.null(mt))
    return(TRUE)
  else
    return(mt)
})

setReplaceMethod("mutable", c("ideogram", "logical"), function(x, value){
  attr(x, "mutable") <- value
  x
})


setMethod("mutable", "gtable", function(x){
  mt <- attr(x, "mutable")
  if(is.null(mt))
    return(TRUE)
  else
    return(mt)
})

setReplaceMethod("mutable", c("gtable", "logical"), function(x, value){
  attr(x, "mutable") <- value
  x
})


setGeneric("hasAxis",  function(x, ...) standardGeneric("hasAxis"))
setGeneric("hasAxis<-",  function(x, value,  ...) standardGeneric("hasAxis<-"))
setMethod("hasAxis", "gg", function(x){
  mt <- attr(x, "hasAxis")
  if(is.null(mt))
    return(FALSE)
  else
    return(mt)
})

setReplaceMethod("hasAxis", c("gg", "logical"), function(x, value){
  attr(x, "hasAxis") <- value
  x
})

setMethod("hasAxis", "ideogram", function(x){
  mt <- attr(x, "hasAxis")
  if(is.null(mt))
    return(FALSE)
  else
    return(mt)
})

setReplaceMethod("hasAxis", c("ideogram", "logical"), function(x, value){
  attr(x, "hasAxis") <- value
  x
})


setMethod("hasAxis", "gtable", function(x){
  mt <- attr(x, "hasAxis")
  if(is.null(mt))
    return(FALSE)
  else
    return(mt)
})

setReplaceMethod("hasAxis", c("gtable", "logical"), function(x, value){
  attr(x, "hasAxis") <- value
  x
}
)

setGeneric("height",  function(x, ...) standardGeneric("height"))
setGeneric("height<-",  function(x, value,  ...) standardGeneric("height<-"))
setMethod("height", "gg", function(x){
  ht <- attr(x, "height")
  if(is.null(ht))
    return(unit(1, "null"))
  else if(is.numeric(ht)  && !is.unit(ht)){
    return(unit(mt, "null"))
  }else if(is.unit(ht)){
    return(ht)
  }else{
    stop("height attribute must be numeric or ")
  }
})

setReplaceMethod("height", c("gg", "numericORunit"), function(x, value){
  if(length(value) != 1)
    stop("height value can only be of length 1.")
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  attr(x, "height") <- value
  x
})

setMethod("height", "ideogram", function(x){
  ht <- attr(x, "height")
  if(is.null(ht))
    return(unit(1, "null"))
  else if(is.numeric(ht)  && !is.unit(ht)){
    return(unit(mt, "null"))
  }else if(is.unit(ht)){
    return(ht)
  }else{
    stop("height attribute must be numeric or ")
  }
})

setReplaceMethod("height", c("ideogram", "numericORunit"), function(x, value){
  if(length(value) != 1)
    stop("height value can only be of length 1.")
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  attr(x, "height") <- value
  x
})

setMethod("height", "gtable", function(x){
  ht <- attr(x, "height")
  if(is.null(ht))
    return(unit(1, "null"))
  else if(is.numeric(ht)  && !is.unit(ht)){
    return(unit(mt, "null"))
  }else if(is.unit(ht)){
    return(ht)
  }else{
    stop("height attribute must be numeric or ")
  }
})

setReplaceMethod("height", c("gtable", "numericORunit"), function(x, value){
  if(length(value) != 1)
    stop("height value can only be of length 1.")
  if(is.numeric(value) && !is.unit(value))
    value <- unit(value, "null")
  attr(x, "height") <- value
  x
})

textPlot <- function(lb="", ut = unit(4, "lines")){
  df <- data.frame(x =1:10, y = 1)
  p <- qplot(x = 1, y = 1, geom = "text", label = lb) + theme_null()+
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.margin = unit(c(0, 0, 0, 0), "lines"))
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
  h1 <- sum(ggplotGrobAttr(p)$height)
  h2 <- sum(ggplotGrobAttr(p2)$height)
  .h <- convertUnit(h1, "cm", value = TRUE)/convertUnit(h2, "cm", value = TRUE) * .base
  unit(.h, "null")
}

ggplotGrobAttr <- function(x){
  attrs <- attributes(x)
  attrs <- attrs[setdiff(names(attrs), c("class", "names"))]
  res <- ggplotGrob(x)
  attrs <- c(attrs, attributes(res))
  attributes(res) <- attrs
  res
}

## from x1 object to x2 object
copyAttr <- function(x1, x2){
  attrs <- attributes(x1)
  attrs <- attrs[setdiff(names(attrs), c("class", "names"))]
  attrs <- c(attrs, attributes(x2))
  attributes(x2) <- attrs
  x2
}
