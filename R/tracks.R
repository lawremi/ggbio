setOldClass("options")
setOldClass("unit")
setClassUnion("optionsORNULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
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
                                      theme = "optionsORNULL",
                                      track.skip = "numeric",
                                      fixed = "logical",
                                      named = "logical",
                                      label.bg.color = "character",
                                      label.bg.fill = "character",
                                      label.text.color = "character",
                                      label.text.cex = "numeric",
                                      track.plot.color = "characterORNULL",
                                      label.width = "unit"))

.tracks.theme <- setdiff(slotNames("Tracks"), c("backup", "grobs"))

tracks <- function(..., heights, xlim, xlab = NULL,                
                   theme = NULL, track.skip = -1,
                   fixed = NULL,
                   track.plot.color = NULL,
                   scale.width = unit(3, "line"),
                   scale.line.color = "gray75",
                   label.bg.color =  "white",
                   label.bg.fill = "gray80",
                   label.text.color = "black",
                   label.text.cex = 1,                   
                   label.width = unit(2.5, "line")){

  dots <- list(...)
  if(length(dots) == 1 && is.list(dots[[1]]) && !is(dots[[1]], "ggplot"))
    dots <- dots[[1]]

  if(is.null(fixed))
    fixed <- sapply(dots, fixed)

  if(missing(xlim)){
    lst <- lapply(dots[!fixed], function(obj){
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

  nrow <- length(dots)

  getHeight <- function(h, n, scale.width){
    if(is.numeric(heights) && !is.unit(heights)){
      heights <- unit(heights, "null")
    }
    if(length(heights) == n){
      heights <- unit.c(heights, scale.width)
    } else if(length(heights) == 1){
      heights <- rep(heights, nrow)
      heights <- unit.c(heights, scale.width)
    } else{
      stop("heights(if specified), must be the same length
           of plots passed in tracks(), or just one single numeric value or single unit")
    }
  }
  
  if(missing(heights))
    heights <- unit.c(unit(rep(1, nrow), "null"), scale.width)
  else
    heights <- getHeight(heights, nrow, scale.width)
  
  
  ylim <- lapply(dots, function(grob){
     scales::expand_range(getLimits(grob)$ylim, mul = 0.05)
  })

  ## adding scale
  dots <- c(dots, list(ScalePlot(xlim, color = scale.line.color)))
  
  if(is.null(names(dots)))
    named <- FALSE
  else
    named <- TRUE
  
  fixed <- c(fixed, FALSE)

  gbl <- do.call(ggplotGrobList, dots)
  
  backup <- list(grobs = gbl,
                 heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, theme = theme,
                 track.skip = track.skip, fixed = fixed,
                 named = named, label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
                 label.text.color = label.text.color,
                 track.plot.color = track.plot.color,
                 label.text.cex = label.text.cex,
                 label.width = label.width)
  
  new("Tracks", grobs = gbl, backup = backup, named = named, 
      heights = heights,  xlim = xlim,  ylim = ylim, xlab = xlab, theme = theme,
      track.skip = track.skip, fixed = fixed,
      label.bg.color = label.bg.color, label.bg.fill = label.bg.fill,
      label.text.color = label.text.color,      
      track.plot.color = track.plot.color,
      label.text.cex = label.text.cex,
      label.width = label.width)
}


setMethod("summary", "Tracks", function(object){
  cat("-------------------------------------------\n")
  cat("Tracks contains: ", length(object@grobs), " graphic objects\n")
  cat("-------------------------------------------\n")
  cat("xlim:", object@xlim, "\n")
  cat("heights", object@heights, "\n")
  cat("track.skip", object@track.skip, "\n")
  cat("fixed", object@fixed, "\n")
  cat("track.plot.color", object@track.plot.color, "\n")
  cat("-------------------------------------------\n")
})


setMethod("print", "Tracks", function(x){

  grid.newpage()
    grobs <- x@grobs
    N <- length(grobs)
    if(x@named)
      nms <- names(x@grobs)
    else
      nms <- NULL
    lst <- lapply(seq_len(N),
                  function(i) {
                    s <- coord_cartesian(xlim = x@xlim)
                    if(i %in% which(!x@fixed))
                      grobs[[i]] <- grobs[[i]] + s
                    if(!is.null(theme))
                      grobs[[i]] <- grobs[[i]] + x@theme
                    ## margin
                    if(!is.null(x@track.skip))
                      if(i < N){  s <- coord_cartesian(xlim = x@xlim)
                        grobs[[i]] <- grobs[[i]] +
                          theme(plot.margin = unit(c(1, 1, x@track.skip, 0.5), "lines"))
                      }
                    ## xlab
                    if(i %in% 1:(N-1)){
                      grobs[[i]] <- grobs[[i]] +
                          theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank()) + xlab("") 
                    }else{
                      if(!is.null(xlab))
                        grobs[[i]] <- grobs[[i]] + xlab(x@xlab)
                      grobs[[i]] <- grobs[[i]] +
                        theme(axis.title.x = element_text(vjust = 0))
                    }
                    grobs[[i]]
                  })

  names(lst) <- nms

    if(x@named)
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             label.bg.color =  x@label.bg.color,
                                             label.bg.fill = x@label.bg.fill,
                                             label.text.color = x@label.text.color,
                                             label.text.cex = x@label.text.cex,                   
                                             label.width = x@label.width,
                                             track.plot.color = x@track.plot.color)))
    else
      res <- do.call(alignPlots, c(lst, list(heights = x@heights,
                                             track.plot.color = x@track.plot.color)))
})

setMethod("show", "Tracks", function(object){
  print(object)
})


setMethod("Arith", signature = c("Tracks", "ANY"), function(e1, e2) {
     switch(.Generic,
            "+"= {
              N <- length(e1@grobs)
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
      e1@ylim[[i]] <- e2$limits$y    
    }
  }
  N <- length(e1@grobs)
  for(i in 1:N){
    e1@grobs[[i]] <- e1@grobs[[i]] + e2
  }
  e1
})

setGeneric("xlim",function(obj, ...) standardGeneric("xlim"))

setMethod("xlim", "numeric", function(obj, ...){
  ggplot2::xlim(obj, ...)
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
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,                   
                       label.width = unit(2.5, "line")){
  ## check
  if(!is.null(height) && is.null(heights))
    heights <- height

  if(!is.null(width) && is.null(widths))
    widths <- width
  
  ggl <- list(...)

  if(length(ggl)){
    if(length(ggl) == 1 && is.list(ggl[[1]])){
      ggl <- ggl[[1]]
    }}else{
      return(gtable())
    }
  
  ## labeled or not
  lbs <- sapply(ggl, labeled)
  
  nms <- names(ggl)  
  N <- length(ggl)

  ## plot background 
  if(is.null(track.plot.color)){
    track.plot.color <- sapply(ggl, bgColor)
  }
  stopifnot(length(track.plot.color) != N || length(track.plot.color) != 1)
  
  if(length(track.plot.color) == 1){
      track.plot.color <- rep(track.plot.color, N)
  }


  grobs <- lapply(ggl, function(x){
    gg <- ggplotGrob(x)
    if(length(padding))
      gg <- gtable_add_padding(gg, padding)
    gg
  })

  
  
  addLabel <- function(grobs, nms, lbs,
                       label.bg.color =  "white",
                       label.bg.fill = "gray80",
                       label.text.color = "black",
                       label.text.cex = 1,                   
                       label.width = unit(2.5, "line"),
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

  grobs <- lapply(1:N, function(i){
    ## better figure out a better idea
    .col <- track.plot.color[i]    
    gt.temp <- grobs[[i]]$grobs[[1]]$children[[1]]$children$layout
    gt.temp$grobs[[1]] <- editGrob(gt.temp$grobs[[1]],
                                   gp = gpar(alpha = 0))
    grobs[[i]]$grobs[[1]]$children[[1]]$children$layout <- gt.temp
    grobs[[i]]$grobs[[1]] <- editGrob(grobs[[i]]$grobs[[1]], "bgColor", grep = TRUE, global = TRUE,
                       gp  = gpar(fill = .col, col = .col))
    grobs[[i]]
  })

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

    ## .h <- getAxisHeight(ggl[[N]], heights[N])
    ## heights <- unit.c(heights[1:(N-1)],  .h)
    tab <- gtable(widths, heights)
    for(i in 1:N){
      tab <- gtable_add_grob(tab, grobs[[i]], t = i, r = 1, l  = 1)
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
    g <- ggplotGrob(g)
  idx <- which("panel" == g$layout$name)
  idx
}

spaceAroundPanel <- function(g, type = c("t", "l", "b", "r")){
  if(inherits(g, "ggplot"))
    g <- ggplotGrob(g)
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
      g <- ggplotGrob(p)
    else
      g <- p
    g
  })
  if(dir == "row"){
    slst <- lapply(grobs, spaceAroundPanel, c("l", "r"))
    lmx <- do.call(max, lapply(slst, function(lst) lst$l))
    rmx <- do.call(max, lapply(slst, function(lst) lst$r))
    for(i in 1:length(grobs)){
      gt <- gtable(unit(1, "null"), unit(1, "null"), name = "panel.ori")
      grobs[[i]] <- gtable_add_cols(grobs[[i]], lmx - slst[[i]]$l ,
                                    pos = 0)
      grobs[[i]] <- gtable_add_cols(grobs[[i]], rmx - slst[[i]]$r, pos = -1)
      rect.grob <- rectGrob(gp = gpar(fill = NA, col = NA),
                            name = "bgColor")
      ## rect.grob <- rectGrob(gp = gpar(fill = NA, col = NA))
      all.grob <- grobTree(gTree(children = gList(rect.grob, grobs[[i]])))      
      grobs[[i]] <- gtable_add_grob(gt, all.grob, 1, 1)
    }
  }
  if(dir == "col"){
    
    slst <- lapply(grobs, spaceAroundPanel, c("t", "b"))    
    tmx <- do.call(max, lapply(slst, function(lst) lst$t))
    bmx <- do.call(max, lapply(slst, function(lst) lst$b))    
    for(i in 1:length(grobs)){
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
                 axis.text.x = element_text(vjust = 1),
                 axis.ticks = element_line(colour = "grey50"),                 
                 axis.ticks.y = element_blank(),
                 axis.title.x = element_text(),
                 axis.ticks.length = unit(0.15, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"))

  list(res,xlab(""))
}

ScalePlot <- function(x, color = "black", fill = NA){
  df <- data.frame(x =x, y = 1)
  p <- qplot(data = df, x = x, y = y, geom = "blank") +
    theme_onlyXaxis() + theme(aspect.ratio = 1/1000) +
      theme(panel.border = element_rect(color = color, fill = fill))
  mutable(p) <- FALSE
  p
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
                p <- qplot(x = range(cbs$breaks), y = y, geom = "line") +
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
                p <- qplot(x = range(cbs$breaks), y = y, geom = "line") +
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
                p <- qplot(x = range(cbs$breaks), y = y, geom = "line") +
                  geom_segment(data = cbs, aes(x = breaks, y = y, yend = yend, xend = breaks)) +
                   geom_text(data = cbs, aes(x = breaks, y = y.text, label = labels), vjust = 0.5,
                             size = 4) + coord_cartesian(ylim = ylim)+
                      theme_null() + theme(aspect.ratio = aspect.ratio)
                p
              })
  p
}


getAxisHeight <- function(p, base){
  .base <- as.numeric(base)
  p2 <- p + theme(axis.text.x = element_blank())
  h1 <- sum(ggplotGrob(p)$height)
  h2 <- sum(ggplotGrob(p2)$height)
  .h <- convertUnit(h1, "cm", value = TRUE)/convertUnit(h2, "cm", value = TRUE) * .base
  unit(.h, "null")
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

