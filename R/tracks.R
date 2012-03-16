tracks <- function(..., heights, xlim, ylim, xlab = NULL,                
                   theme = NULL, legend = FALSE,     #
                   track.skip = -1,
                   xlim.change = rep(TRUE, length(list(...)))){

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
  
  ## need fix legend
    N <- length(grobs)
    lst <- lapply(seq_len(N),
                  function(i) {
                    ylim <- getLimits(grobs[[i]])$ylim
                    s <- coord_cartesian(xlim = xlim, ylim = ylim, wise = TRUE)
                    if(i %in% which(xlim.change))
                      grobs[[i]] <- grobs[[i]] + s
                    if(!is.null(theme))
                      grobs[[i]] <- grobs[[i]] + theme
                    ## margin
                    if(!is.null(track.skip))
                      if(i < N){  s <- coord_cartesian(xlim = xlim, wise = TRUE)
                        grobs[[i]] <- grobs[[i]] +
                          opts(plot.margin = unit(c(1, 1, track.skip, 0.5), "lines"))
                      }
                    ## xlab
                    if(i %in% 1:(N-1)){
                      grobs[[i]] <- grobs[[i]] + 
                          opts(axis.text.x = theme_blank()) + xlab("") 
                    }else{
                      if(!is.null(xlab))
                        grobs[[i]] <- grobs[[i]] + xlab(xlab)
                      grobs[[i]] <- grobs[[i]] +
                        opts(axis.title.x = theme_text(vjust = 0))
                             ## panel.margin = unit(5, "lines"))
                    }
                    
                    grobs[[i]] 
                  })
  
  res <- do.call(align.plots, c(lst, list(heights = heights)))
  res
}



## this function is from ggExtra
## a little tweak to check the axis.text.y.text

##
## TODO: adust due to left/right legend
align.plots <- function (..., vertical = TRUE,
                          heights = unit(rep(1, nrow), "null")) 
{

  if (!vertical) stop("only vertical alignment implemented")
    dots0 <- list(...)
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
    ## ggplot_gtable(ggplot_build(dots0[[1]]))$layout
    if(length(idx)){
      if(l.pos == "right"){
        gt$widths[gt$layout[idx,"r"]]
      }else{
        warning("only support right legend now")
        grobWidth(ggplot2:::zeroGrob())
      }
      ## editGrob(gt$grobs[[idx]], vp = NULL)
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
