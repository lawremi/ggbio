tracks <- function(...,
                   heights,
                   xlim, ylim,                   
                   theme = NULL,
                   legend = FALSE,     #
                   xlab = NULL,
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
    ## take the smallest range
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
  ## s <- scale_x_continuous(limits = xlim)
  s <- coord_cartesian(xlim = xlim, wise = TRUE)
  ## need fix legend
    N <- length(grobs)
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(i %in% which(xlim.change))
                      grobs[[i]] <- grobs[[i]] + s
                    if(!is.null(theme))
                      grobs[[i]] <- grobs[[i]] + theme
                    ## margin
                    if(!is.null(track.skip))
                      if(i < N){
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
align.plots <- function (..., vertical = TRUE,
                          heights = unit(rep(1, nrow), "null")) 
{
  if (!vertical) stop("only vertical alignment implemented")

    dots0 <- list(...)
    nrow <- length(dots0)

    legend.pos <- lapply(dots0,
                       function(x) {
                         if (is.null(x$options$legend.pos)) "right"
                         else x$options$legend.pos })
    dots <- lapply(dots0, ggplotGrob)

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
                      
    legends <- lapply(dots, function(.g) if (!is.null(.g$children$legends)) 
        editGrob(.g$children$legends, vp = NULL)
    else ggplot2:::zeroGrob())

    ## get strips
    strips <- lapply(dots, function(.g) {
      cc <- .g$children$layout$children
      vstrips <- cc[grepl("^strip_v",names(cc))]
      ## assume all strips the same width/height, so just use the first one?
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
      grobWidth(g) + if (lp=="none") unit(0,"lines") else unit(0.5,"lines") + grobWidth(s)
    },
                           legends,legend.pos,strips,
                           SIMPLIFY=FALSE)
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
