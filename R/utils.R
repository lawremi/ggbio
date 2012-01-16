newDataAfterFacetByGr <- function(gr, which, id.name){
  gr <- sort(gr)
  which <- sort(which)
  ## suppose which is a GRanges
  res <- lapply(seq_len(length(which)),function(i){
    res <- subsetByOverlaps(gr, which[i])
    values(res)$.bioviz.facetid <- i
    res
  })
  do.call(c, res)
}

getLimits <- function(obj){
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  else
    x <- NULL
  
  if(!is.null(obj$mapping$y) && length(obj$data))
    y <- eval(obj$mapping$y, obj$data)
  else
    y <- NULL
  
  if(!is.null(obj$mapping$xmin) && length(obj$data))
    xmin <- eval(obj$mapping$xmin, obj$data)
  else
    xmin <- NULL
  
  if(!is.null(obj$mapping$ymin) && length(obj$data))
    ymin <- eval(obj$mapping$ymin, obj$data)
  else
    ymin <- NULL
  
  if(!is.null(obj$mapping$xmax) && length(obj$data))
    xmax <- eval(obj$mapping$xmax, obj$data)
  else
    xmax <- NULL
  
  if(!is.null(obj$mapping$ymax) && length(obj$data))
    ymax <- eval(obj$mapping$ymax, obj$data)
  else
    ymax <- NULL
  
  if(!is.null(obj$mapping$xend) && length(obj$data))
    xend <- eval(obj$mapping$xend, obj$data)
  else
    xend <- NULL
  
  if(!is.null(obj$mapping$yend) && length(obj$data))
    yend <- eval(obj$mapping$yend, obj$data)
  else
    yend <- NULL

  ## if(length(obj$layer)>1){
  l.res <- getLimitsFromLayer(obj)
  ## }else{
  ##   l.res <- NULL
  ## }
  
  list(xlim = c(min(c(l.res$xmin, x, xmin)),
         max(c(l.res$xmax, x, xmax, xend))),
       ylim = c(min(c(l.res$ymin, y, ymin)),
         max(c(l.res$ymax, y, ymax, yend))))
}

getLimitsFromLayer <- function(obj){
  layers <- obj$layer
  lst <- lapply(layers, function(layer){
    if(length(obj$data) | length(layer$data)){

    if(length(layer$data))
      dt <- layer$data
    else
      dt <- obj$data
      
    if(!is.null(layer$mapping$x))
      x <- eval(layer$mapping$x, dt)
    else
      x <- NULL
    
    if(!is.null(layer$mapping$y))
      y <- eval(layer$mapping$y, dt)
    else
      y <- NULL
    
    if(!is.null(layer$mapping$xmin))
      xmin <- eval(layer$mapping$xmin, dt)
    else
      xmin <- NULL
    
    if(!is.null(layer$mapping$ymin))
      ymin <- eval(layer$mapping$ymin, dt)
    else
      ymin <- NULL
    
    if(!is.null(layer$mapping$xmax))
      xmax <- eval(layer$mapping$xmax, dt)
    else
      xmax <- NULL
    
    if(!is.null(layer$mapping$ymax))
      ymax <- eval(layer$mapping$ymax, dt)
    else
      ymax <- NULL
    
    if(!is.null(layer$mapping$xend))
      xend <- eval(layer$mapping$xend, dt)
    else
      xend <- NULL
    
    if(!is.null(layer$mapping$yend))
      yend <- eval(layer$mapping$yend, dt)
    else
      yend <- NULL
    res <- data.frame(xmin = min(c(x, xmin)), xmax = max(c(x, xmax, xend)),
               ymin = min(c(y, ymin)), ymax = max(c(y, ymax, yend)))
  }else{
    res <- NULL
  }
  })
  lst <- lst[!is.null(lst)]
  res <- do.call("rbind", lst)
  res
}

getGap <- function(data, group.name){
  res <- split(data, seqnames(data))
  grl <- endoapply(res, function(dt){
    res <- split(dt, values(dt)[,group.name])
    ## browser()
    gps.lst <- lapply(res, function(x){
      if(length(x) > 1){
        ## if(values(x)$.levels  == 93) browser()
        gps <- gaps(ranges(x))
        if(length(gps)){
          seqs <- unique(as.character(seqnames(x)))
          ## print(seqs)
          ir <- gps
          ## print(ir)
          ## res
          gr <- GRanges(seqs, ir)
          ## tryes <- try(gr <- GRanges(, gps))
          ## if(inherits(tryes, "try-error")) browser()
          values(gr)$.levels <- unique(values(x)$.levels)
          gr
        }else{
          NULL
        }}else{
          NULL
        }
    })
    gps <- do.call("c", gps.lst)          #remove NULL
    gps <- do.call("c", unname(gps))
  })
  res <- unlist(grl)
  values(res)$type <- "gaps"
  res
}
## suppose we have freq?
getModelRange <- function(data, group.name){
  seqs <- unique(as.character(seqnames(data)))
  ir <- unlist(range(ranges(split(data, values(data)[,group.name]),
                            ignore.strand = TRUE)))
  ## freqs <- values(data)$freq[match(names(ir), values(data)[,group.name])]
  .lvs <- values(data)$.levels[match(names(ir), values(data)[,group.name])]
  ## with levels
  gr <- GRanges(seqs, ir, .levels = .lvs)
  values(gr)$.label <- names(gr)
  gr
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
    ytitles <- lapply(dots, function(.g) editGrob(getGrob(.g, 
        "axis.title.y.text", grep = TRUE), vp = NULL))
    ylabels <- lapply(dots, function(.g){
      grob.y.text <- getGrob(.g, "axis.text.y.text", grep = TRUE)
      if(!is.null(grob.y.text))
        editGrob(grob.y.text, vp = NULL)
      else
        zeroGrob()
    })
                      
    legends <- lapply(dots, function(.g) if (!is.null(.g$children$legends)) 
        editGrob(.g$children$legends, vp = NULL)
    else zeroGrob())

    ## get strips
    strips <- lapply(dots, function(.g) {
      cc <- .g$children$layout$children
      vstrips <- cc[grepl("^strip_v",names(cc))]
      ## assume all strips the same width/height, so just use the first one?
      if (length(vstrips)>0) 
        editGrob(vstrips[[1]],vp=NULL)
      else zeroGrob()
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
