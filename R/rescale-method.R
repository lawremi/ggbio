## ======================================================================
##        For "Granges"
## ======================================================================
setMethod("rescale", signature(x = "numeric"), function(x, to = c(0, 1),
                                 from = range(x, na.rm = TRUE)){
  scales::rescale(x, to = to , from = from)
})

setMethod("rescale", "gg", function(x, xlim, ylim, sx = 1, sy = 1){
  if(!missing(xlim) & sx != 1)
    stop("You can only rescale by one of xlim or sx")
  if(!missing(ylim) & sy != 1)
    stop("You can only rescale by one of ylim or sy")
  if(!missing(xlim))
    res <- x + coord_cartesian(xlim = xlim)
    ## res <- x + scale_x_continuous(limits = xlim)
  if(!missing(ylim))
    res <- x + coord_cartesian(ylim = ylim)    
    ## res <- x + scale_x_continuous(limits = ylim)
  if(sx != 1){
    xlim <- .getLimits(x)$xlim
    xlim.mean <- mean(xlim)
    extra.new <- diff(xlim) * sx/2
    xlim <- c(xlim.mean - extra.new, xlim.mean + extra.new)
    res <- x + coord_cartesian(xlim = xlim)
  }
  if(sy != 1){
    ylim <- .getLimits(x)$ylim
    ylim.mean <- mean(ylim)
    extra.new <- diff(ylim) * sy/2
    ylim <- c(ylim.mean - extra.new, ylim.mean + extra.new)
    res <- x + coord_cartesian(ylim = ylim)
  }
  res
})
