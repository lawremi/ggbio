rangesCentric <- function(gr, which, id.name){
  ## first subset, need to give notice
  gr <- subsetByOverlaps(gr, which)
  which <- which[order(start(which))]
  gr <- gr[order(start(gr))]
  ## which <- subsetByOverlaps(which, gr)
  ## FIXME: give message here
  ## need to be exclusive
  which <- reduce(which)
  mx <- matchMatrix(findOverlaps(gr, which))
  values(gr)$.id.name <- NA
  values(gr)$.id.name[mx[,1]] <- mx[,2]
  if(!missing(id.name) && (id.name %in% colnames(values(which))))
    values(gr)$.id.name[mx[,1]] <- values(which)[mx[,2],id.name]
  values(which)$.id.name <- seq_len(length(which))
  ## cannot be putted into GRangesList, unequal, should be in a GenomicRangesList
  list(gr = gr, which = which)
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

  if(length(obj$layer)>1){
    l.res <- getLimitsFromLayer(obj$layer)
  }else{
    l.res <- NULL
  }
  
  list(xlim = c(min(c(l.res$xmin, x, xmin)),
         max(c(l.res$xmax, x, xmax, xend))),
       ylim = c(min(c(l.res$ymin, y, ymin)),
         max(c(l.res$ymax, y, ymax, yend))))
}

getLimitsFromLayer <- function(layers){
  lst <- lapply(layers, function(layer){
    if(length(layer$data)){
    if(!is.null(layer$mapping$x))
      x <- eval(layer$mapping$x, layer$data)
    else
      x <- NULL
    
    if(!is.null(layer$mapping$y))
      y <- eval(layer$mapping$y, layer$data)
    else
      y <- NULL
    
    if(!is.null(layer$mapping$xmin))
      xmin <- eval(layer$mapping$xmin, layer$data)
    else
      xmin <- NULL
    
    if(!is.null(layer$mapping$ymin))
      ymin <- eval(layer$mapping$ymin, layer$data)
    else
      ymin <- NULL
    
    if(!is.null(layer$mapping$xmax))
      xmax <- eval(layer$mapping$xmax, layer$data)
    else
      xmax <- NULL
    
    if(!is.null(layer$mapping$ymax))
      ymax <- eval(layer$mapping$ymax, layer$data)
    else
      ymax <- NULL
    
    if(!is.null(layer$mapping$xend))
      xend <- eval(layer$mapping$xend, layer$data)
    else
      xend <- NULL
    
    if(!is.null(layer$mapping$yend))
      yend <- eval(layer$mapping$yend, layer$data)
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




