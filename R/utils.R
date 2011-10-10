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
  res <- split(data, values(data)[,group.name])
  gps.lst <- lapply(res, function(x){
    gps <- gaps(ranges(x))
    if(length(gps)){
      gr <- GRanges(unique(as.character(seqnames(x))), gps)
      ## wait this request a .levels?
      values(gr)$.levels <- unique(values(x)$.levels)
      ## values(gr)$.model.group <- unique(values(x)$.model.group)
      ## if(".freq" %in% colnames(values(x)))
      ##   values(gr)$.freq <- unique(values(x)$.freq)
      gr
    }else{
      NULL
    }
  })
  gps <- do.call("c", gps.lst)          #remove NULL
  gps <- do.call("c", unname(gps))
  values(gps)$type <- "gaps"
  gps
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


