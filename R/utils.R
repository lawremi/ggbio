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
  x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
  ## x
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  ## y
  if(!is.null(obj$mapping$y) && length(obj$data))
    y <- eval(obj$mapping$y, obj$data)
  
  if(!is.null(obj$mapping$xmin) && length(obj$data))
    xmin <- eval(obj$mapping$xmin, obj$data)

  
  if(!is.null(obj$mapping$ymin) && length(obj$data))
    ymin <- eval(obj$mapping$ymin, obj$data)
  
  if(!is.null(obj$mapping$xmax) && length(obj$data))
    xmax <- eval(obj$mapping$xmax, obj$data)
  
  if(!is.null(obj$mapping$ymax) && length(obj$data))
    ymax <- eval(obj$mapping$ymax, obj$data)
  
  if(!is.null(obj$mapping$xend) && length(obj$data))
    xend <- eval(obj$mapping$xend, obj$data)
  
  if(!is.null(obj$mapping$yend) && length(obj$data))
    yend <- eval(obj$mapping$yend, obj$data)
  else
    yend <- NULL

  ## if(length(obj$layer)>1){

  l.res <- getLimitsFromLayer(obj)
  ## }else{
  ##   l.res <- NULL
  ## }
  res <- list(xlim = c(min(c(l.res$xmin, x, xmin)),
                max(c(l.res$xmax, x, xmax, xend))),
              ylim = c(min(c(l.res$ymin, y, ymin)),
                max(c(l.res$ymax, y, ymax, yend))))
  if(length(obj$coordinates$limits$x) == 2)
    res$xlim <- obj$coordinates$limits$x
  
  if(length(obj$coordinates$limits$y) == 2)
    res$ylim <- obj$coordinates$limits$y

  res
  
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



