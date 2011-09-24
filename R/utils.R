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
  if(is.null(obj$mapping)){
    x <- eval(obj$layers[[1]]$mapping$x, obj$data)
    y <- eval(obj$layers[[1]]$mapping$y, obj$data)  
  }else{
    x <- eval(obj$mapping$x, obj$data)
    y <- eval(obj$mapping$y, obj$data)  
  }
  if(is.null(obj$coordinates$limits$x)){
    if(!is.null(x))
      limx <- c(min(x), xmax = max(x))
    else
      limx <- c(min(obj$data$start), xmax = max(obj$data$end))
  }else{
    limx <- obj$coordinates$limits$x
  }

  if(is.null(obj$coordinates$limits$y)){
    if(!is.null(y))
      limy <- c(min(y), xmax = max(y))
    else
      limy <- c(min(obj$data$start), xmax = max(obj$data$end))
  }else{
    limx <- obj$coordinates$limits$y
  }
  names(limx) <- NULL
  names(limy) <- NULL
  list(xlim = limx, ylim = limy)
}





