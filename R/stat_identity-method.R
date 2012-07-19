setGeneric("stat_identity", function(data, ...) standardGeneric("stat_identity"))
setMethod("stat_identity", "data.frame", function(data, ...){
  ggplot2::stat_identity(data = data, ...)
})

setMethod("stat_identity", "GRanges", function(data, ..., geom = NULL){
  args <- list(...)
  gr.geoms <- c("chevron", "arrow", "arrowrect", "segment", "rect", "alignment")
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)
  if(is.null(geom))
    geom <- "segment"
  if(!geom %in% gr.geoms){
    args$geom <- geom
    data <- fortify(data)
    args$data <- data
    p <- do.call(ggplot2::stat_identity, args)
  }else{
    .geom.fun <- getGeomFun(geom)
    args$stat <- "identity"
    args$data <- data
    p <- do.call(.geom.fun, args)
  }
  p <- c(list(p), list(facet))
  p
})


setMethod("stat_identity", "Rle", function(data, ...,
                                           xlab, ylab, main, geom = NULL){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(is.null(geom))
    geom <- "line"
  x <- 1:length(data)                
  y <- as.numeric(data)
  df <- data.frame(x = x, y = y)
  args.non$geom <- geom
  args.non$data <- df
  args.aes <- list(x = substitute(x),
                   y = substitute(y))
  p <- do.call(ggplot2::stat_identity, c(args.non, list(do.call(aes, args.aes))))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("x")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("y")))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  p
})


setMethod("stat_identity", "RleList", function(data, ...,
                                               xlab, ylab, main,
                                               geom = NULL, indName = "sample"){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(is.null(geom))
    geom <- "line"
  
  x <- do.call(c,lapply(elementLengths(data),function(n) 1:n))
  y <- as.numeric(unlist(data))
  if(is.null(names(data)))
    nms <- rep(1:length(data), times = elementLengths(data))
  else
    nms <- rep(names(data), times = elementLengths(data))

  df <- data.frame(x = x, y = y, z = nms)
  colnames(df) <- c("x", "y", indName)

  facets <- as.formula(paste(indName, "~ .", sep = ""))
  facet <- facet_grid(facets)
  
  args.non$geom <- geom
  args.non$data <- df
  
    args.aes <- list(x = substitute(x),
                     y = substitute(y))
    p <- do.call(ggplot2::stat_identity, c(args.non, list(do.call(aes, args.aes))))

  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("x")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("y")))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  c(list(p), list(facet))
})


