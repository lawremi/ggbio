## FIXME: add ..coverage.., and a new way
setGeneric("stat_slice", function(data, ...) standardGeneric("stat_slice"))

setMethod("stat_slice", "Rle", function(data, ...,
                                        xlab, ylab, main,
                                        na.rm = FALSE,
                                        geom = NULL,
                                        lower=-Inf, upper=Inf,
                                        includeLower=TRUE, includeUpper=TRUE,
                                        rangesOnly = FALSE,                                       
                                        type = c("viewSums","viewMins",
                                        "viewMaxs", "viewMeans")){



  type <- match.arg(type)

  if(is.null(geom))
    geom <- "segment"
  
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  
  if(!"x" %in% names(args.aes))
    args.aes$x <- substitute(x)
  
  if(!"y" %in% names(args.aes))
    args.aes$y <- substitute(y)

  args.non$geom <- geom  

    df <- switch(type,
                 viewMaxs = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- viewWhichMaxs(vs, na.rm = na.rm)                
                   y <- viewMaxs(vs, na.rm = na.rm)
                   data.frame(x = x, y = y)
                 },
                 viewMins = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- viewWhichMins(vs, na.rm = na.rm)                
                   y <- viewMins(vs, na.rm = na.rm)
                   data.frame(x = x, y = y)
                 },
                 viewSums = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- start(vs) + width(vs)/2
                   y <- viewSums(vs, na.rm = na.rm)
                   data.frame(x = x, y = y)                
                 },
                 viewMeans = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- start(vs) + width(vs)/2
                   y <- viewMeans(vs, na.rm = na.rm)
                   data.frame(x = x, y = y)                
                 })
  if(geom == "segment"){
    args.aes$x <- as.name("x")
    args.aes$xend <- as.name("x")
    args.aes$y <- 0
    args.aes$yend <- as.name("y")
  }
  if(geom == "rect"){
    args.aes$xmin <- start(vs)
    args.aes$xmax <- end(vs)
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
  }
  if(geom == "heatmap"){
    args.non$geom <- "rect"
    args.aes$xmin <- start(vs)
    args.aes$xmax <- end(vs)
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]    
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")    
  }
  if(geom == "bar"){
    args.non$geom <- "rect"
    args.aes$xmin <- start(vs)
    args.aes$xmax <- end(vs)
    args.aes$ymin <- 0
    args.aes$ymax <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]    
  }

  if(geom %in% c("bar", "rect")){
    if(!"color" %in% names(args.aes) &&
       !"color" %in% names(args.non)){
      args.non$color <- "grey20"
    }
  }
    
  
  args.non$data <- df
  aes.args <- do.call(aes, args.aes)
  res.args <- c(list(aes.args), args.non)
  p <- do.ggcall(ggplot2::stat_identity, res.args)
    if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("x")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("y")))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)  
  p
})


setMethod("stat_slice", "RleList", function(data, ...,
                                        xlab, ylab, main,
                                            indName = "sample",
                                        na.rm = FALSE,
                                        geom = NULL,
                                        lower=-Inf, upper=Inf,
                                        includeLower=TRUE, includeUpper=TRUE,
                                        rangesOnly = FALSE,                                       
                                        type = c("viewSums","viewMins",
                                        "viewMaxs", "viewMeans")){

  type <- match.arg(type)

  if(is.null(geom))
    geom <- "segment"
  args <- list(...)

  args.aes <- parseArgsForAes(args)
  
  if(!"x" %in% names(args.aes))
    args.aes$x <- as.name("x")
  
  if(!"y" %in% names(args.aes))
    args.aes$y <- as.name("y")
  
  args.non <- parseArgsForNonAes(args)
  args.non$geom <- geom

    df <- switch(type,
                 viewMaxs = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- viewWhichMaxs(vs)
                   y <- viewMaxs(vs)
                   xmin <- start(vs)
                   xmax <- end(vs)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms,
                              xmin = unlist(xmin), xmax = unlist(xmax))
                  },
                 viewMins = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- viewWhichMins(vs)                
                   y <- viewMins(vs)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms,
                              xmin = unlist(xmin), xmax = unlist(xmax))                
                 },
                 viewSums = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   
                   x <- start(vs) + width(vs)/2
                   xmin <- start(vs)
                   xmax <- end(vs)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   y <- viewSums(vs)
                   data.frame(x = unlist(x), y = unlist(y), listName = nms,
                              xmin = unlist(xmin), xmax = unlist(xmax))                
                 },
                 viewMeans = {
                   vs <- slice(data, upper = upper, lower = lower,
                               includeLower = includeLower,
                               includeUpper = includeUpper,
                               rangesOnly = rangesOnly)
                   x <- start(vs) + width(vs)/2
                   xmin <- start(vs)
                   xmax <- end(vs)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   y <- viewMeans(vs)
                   data.frame(x = unlist(x), y = unlist(y), listName = nms,
                              xmin = unlist(xmin), xmax = unlist(xmax))                
                 })



  colnames(df) <- c("x", "y", indName, "xmin", "xmax")

  if(is.null(names(x)))
    levels(df[, indName]) <- 1:length(x)
  else
    levels(df[, indName]) <- unique(names(x))

  facets <- as.formula(paste(indName, "~ .", sep = ""))

  args$facets <- facets
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- do.call(facet_grid, args.facets)


  if(geom == "segment"){
    args.aes$x <- as.name("x")
    args.aes$xend <- as.name("x")
    args.aes$y <- 0
    args.aes$yend <- as.name("y")
  }

  if(geom == "rect"){
    args.aes$xmin <- as.name("xmin")
    args.aes$xmax <- as.name("xmax")
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
  }
  if(geom == "heatmap"){
    args.non$geom <- "rect"
    args.aes$xmin <- as.name("xmin")
    args.aes$xmax <- as.name("xmax")
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]    
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")    
  }
  if(geom == "bar"){
    args.non$geom <- "rect"
    args.aes$xmin <- as.name("xmin")
    args.aes$xmax <- as.name("xmax")
    args.aes$ymin <- 0
    args.aes$ymax <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]    
  }

  if(geom %in% c("bar", "rect")){
    if(!"color" %in% names(args.aes) &&
       !"color" %in% names(args.non)){
      args.non$color <- "grey20"
    }
  }
  

  args.non$data <- df
  aes.args <- do.call(aes, args.aes)
  res.args <- c(list(aes.args), args.non)
  p <- do.ggcall(ggplot2::stat_identity, res.args)
    if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("x")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("y")))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- c(list(p), list(facet))
  p <- setStat(p)
  p
})
