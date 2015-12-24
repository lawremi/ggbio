setGeneric("stat_bin", function(data, ...) standardGeneric("stat_bin"))
setMethod("stat_bin", "ANY", function(data, ...){
  ggplot2::stat_bin(data = data, ...)
})

setMethod("stat_bin", "Rle", function(data, ..., binwidth, nbin = 30,
                                      xlab, ylab, main, geom = c("bar", "heatmap"),
                                      type = c("viewSums","viewMins",
                                        "viewMaxs", "viewMeans")){

  geom <- match.arg(geom)
  type <- match.arg(type)
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(!"x" %in% names(args.aes))
    args.aes$x <- substitute(x)
  
  if(!"y" %in% names(args.aes))
    args.aes$y <- substitute(y)
  
  if(geom == "bar"){
    args.non$stat <- "identity"
  }
  if(missing(binwidth)){
    binwidth <- length(data)/nbin
    message("Default use binwidth: range/", nbin)
  }
    df <- switch(type,
                 viewMaxs = {
                   vs <- Views(data,
                                  start = seq(from = 1,
                                    to = length(data), by = binwidth),
                                  width = binwidth)
                   
                   x <- seq(from = 1,to = length(data), by = binwidth) + binwidth/2
                   y <- viewMaxs(vs)
                   data.frame(x = x, y = y)
                 },
                 viewMins = {
                   vs <- Views(data,
                                  start = seq(from = 1,
                                    to = length(data), by = binwidth),
                                  width = binwidth)
                   x <- seq(from = 1,to = length(data), by = binwidth) + binwidth/2
                   y <- viewMins(vs)
                   data.frame(x = x, y = y)
                 },
                 viewSums = {
                   vs <- Views(data,
                                  start = seq(from = 1,
                                    to = length(data), by = binwidth),
                                  width = binwidth)
                   x <- seq(from = 1,to = length(data), by = binwidth) + binwidth/2
                   y <- viewSums(vs)
                   data.frame(x = x, y = y)                
                 },
                 viewMeans = {
                   vs <- Views(data,
                                  start = seq(from = 1,
                                    to = length(data), by = binwidth),
                                  width = binwidth)
                   x <- seq(from = 1,to = length(data), by = binwidth) + binwidth/2
                   y <- viewMeans(vs)
                   data.frame(x = x, y = y)                
                 })

  args.non$data <- df
  if(geom == "bar"){
    aes.args <- do.call(aes, args.aes)    
    args.res <- c(list(aes.args),
                  args.non)
    p <- do.ggcall(geom_bar, args.res)
  }
  if(geom == "heatmap"){
    args.aes$xmin <- substitute(x-binwidth/2, list(binwidth = binwidth))
    args.aes$xmax <- substitute(x+binwidth/2, list(binwidth = binwidth))
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    aes.args <- do.call(aes, args.aes)        
    args.res <- c(list(aes.args),
                  args.non)
    p <- do.ggcall(geom_rect, args.res)    
  }
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
  p
})


setMethod("stat_bin", "RleList", function(data, ..., binwidth, nbin = 30,
                                      xlab, ylab, main,
                                      indName = "sample",
                                      geom = c("bar", "heatmap"),
                                      type = c("viewSums","viewMins",
                                        "viewMaxs", "viewMeans")){

  geom <- match.arg(geom)
  type <- match.arg(type)
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(!"x" %in% names(args.aes))
    args.aes$x <- substitute(x)
  
  if(!"y" %in% names(args.aes))
    args.aes$y <- substitute(y)
  
  if(geom == "bar"){
    args.non$stat <- "identity"
  }

  ## facets <- as.formula(paste(indName, "~ .", sep = ""))
  ## facet <- facet_grid(facets)
  
    mn <- mean(unlist(lapply(data, length)))
  if(missing(binwidth)){
      binwidth <- mn/nbin
      message("Default use binwidth: range/", nbin)
    }
    df <- switch(type,
                 viewMaxs = {
                   vs <- lapply(data, function(x) {
                     Views(x, start = seq(from = 1,
                                    to = length(x), by = binwidth),
                                  width = binwidth)})
                   x <- lapply(data, function(x) {
                     seq(from = 1, to = length(x), by = binwidth) + binwidth/2
                   })
                   y <- lapply(vs, viewMaxs)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms)
                 },
                 viewMins = {
                   vs <- lapply(data, function(x) {
                     Views(x, start = seq(from = 1,
                                    to = length(x), by = binwidth),
                                  width = binwidth)})
                   x <- lapply(data, function(x) {
                     seq(from = 1, to = length(x), by = binwidth) + binwidth/2
                   })
                   y <- lapply(vs, viewMins)

                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms)                
                 },
                 viewSums = {
                   vs <- lapply(data, function(x) {
                     Views(x, start = seq(from = 1,
                                    to = length(x), by = binwidth),
                                  width = binwidth)})
                   x <- lapply(data, function(x) {
                     seq(from = 1, to = length(x), by = binwidth) + binwidth/2
                   })
                   y <- lapply(vs, viewSums)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms)                
                 },
                 viewMeans = {
                   vs <- lapply(data, function(x) {
                     Views(x, start = seq(from = 1,
                                    to = length(x), by = binwidth),
                                  width = binwidth)})
                   x <- lapply(data, function(x) {
                     seq(from = 1, to = length(x), by = binwidth) + binwidth/2
                   })
                   y <- lapply(vs, viewMeans)
                   if(is.null(names(x)))
                     nms <- rep(1:length(x), times = elementLengths(x))
                   else
                     nms <- rep(names(x), times = elementLengths(x))
                   data.frame(x = unlist(x), y = unlist(y), listName = nms)                
                 })
  colnames(df) <- c("x", "y", indName)
  
  if(is.null(names(x)))
    levels(df[, indName]) <- 1:length(x)
  else
    levels(df[, indName]) <- unique(names(x))

  facets <- as.formula(paste(indName, "~ .", sep = ""))

  args$facets <- facets
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- do.call(facet_grid, args.facets)
  
    args.non$data <- df
  if(geom == "bar"){
    aes.args <- do.call(aes, args.aes)    
    args.res <- c(list(aes.args),
                  args.non)
    p <- do.ggcall(geom_bar, args.res)
  }
  if(geom == "heatmap"){
    args.aes$xmin <- substitute(x-binwidth/2, list(binwidth = binwidth))
    args.aes$xmax <- substitute(x+binwidth/2, list(binwidth = binwidth))
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    aes.args <- do.call(aes, args.aes)        
    args.res <- c(list(aes.args),
                  args.non)
    p <- do.ggcall(geom_rect, args.res)    
  }
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
