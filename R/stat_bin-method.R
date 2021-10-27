setGeneric("stat_bin", function(data, ...) standardGeneric("stat_bin"))
setMethod("stat_bin", "ANY", function(data, ...){
  ggplot2::stat_bin(data = data, ...)
})


stat_bin_geom_bar <- function(args.aes, args.non) {
    args.non$stat <- "identity"
    aes.args <- do.call(aes, args.aes)
    p <- do.call(geom_bar, c(list(aes.args), args.non))
}

stat_bin_geom_heatmap <- function(args.aes, args.non, binwidth) {
    args.aes$xmin <- substitute(x - binwidth / 2, list(binwidth = binwidth))
    args.aes$xmax <- substitute(x + binwidth / 2, list(binwidth = binwidth))
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    aes.args <- do.call(aes, args.aes)
    p <- do.call(geom_rect, c(list(aes.args), args.non))
}

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
  vs <- Views(data, start = seq(from = 1, to = length(data), by = binwidth),
              width = binwidth)
  x <- seq(from = 1, to = length(data), by = binwidth) + binwidth/2
  y <- switch(type, viewMaxs = viewMaxs(vs),
                      viewMins = viewMins(vs), 
                      viewSums =  viewSums(vs),
                      viewMeans = viewMeans(vs))
  args.non$data <- data.frame(x = x, y = y)
  if(geom == "bar")
    p <- stat_bin_geom_bar(args.aes, args.non)
  if(geom == "heatmap")
    p <- stat_bin_geom_heatmap(args.aes, args.non, binwidth)
  labels <- Labels(xlab, ylab, main, fallback = c(x = "x", y = "y"))
  p <- c(p, labels)
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
  vs <- lapply(data, function(x) {
      Views(x, start = seq(from = 1, to = length(x), by = binwidth),
            width = binwidth)
  })
  x <- lapply(data, function(x) {
      seq(from = 1, to = length(x), by = binwidth) + binwidth/2
  })
  y <- switch(type, viewMaxs = lapply(vs, viewMaxs),
                    viewMins = lapply(vs, viewMins),
                    viewSums = lapply(vs, viewSums),
                    viewMeans = lapply(vs, viewMeans))
  if(is.null(names(x)))
      nms <- rep(1:length(x), times = elementNROWS(x))
  else
      nms <- rep(names(x), times = elementNROWS(x))
  df <- data.frame(x = unlist(x), y = unlist(y), listName = nms)
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
  if(geom == "bar")
    p <- stat_bin_geom_bar(args.aes, args.non)
  if(geom == "heatmap")
    p <- stat_bin_geom_heatmap(args.aes, args.non, binwidth)
  labels <- Labels(xlab, ylab, main, fallback = c(x = "x", y = "y"))
  p <- c(p, labels)
  p <- c(list(p), list(facet))
  p <- setStat(p)
  p
})
