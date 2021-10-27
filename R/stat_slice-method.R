## FIXME: add ..coverage.., and a new way
setGeneric("stat_slice", function(data, ...) standardGeneric("stat_slice"))

get_coordinate_by_type <- function(type, vs, na.rm, islist = FALSE) {
   values <- switch(type,
            viewMaxs = list(x = viewWhichMaxs(vs, na.rm = na.rm), y = viewMaxs(vs, na.rm = na.rm)),
            viewMins = list(x = viewWhichMins(vs, na.rm = na.rm), y = viewMins(vs, na.rm = na.rm)),
            viewSums = list(x = start(vs) + width(vs)/2, y = viewSums(vs, na.rm = na.rm)),
            viewMeans = list(x = start(vs) + width(vs)/2, y = viewMeans(vs, na.rm = na.rm)))
    if (islist) {
        values[["xmin"]] <- start(vs)
        values[["xmax"]] <- end(vs)
    }
    values
}

map_coordinate_by_geom <- function(geom, args.aes, args.non, vs, islist = FALSE) {
    values <- list()
    if (islist) {
        values[["xmin"]] <- as.name("xmin")
        values[["xmax"]] <- as.name("xmax")
    } else {
        values[["xmin"]] <- start(vs)
        values[["xmax"]] <- end(vs)
    }

    if (geom == "segment") {
        args.aes$x <- as.name("x")
        args.aes$xend <- as.name("x")
        args.aes$y <- 0
        args.aes$yend <- as.name("y")
    }
    if (geom == "rect") {
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- 5
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    }
    if (geom == "heatmap") {
        args.non$geom <- "rect"
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- 5
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
        args.aes$color <- as.name("y")
        args.aes$fill <- as.name("y")
    }
    if (geom == "bar") {
        args.non$geom <- "rect"
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- as.name("y")
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    }
    if (geom %in% c("bar", "rect")) {
        if(!"color" %in% names(args.aes) &&
        !"color" %in% names(args.non) &&
        !"colour" %in% names(args.aes) &&
        !"colour" %in% names(args.non)) {
            args.non$color <- "grey20"
        }
    }
    list(args.aes = args.aes, args.non = args.non)
}

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
  vs <- slice(data, upper = upper, lower = lower, includeLower = includeLower,
              includeUpper = includeUpper, rangesOnly = rangesOnly)
  values <- get_coordinate_by_type(type, vs, na.rm)
  df <- data.frame(x = values[["x"]], y = values[["y"]])
  values <- map_coordinate_by_geom(geom, args.aes, args.non, vs)
  args.aes <- values[["args.aes"]]
  args.non <- values[["args.non"]]
  args.non$data <- df
  aes.args <- do.call(aes, args.aes)
  res.args <- c(list(aes.args), args.non)
  p <- do.call(ggplot2::stat_identity, res.args)
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
  vs <- slice(data, upper = upper, lower = lower, includeLower = includeLower,
              includeUpper = includeUpper, rangesOnly = rangesOnly)
  values <- get_coordinate_by_type(type, vs, na.rm, islist = TRUE)
  x <- values[["x"]]
  if (is.null(names(x)))
      nms <- rep(1:length(x), times = elementNROWS(x))
  else
      nms <- rep(names(x), times = elementNROWS(x))
  df <- data.frame(x = unlist(values[["x"]]), y = unlist(values[["y"]]), listName = nms,
                    xmin = unlist(values[["xmin"]]), xmax = unlist(values[["xmax"]]))
  colnames(df) <- c("x", "y", indName, "xmin", "xmax")

  if(is.null(names(x)))
    levels(df[, indName]) <- 1:length(x)
  else
    levels(df[, indName]) <- unique(names(x))

  facets <- as.formula(paste(indName, "~ .", sep = ""))

  args$facets <- facets
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- do.call(facet_grid, args.facets)

  values <- map_coordinate_by_geom(geom, args.aes, args.non, vs, islist = TRUE)
  args.aes <- values[["args.aes"]]
  args.non <- values[["args.non"]]

  args.non$data <- df
  aes.args <- do.call(aes, args.aes)
  res.args <- c(list(aes.args), args.non)
  p <- do.call(ggplot2::stat_identity, res.args)
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
