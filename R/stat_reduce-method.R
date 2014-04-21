setGeneric("stat_reduce", function(data, ...) standardGeneric("stat_reduce"))

setMethod("stat_reduce", "GRanges", function(data, ...,
                                               xlab, ylab, main,
                                               drop.empty.ranges = FALSE,
                                               min.gapwidth = 1L,
                                               facets = NULL,
                                               geom = NULL){


  data <- reduce(data, drop.empty.ranges = drop.empty.ranges,
                 min.gapwidth = min.gapwidth)
  args <- list(...)
  args$facets <- facets
  args$geom <- geom
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- data
  aes.res <- do.call(aes, args.aes)
  args.res <- c(list(aes.res), args.non)
  p <- list(do.call(stat_stepping, args.res))

  if(missing(xlab))
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))


  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("")))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)
p
})

setMethod("stat_reduce", "IRanges", function(data, ...,
                                               xlab, ylab, main,
                                               drop.empty.ranges = FALSE,
                                               min.gapwidth = 1L,
                                             with.inframe.attrib=FALSE,
                                               facets = NULL,
                                               geom = NULL){


  data <- reduce(data, drop.empty.ranges = drop.empty.ranges,
                 min.gapwidth = min.gapwidth,
                 with.inframe.attrib = with.inframe.attrib)
  df <- values(data)
  values(data) <- NULL
  data <- GRanges("chr_non", data)
  values(data) <- df

  args <- list(...)
  args$facets <- facets
  args$geom <- geom
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non$data <- data
  aes.res <- do.call(aes, args.aes)
  args.res <- c(list(aes.res), args.non)
  p <- list(do.call(stat_stepping, args.res))

  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Position")))
  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("")))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
    p <- setStat(p)
p
})


setMethod("stat_reduce", "TranscriptDb", function(data, ...){
  p <- geom_alignment(data, ..., stat = "reduce")
  p <- setStat(p)
  p
})





