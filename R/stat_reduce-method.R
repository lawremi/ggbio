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

  labels <- Labels(xlab, ylab, main, fallback = c(x = "", y = ""))
  p <- c(p, labels)
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

  labels <- Labels(xlab, ylab, main, fallback = c(x = "Position", y = ""))
  p <- c(p, labels)
  p <- setStat(p)
  p
})


setMethod("stat_reduce", "TxDbOREnsDb", function(data, ...){
  p <- geom_alignment(data, ..., stat = "reduce")
  p <- setStat(p)
  p
})




