setGeneric("geom_bar", function(data, ...) standardGeneric("geom_bar"))

setMethod("geom_bar", "ANY", function(data, ...){
  ggplot2::geom_bar(data  = data, ...)
})

## alignment should be convenient toggle with chevron...
setMethod("geom_bar", "GRanges", function(data,..., xlab, ylab, main){

  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  facet <- build_facet(data, args)
  if(length(data)){
    if(!"y" %in% names(args.aes)){
      if("score" %in% colnames(values(data))){
        message("use score as y by default")
        args.aes$y <- as.name("score")
      }else{
        stop("missing y values in aes(), or please provide a column named 'score'")
      }
    }
    .y <- quo_name(args.aes$y)
    if (missing(ylab))
      ylab <- .y
    args.aes <- remove_args(args.aes, "y")
    args.aes$xmin <- as.name("start")
    args.aes$xmax <- as.name("end")
    args.aes$ymin <- 0
    args.aes$ymax <- as.name(.y)
    aes.res <- do.call(aes, args.aes)
    p <- list(do.call(geom_rect, c(list(data = mold(data)), list(aes.res), args.non)))
  }else{
    p <- NULL
  }
  p <- c(p, list(facet))
  labels <- Labels(xlab, ylab, main, fallback = c(x = ""))
  p <- c(p, labels)
  p
})
