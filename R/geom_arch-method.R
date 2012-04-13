setGeneric("geom_arch", function(data, ...) standardGeneric("geom_arch"))

setMethod("geom_arch", "data.frame", function(data, ..., 
                                              n = 25, max.height = 10){


  args <- list(...)

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)

  
  ## check required argument
  if(!all(c("x", "xend") %in% names(args.aes)))
    stop("x, xend, are requried in aes(), need to be passed into geom_arch()")
  startX <- eval(args.aes$x, data)
  endX <- eval(args.aes$xend, data)
  if("height" %in% names(args.aes)){
  if(!is.numeric(args.aes$height)){
    h <- eval(args.aes$height, data)
  }else{
    if(length(args.aes$height) == 1)
      h <- rep(args.aes$height, length(startX))
    else
      stop("unequal length of heights specified")
  }}else{
     h <- rep(max.height, length(startX))
  }
  if("y" %in% names(args.aes))
    y <- eval(args.aes$y, data)
  else
    y <- rep(0, length(startX))
  args.aes2 <- args.aes[!(names(args.aes) %in% c("x", "y", "group",
                                              "hjust", "xend", "yend"))]  
  xx<-c()
  yy<-c()
  for(i in 1:n){
    ang<-i*pi/(2*n)
    xx[i]<-cos(ang)
    yy[i]<-sin(ang)
  }
  ##takes the quarter of the curve calculated, flips a copy over the y axis
  ##reduces time spent in for loop
  xx<-c(1,xx,rev(-xx),-1)
  yy<-c(0,yy,rev(yy), 0)
  ##SETS UP DATAFRAME TO KEEP TRACK OF ALL POINTS TO DRAW ALL ARCHES
  junc <- rep(seq_along(startX), each = length(xx))
  startX <- rep(startX, each = length(xx))
  endX <- rep(endX, each = length(xx))
  h <- rep(h, each = length(xx))
  y <- rep(y, each = length(xx))
  jump <- abs(endX - startX)
  jumpAdj <- if (length(jump)) max(jump) / max(abs(h)) else NA
  apoint <- data.frame(xx = xx * (abs(startX - endX) / 2) + (startX + endX) / 2,
                       yy = yy * h + y, junc,
                       s = ((abs(h) - jump / jumpAdj)) /
                           if (length(jump)) max(jump) else NA)
  data$junc <- seq_len(nrow(data))
  apoint <- merge(apoint, data, by = "junc")
  args.aes <- list(x = as.name("xx"),
                  y = as.name("yy"),
                  group = as.name("junc"))
  
  aesres <- do.call(aes, c(args.aes, args.aes2))
  if(nrow(apoint)){
    reslst <- c(list(data = apoint), list(aesres),args.non)
    p <- do.call(geom_line, reslst)
  }else{
    p <- NULL
  }
  p
})

## that means span the range of two end 
setMethod("geom_arch", "GRanges", function(data, ...,
                                           xlab, ylab, main,
                                           facets = NULL, rect.height = 0,
                                              n = 25, max.height = 10
                                              ){

  args <- list(...)  
  args$facets <- facets
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)

  ## note rect.height = 0.4 is default cross ggbio
  ## need to make sure they are connected by two nearest point of rectangle
  df <- as.data.frame(data)
  if("height" %in% names(args.aes))
    signs <- sign(eval(args.aes$height, df))
  else
    signs <- 1
  args.aes$x <- substitute(start)
  args.aes$xend <- substitute(end)
  if("y" %in% names(args.aes)){
    y <- eval(args.aes$y, df)
    df[,as.character(args.aes$y)] <- df[,as.character(args.aes$y)] + rect.height * signs
  }else{
    df$.y <- rep(0, nrow(df)) + rect.height * signs
    args.aes$y <- substitute(.y)
  }
  if(nrow(df)){
    args.res <- c(list(data = df),
                  args.non,
                  list(do.call(aes, args.aes)))
    p <- do.call(geom_arch, args.res)
    p <- c(list(p) , list(facet))
  }else{
    p <- NULL
  }
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))
  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  p
})

## setMethod("geom_arch", "GRangesList", function(data, ..., rect.height = 0.4,
##                                               n = 25, max.height = 10
##                                               ){

##   args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
##   args.aes <- parseArgsForAes(args)
##   args.non <- parseArgsForNonAes(args)
##   args.non <- args.non[!names(args.non) %in% c("data", "n", "rect.height",
##                                                "stat")]            
##   args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
##   facet <- .buildFacetsFromArgs(data, args.facets)
  
##   ## we require GRangesList elementLengths is always 2 to sepcify two end point now
##   if(!all(elementLengths(data) == 2))
##     stop("Element lengths of the data must be of 2 now.")
##   gr <- stack(data)
##   DF <- as.data.frame(values(data))
##   lst <- lapply(seq_len(nrow(DF)),function(i){
##     rbind(DF[i,,drop = FALSE],DF[i,,drop = FALSE])
##   })
##   df <- do.call(rbind,lst)
##   values(gr) <- cbind(as.data.frame(values(gr)), df)
##   args.new <- list(data = gr, n = n,
##                    max.height = max.height, list(do.call(args.aes)))
##   do.call(geom_arch, args.new)  
## })


