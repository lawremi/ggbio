setGeneric("geom_arch", function(data, ...) standardGeneric("geom_arch"))

setMethod("geom_arch", "data.frame", function(data, ...,
                                              n = 25, max.height = 10){
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  ## check required argument
  if(!all(c("x", "xend") %in% names(aes.lst)))
    stop("x, xend, height are requried in aes(), need to be passed into geom_arch()")
  startX <- eval(aes.lst$x, data)
  endX <- eval(aes.lst$xend, data)
  if("height" %in% names(aes.lst)){
  if(!is.numeric(aes.lst$height)){
    h <- eval(aes.lst$height, data)
  }else{
    if(length(aes.lst$height) == 1)
      h <- rep(aes.lst$height, length(startX))
    else
      stop("unequal length of heights specified")
  }}else{
     h <- rep(max.height/2, length(startX))
  }
  if("y" %in% names(aes.lst))
    y <- eval(aes.lst$y, data)
  else
    y <- rep(0, length(startX))
  aes.lst2 <- aes.lst[!(names(aes.lst) %in% c("x", "y", "group",
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
  apoint<-data.frame()
  jump<-abs(endX-startX)
  jumpAdj <- max(jump)/max(abs(h))
  for(i in 1:length(startX)){
    temp<-data.frame(xx = xx*(abs(startX[i]-endX[i])/2)+(startX[i]+endX[i])/2,
                     yy=yy*h[i]+y[i],
                     junc = i,
                     s=((abs(h[i])-jump[i]/jumpAdj))/max(jump))
    apoint<-rbind(apoint,temp)	
  }
  data$junc <- seq_len(nrow(data))
  apoint <- merge(apoint, data, by = "junc")
  aes.lst <- list(x = as.name("xx"),
                  y = as.name("yy"),
                  group = as.name("junc"))
  
  aesres <- do.call(aes, c(aes.lst, aes.lst2))
  reslst <- list(data = apoint, aesres)
  do.call(geom_line, reslst)
})


## that means span the range of two end 
setMethod("geom_arch", "GRanges", function(data, ..., rect.height = 0.4,
                                              n = 25, max.height = 10
                                              ){
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  ## note rect.height = 0.4 is default cross ggbio
  ## need to make sure they are connected by two nearest point of rectangle
  df <- as.data.frame(data)
  if("height" %in% names(aes.lst))
    signs <- sign(eval(aes.lst$height, df))
  else
    signs <- 1
  aes.lst$x <- substitute(start)
  aes.lst$xend <- substitute(end)
  if("y" %in% names(aes.lst)){
    y <- eval(aes.lst$y, data)
    df[,as.character(aes.lst$y)] <- df[,as.character(aes.lst$y)] + rect.height * signs
  }else{
    df$.y <- rep(0, nrow(df)) + rect.height * signs
    aes.lst$y <- substitute(.y)
  }
  args.new <- list(data = df, n = n,
                   max.height = max.height,aes.lst)
  do.call(geom_arch, args.new)
})

setMethod("geom_arch", "GRangesList", function(data, ..., rect.height = 0.4,
                                              n = 25, max.height = 10
                                              ){
  ## we require GRangesList elementLengths is always 2 to sepcify two end point now
  if(!all(elementLengths(data) == 2))
    stop("Element lengths of the data must be of 2 now.")
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  gr <- stack(data)
  DF <- as.data.frame(values(data))
  lst <- lapply(seq_len(nrow(DF)),function(i){
    rbind(DF[i,,drop = FALSE],DF[i,,drop = FALSE])
  })
  df <- do.call(rbind,lst)
  values(gr) <- cbind(as.data.frame(values(gr)), df)
  args.new <- list(data = gr, n = n,
                   max.height = max.height,aes.lst)
  do.call(geom_arch, args.new)  
})


