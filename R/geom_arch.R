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
  if(!all(c("x", "xend", "height") %in% names(aes.lst)))
    stop("x, xend, height are requried in aes(), need to be passed into geom_arch()")
  startX <- eval(aes.lst$x, data)
  endX <- eval(aes.lst$xend, data)
  if(!is.numeric(aes.lst$height)){
    h <- eval(aes.lst$height, data)
  }else{
    if(length(aes.lst$height) == 1)
      h <- rep(aes.lst$height, length(startX))
    else
      stop("unequal length of heights specified")
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
  signs <- sign(eval(aes.lst$height, df))
  df[,as.character(aes.lst$y)] <- df[,as.character(aes.lst$y)] + rect.height * signs
  aes.lst$x <- as.name(start)
  aes.lst$xend <- as.name(xend)
  args.new <- c(list(data = df, n = n,
                   max.height = max.height), aes.lst)
  do.call(geom_arch, args.new)
})

setMethod("geom_arch", "GRangesList", function(data, ..., rect.height = 0.4,
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
  
})


## df <- data.frame(x = seq(1, 100, by = 2),
##                  y = 1:50,
##                  xend = seq(1, 100, by = 2) + rnorm(50, 10),
##                  yend =0 , size = rnorm(50))

## ggplot() + geom_arch(data = df, aes(x = x, xend = xend, height = size, y = y, size = size))
## ggplot() + geom_arch(data = df, aes(x = x, xend = xend, height = size, size = size))
## ggplot() + geom_arch(data = df, aes(x = x, xend = xend, size = size))




