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
  startX <- eval(aes.lst$x, data)
  endX <- eval(aes.lst$xend, data)
  h <- eval(aes.lst$height, data)
  y <- eval(aes.lst$y, data)
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

## suppose element contains only two??
## setMethod("geom_arch", "GRangesList", function(data, ..., n = 25){
##   ## each element of list contains two exons
##   df <- as.data.frame(data)
##   ## geom_line(data = df, aes(x = start,))
  
## })


