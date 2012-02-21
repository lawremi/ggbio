setGeneric("geom_chevron", function(data, ...) starndGeneric("geom_chevron"))
setMethod("geom_chevron", "GenomicRanges",
           function(data, ..., group.name, offset = 0.1,
                         chevron.height = c(0.1, 0.8)){
  if(!".levels" %in% colnames(values(data))){
    if(!missing(group.name))
      data <- addSteppings(data, group.name = group.name)
    else
      data <- addSteppings(data)
  }
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  data.new <- breakGr(data)
  names(data.new) <- NULL
  df <- as.data.frame(data.new)
  getY <- function(n){
    switch(n,
           {
             y.offset <- 0
             yend.offset <- offset
             data.frame(y.offset = y.offset,
                        yend.offset = yend.offset)
            },
           {
             y.offset <- offset
             yend.offset <- 0
             data.frame(y.offset = y.offset,
                        yend.offset = yend.offset)
           })
  }
  getY2 <- function(df){
    res <- df[,offset]
    os <- rescale(res, chevron.height)
    lst <- lapply(1:nrow(df), function(i){
      n <- df[i,".bioviz.chevron"]
      switch(n,
             {
               y.offset <- 0
               yend.offset <- os[i]
               data.frame(y.offset = y.offset,
                          yend.offset = yend.offset)
             },
             {
               y.offset <- os[i]
               yend.offset <- 0
               data.frame(y.offset = y.offset,
                          yend.offset = yend.offset)
             })
    })
    do.call("rbind", lst)
  }
  
  if(!is.numeric(offset)){
    offset <- as.character(offset)
    if(offset %in% colnames(values(data)))    
      ydf <- getY2(df)
    else
      stop("offset must be a numeric value or one of the colnames")
  }else{
      ydf <- do.call("rbind", lapply(df$.bioviz.chevron, getY))
  }
  df <- cbind(df, ydf)
  args <- c(aes.lst, list(x = substitute(start),
                       xend = substitute(end),
                       y = substitute(.levels + y.offset),
                       yend = substitute(.levels + yend.offset)))
  geom_segment(data = df, do.call(aes, args))
})

## 
breakGr <- function(gr){
  mids <- start(gr) + width(gr)/2
  res1 <- res2 <- gr
  end(res1) <- mids
  values(res1)$.bioviz.chevron <- 1
  start(res2) <- mids
  values(res2)$.bioviz.chevron <- 2
  res <- c(res1, res2)
}

