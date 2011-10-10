geom_chevron <- function(data, ..., group.name, offset = 0.1,
                         chevron.height = c(0.1, 0.8)){
  if(!".levels" %in% colnames(values(data))){
    if(!missing(group.name))
      data <- addSteppings(data, group.name = group.name)
    else
      data <- addSteppings(data)
  }
  ## args <- as.list(match.call(expand.dots = TRUE)[-1])
  ## args <- args[!names(args) %in% c("data", "offset", "group.name")]
  args <- list(...)
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  data.new <- breakGr(data)
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
      n <- df[i,"chevron"]
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
      ydf <- do.call("rbind", lapply(df$chevron, getY))
  }
  df <- cbind(df, ydf)
  args <- c(aes.lst, list(x = substitute(start),
                       xend = substitute(end),
                       y = substitute(.levels + y.offset),
                       yend = substitute(.levels + yend.offset)))
  geom_segment(data = df, do.call(aes, args))
}

breakGr <- function(gr){
  lst <- lapply(1:length(gr), function(i){
    st <- start(gr[i])
    ed <- end(gr[i])
    mid <- mean(c(st, ed))
    chr <- seqnames(gr[i])
    res <- GRanges(chr, IRanges(start = c(st, mid), end = c(mid, ed)))
    values(res) <- rbind(elementMetadata(gr[i]), elementMetadata(gr[i]))
    values(res)$chevron <- c(1, 2)
    res
  })
  do.call("c", lst)
}

