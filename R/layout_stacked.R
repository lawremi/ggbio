geom_hotregion <- function(data,...){
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  args <- args[!names(args) %in% "data"]
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  args <- c(aes.lst, list(xmin = substitute(start),
                       xmax = substitute(end),
                       ymin = 0,
                       ymax = 10))

  df <- as.data.frame(data)
  ## this is a hack to make sure geom_rect can show segments too
  ## need color and fill when it's just segment
  if(any(c("colour", "fill") %in% names(args))){
    if(!all(c("colour", "fill") %in% names(args))){
      idx <- which(c("colour", "fill") %in% names(args))
      known <- c("colour", "fill")[idx]
      unknown <- c("colour", "fill")[-idx]
      args[[unknown]] <- args[[known]]
    }
    geom_rect(data = df, do.call(aes, args))
  }
  else
    geom_rect(data = df, do.call(aes, args), color = "black", fill = "black")

}
