setGeneric("geom_arrow", function(data, ...) standardGeneric("geom_arrow"))

setMethod("geom_arrow", "data.frame", function(data, ..., angle = 30,
                                               length = unit(0.25, "cm"),
                                               ends = "last",
                                               type = "open"
                                               ){
browser()
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  aes.lst <- parseArgsForAes(args)
  args <- parseArgsForNonAes(args)
  aes.temp <- do.call(aes, aes.lst)
  arrow.formals <- names(formals(arrow))
  args.arrow <- args[names(args) %in% arrow.formals]
  args <- args[!names(args) %in% arrow.formals]
  args$arrow <- arrow(length = length, ends = ends, type = type, angle = angle)
  do.call(geom_segment, c(args, list(aes.temp)))
})




setMethod("geom_arrow", "GenomicRanges", function(data, ...){
  N <- 40
  data <- gr
  values(data)$.levels <- disjointBins(data)
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  ## args <- args[names(args) != data]
  aes.lst <- parseArgsForAes(args)
  df <- as.data.frame(data)
  browser()
  lst <- apply(df, 1, function(x){
     x <- as.data.frame(t(x))
    res <- approx(c(as.numeric(as.character(x$start)), as.numeric(as.character(x$end))),
           rep(as.numeric(as.character(x$.levels)), 2),n = N)
    res.df <- do.call(rbind,lapply(1:N, function(i){
      x
    }))
    res.df$temp.x <- res$x
    res.df
  })
  res <- do.call(rbind,lst)
  
})


breakLine2seg <- function(data){
  
}
approx(x = c(0, 1), y = c(0, 0))
geom_line()
