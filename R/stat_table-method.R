setGeneric("stat_table", function(data, ...) standardGeneric("stat_table"))

setMethod("stat_table", "GenomicRanges", function(data, ..., geom = NULL,
                                                  stat = NULL){

  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non <- args.non[!names(args.non) %in% c("data", "facets", "geom", "stat")]
  
  tab <- table(paste(seqnames(data), start(data), end(data), strand(data), sep = ":"))
  key_mat <- matrix(unlist(strsplit(names(tab), ":", fixed=TRUE)), 4)
  gr <- GRanges(key_mat[1,],
                IRanges(as.integer(key_mat[2,]), as.integer(key_mat[3,])),
                key_mat[4,], score = as.integer(tab),
                seqlengths = seqlengths(data))
  seqinfo(gr) <- seqinfo(data)
  args.non$data <- gr
  
  .ggbio.geom <- c("rect", "chevron", "alignment", "arrowrect", "arrow", "segment", "arch")
  .ggbio.stat <- c("identity", "coverage", "stepping", "aggregate")

  ## if(is.null(stat)){
  ## }
  ## ------------------------------
  ## geom/stat check
  ## ------------------------------
  if(is.null(stat) & is.null(geom)){
    stat <- "steppingg"
    args.non$geom <- "segment"
    args.non$stat <- stat
    if(!"color" %in% names(args.aes))
      args.aes$color <- as.name("score")
    .fun <- stat_stepping
  }else{
    .fun <- getDrawFunFromGeomStat(geom, stat)
    if(!is.null(geom)){
    if(geom != "arch"){
    if(is.null(stat)){
      args.non$stat <- stat <- "identity"
    }else{
      args.non$geom <- geom
    }}}
  }
  ## ------------------------------
  ##   get the right function
  ## ------------------------------
  ## if(!"y" %in% names(args.aes)){
  ##   if(is.null(geom)){
  ##     args.aes$y <- as.name("score")
  ## }else{
  ##   if(geom != "arch")
  ##     args.aes$y <- as.name("score")      
  ## }
  ## }
  aes.res <- do.call(aes, args.aes)
  args.res <- c(args.non, list(aes.res))
  p <- do.call(.fun, args.res)
})  

