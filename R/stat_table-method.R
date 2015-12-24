## ..score..
setGeneric("stat_table", function(data, ...) standardGeneric("stat_table"))

setMethod("stat_table", "GRanges", function(data, ..., xlab, ylab, main,
                                            geom = NULL, stat = NULL){

  args <- list(...)
  
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)

  if(length(data)){
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
      stat <- "stepping"
      args.non$geom <- "rect"
      args.non$stat <- stat
      if(!"color" %in% names(args.aes))
        args.aes$color <- args.aes$fill <- as.name("score")
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
    aes.res <- do.call(aes, args.aes)
    args.res <- c(args.non, list(aes.res))
    p <- do.ggcall(.fun, args.res)
  }else{
    p <- NULL
  }

  if(missing(xlab)) 
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)  
  p  
})  


setMethod("stat_table", "GRangesList", function(data, ..., 
                                                xlab, ylab, main,
                                                facets = NULL, 
                                                geom = NULL){
  
  args <- list(...)

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  aes.res <- do.call(aes, args.aes)
  gr <- flatGrl(data)
  args.non$data <- gr
  p <- do.call(stat_table, c(list(aes.res), args.non))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("Score")))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)  
  p
})
