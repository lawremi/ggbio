setGeneric("stat_mismatch", function(data, ...) standardGeneric("stat_mismatch"))

setMethod("stat_mismatch", "GRanges", function(data, ..., bsgenome,
                                                     xlab, ylab, main,
                                                     geom = c("segment", "bar"),
                                                     show.coverage = TRUE){
  geom <- match.arg(geom)
  args <- list(...)

  ## args <- force(args)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  
  isPileupSum <- function(obj){
    if(is(obj, "GRanges")){
      all(c("read", "ref", "count", "depth", "match") %in% colnames(values(obj)))
    }else if(is(obj, "data.frame")){
      all(c("read", "ref", "count", "depth", "match") %in% colnames(obj))
    }else{
      FALSE
    }
  }
  if(length(data)){
  if(!isPileupSum(data))
    stop("For geom mismatch summary, data must returned from
                        biovizBase::pileupGRangesAsVariantTable function. Or is a GRanges
                        object including arbitrary columns: read, ref, count, depth,
                        match")



  
  ## df <- as.data.frame(data)
  df <- mold(data)
  df.unmatch <- df[!df$match, ]
  ## add two end point?
  pos <- min(df$start):max(df$end)
  idx <- ! (pos %in% df$start)
  if(sum(idx)){
    df.bg <- df[,c("seqnames", "start", "end", "width", "strand", "depth")]
    df.bg.extra <- data.frame(seqnames = unique(as.character(df.bg$seqnames)),
                              start = pos[idx],
                              end = pos[idx],
                              width = 1,
                              strand = "*",
                              depth = 0)
    df.bg <- rbind(df.bg, df.bg.extra)
  }else{
    df.bg <- df
  }
  df.bg <- df.bg[order(df.bg$start),]
  df.bg <- rbind(df.bg[1,], df.bg)
  df.bg <- rbind(df.bg, df.bg[nrow(df.bg),])
  df.bg[c(1, nrow(df.bg)),]$depth <- 0
  addLevels <- function(x){
    idx <- order(x$start, x$read)
    ## assumption: on the same chromosome
    x <- x[idx,]
    eds <- unlist(by(x$count, x$start, function(x){
      cumsum(x)
    }))
    eds <- as.numeric(eds)
    sts <- unlist(by(x$count, x$start, function(x){
      N <- length(x)
      c(0,cumsum(x)[-N])
    }))
    sts <- as.numeric(sts)
    x$eds <- eds
    x$sts <- sts
    x
  }
  df.unmatch <- addLevels(df.unmatch)
  idx <- order(df.bg$start)
  df.bg <- df.bg[idx,]
  ## p <- ggplot(df.bg)
  args.aes$x <- as.name("start")
  args.aes$y <- as.name("depth")
  aes.res <- do.call(aes, args.aes)
  args.non$fill <- I("gray70")
  args.res <- c(list(data = df.bg),
                list(aes.res),
                args.non)
  if(show.coverage)
    p <- list(do.ggcall(ggplot2::geom_polygon, args.res))
  else
    p <- NULL
  DNABasesColor <- getBioColor("DNA_BASES_N")
  if(geom == "segment"){
    p <- c(p, list(ggplot2::geom_segment(data = df.unmatch, aes(x = start, y = sts,
                                           xend = start, yend = eds, color = read))),
           list(scale_color_manual(values = DNABasesColor)))
  }
  if(geom == "bar"){
    p <- c(p, list(ggplot2::geom_rect(data = df.unmatch, aes(xmin = start-0.5, ymin = sts,
                                           xmax = start+0.5, ymax = eds, color = read,
                                        fill = read))),
           list(scale_color_manual(values = DNABasesColor)),
           list(scale_fill_manual(values = DNABasesColor)))
  }
}else{
  p <- NULL
}
  ## p <- c(p, list(xlab("Genomic Coordinates")), list(ylab("Counts")))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("Counts")))

  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- setStat(p)  
  p
})


setMethod("stat_mismatch", "BamFile", function(data, ...,  bsgenome, which,
                                               xlab, ylab, main,
                                               geom = c("segment", "bar"),  
                                               show.coverage = TRUE){
  if(missing(which)){
      ## stop("missing which is not supported yet")
      p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
             list(ggplot2::xlim(c(0, 1))))
      return(p)
  }
    
  geom <- match.arg(geom)
    if(missing(bsgenome)){
      stop("For geom mismatch.summary, please provide bsgenome(A BSgenome object)")
    }else
    if(!is(bsgenome, "BSgenome")){
      stop("bsgenome must be A BSgenome object")
    }
    data <- data$path
    pgr <- pileupAsGRanges(data, regions = which)
    if(length(pgr)){    
    pgr.match <- pileupGRangesAsVariantTable(pgr, bsgenome)
    p <- stat_mismatch(pgr.match, ..., show.coverage = show.coverage, geom = geom)
  }else{
    p <- NULL
  }
    if(!missing(xlab))
      p <- c(p, list(ggplot2::xlab(xlab)))
    else
      p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

    if(!missing(ylab))
      p <- c(p, list(ggplot2::ylab(ylab)))
    else
      p <- c(p, list(ggplot2::ylab("Counts")))

    if(!missing(main))
      p <- c(p, list(labs(title = main)))
  p <- setStat(p)
    p
    
  })

