plotMismatchSum <- function(obj, show.coverage = TRUE){
  if(!isPileupSum(obj))
    stop("For geom mismatch summary, data must returned from
                        pileupGRangesAsVariantTable function. Or is a GRanges
                        object including arbitrary columns: read, ref, count, depth,
                        match")
  df <- as.data.frame(obj)
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
  addLevels <- function(x){
    idx <- order(x$start, x$read)
    ## assumption: on the same chromosome
    x <- x[idx,]
    eds <- unlist(by(x$count, x$start, function(x){
      cumsum(x)
    }))
    sts <- unlist(by(x$count, x$start, function(x){
      N <- length(x)
      c(0,cumsum(x)[-N])
    }))
    x$eds <- eds
    x$sts <- sts
    x
  }
  df.unmatch <- addLevels(df.unmatch)
  idx <- order(df.bg$start)
  df.bg <- df.bg[idx,]
  p <- ggplot(df.bg)
  if(show.coverage)
    p <- p + geom_area(aes(x = start, y = depth), fill = I("gray70"), color = I("gray70"))
  DNABasesColor <- getBioColor("DNA_BASES_N")
  p <- p + geom_segment(data = df.unmatch, aes(x = start, y = sts,
                          xend = start, yend = eds, color = read))+
          scale_color_manual(values = DNABasesColor)
  p <- p + xlab("Genomic Coordinates") + ylab("Counts")
  p
}


isPileupSum <- function(x){
  is(x, "GRanges") &&
  c("read", "ref", "count", "depth", "match") %in% colnames(values(x))
}

