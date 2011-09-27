plotMismatchSum <- function(obj, show.coverage = TRUE){
  if(!isPileupSum(obj))
    stop("For geom mismatch summary, data must returned from
                        pileupGRangesAsVariantTable function. Or is a GRanges
                        object including arbitrary columns: read, ref, count, depth,
                        match")
  df <- as.data.frame(obj)
  df.unmatch <- df[!df$match, ]
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
  p <- ggplot(df)
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

