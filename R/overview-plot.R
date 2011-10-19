## ======================================================================
##        For "Overview"
## ======================================================================
plotOverview <- function(obj, 
                         cytoband = FALSE){

  if(cytoband){
    cytobandColor <- getOption("biovizBase")$cytobandColor
    if(!isIdeogram(obj))
        stop("Need cytoband information, please check the getIdeogram function")
    df <- as.data.frame(obj)
    ## df$seqnames <- factor(as.character(df$seqnames),
    ##                       levels = sort(unique(as.character(df$seqnames))))
    df.rect <- subset(df, gieStain != "acen")
    df.tri <- subset(df, gieStain == "acen")
    df.tri.p <- df.tri[substr(df.tri$name, 1, 1) == "p",]
    df.tri.q <- df.tri[substr(df.tri$name, 1, 1) == "q",]
    p <- ggplot(df.rect)
    p <- p + facet_grid(seqnames ~ .) +
      geom_rect(aes(xmin = start,
                    ymin = 0,
                    xmax = end,
                    ymax = 10,
                    fill = gieStain),
                color = "black")
     p <- p +  geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                    y = c(rep(0, length(start)),
                      rep(10,length(start)),
                      rep(5,length(start))), fill = gieStain))
     p <- p +  geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                    y = c(rep(5, length(start)),
                      rep(10,length(start)),
                      rep(0,length(start))), fill = gieStain))
    
     p <- p + geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                     y = c(0, 10, 5), fill = gieStain))+
      geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                     y = c(5, 10, 0), fill = gieStain))+
                    opts(axis.text.y = theme_blank(),
                         axis.title.y=theme_blank(),
                         axis.ticks = theme_blank(),
                         panel.grid.minor = theme_line(colour = NA),
                         panel.grid.major = theme_line(colour = NA))+
                           scale_fill_manual(values = cytobandColor)
   
  }else {
    if(!isSimpleIdeogram(obj)){
      message("Reduce to simple genome, ignoring cytoband")
      ## obj <- sortChr(reduce(obj))
      ## obj <- sort(reduce(obj))
      obj <- reduce(obj)
      if(!isSimpleIdeogram(obj))
        stop("Cannot reduce to simple genome, please check your ideogram")
    }
    df <- as.data.frame(obj)
    ## df$seqnames <- factor(as.character(df$seqnames),
    ##                       levels = sortChr(unique(as.character(df$seqnames))))
    p <- ggplot(df)
    p <- p + facet_grid(seqnames ~ .) +
      geom_rect(aes(xmin = start,
                    ymin = 0,
                    xmax = end,
                    ymax = 10), fill = "white", color = "black") +
                        opts(axis.text.y = theme_blank(),
                             axis.title.y=theme_blank(),
                             axis.ticks = theme_blank(),
                             panel.grid.minor = theme_line(colour = NA),
                             panel.grid.major = theme_line(colour = NA))
  }
  p <- p + xlab("Genomic Coordinates") + ylab("Chromosomes")
  p
}


plotSingleChrom <- function(obj, subchr, zoom.region, xlabel = FALSE){
  ## do we need subchr here
  if(!missing(subchr)){
    keepSeqlevels(obj, "chr1")
    obj <- obj[seqnames(obj) == subchr]
    obj
    ## seqlevels(obj) <- sortChr(unique(as.character(seqnames(obj))))
    seqlevels(obj)
    names(seqlevels(obj))
  }
  if(length(unique(as.character(seqnames(obj))))>1)
    stop("Mulptiple chromosome information found")
  p <- plotOverview(obj, cytoband = TRUE)
  p <- p + xlab("") + opts(legend.position = "none")
  if(!missing(zoom.region)){
    if(length(zoom.region) != 2)
      stop("zoom.region must be a numeric vector of length 2")
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          seqnames = unique(as.character(seqnames(obj))))
    p <- p + geom_rect(data = zoom.df, aes(xmin = x1,
                         xmax = x2, ymin = -1, ymax = 11), color = "red", fill = NA)
  }
  if(!xlabel)
    p <- p + opts(axis.text.x = theme_blank(),
                  axis.title.x=theme_blank())
  p
}

