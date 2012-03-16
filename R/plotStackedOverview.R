## ======================================================================
##        For "Overview"
## ======================================================================
plotStackedOverview <- function(obj, xlab, ylab, main, facets = seqnames ~ .,
                         cytoband = FALSE){

  ## args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
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
      ggplot2::geom_rect(aes(xmin = start,
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
    ideo.gr <- getIdeoGR(obj)
    apply(as.data.frame(values(obj)), 2, class)
    df <- as.data.frame(ideo.gr)
    ## df$seqnames <- factor(as.character(df$seqnames),
    ##                       levels = sortChr(unique(as.character(df$seqnames))))
    p <- ggplot(df)
    p <- p + facet_grid(facets) +
      ggplot2::geom_rect(aes(xmin = start,
                    ymin = 0,
                    xmax = end,
                    ymax = 10), fill = "white", color = "black") +
                        opts(axis.text.y = theme_blank(),
                             axis.title.y=theme_blank(),
                             axis.ticks = theme_blank(),
                             panel.grid.minor = theme_line(colour = NA),
                             panel.grid.major = theme_line(colour = NA))
  }
  if(!missing(xlab))
    p <- p + xlab(xlab)
  if(!missing(ylab))
    p <- p + ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
}


plotSingleChrom <- function(obj, subchr, zoom.region,
                            xlab, ylab, main,  xlabel = FALSE){
  ## do we need subchr here
  if(!missing(subchr)){
    obj <- obj[seqnames(obj) == subchr]
    ## seqlevels(obj) <- sortChr(unique(as.character(seqnames(obj))))
  }
  if(length(unique(as.character(seqnames(obj))))>1)
    stop("Mulptiple chromosome information found")
  p <- plotStackedOverview(obj, cytoband = TRUE)
  p <- p + opts(legend.position = "none") + ggplot2::xlab("")
  if(!missing(zoom.region)){
    if(length(zoom.region) != 2)
      stop("zoom.region must be a numeric vector of length 2")
    zoom.df <- data.frame(x1 = zoom.region[1],
                          x2 = zoom.region[2],
                          seqnames = unique(as.character(seqnames(obj))))
    p <- p + ggplot2::geom_rect(data = zoom.df, aes(xmin = x1,
                         xmax = x2, ymin = -3, ymax = 13), color = "red", fill = NA)
  }
  if(!xlabel)
    p <- p + opts(axis.text.x = theme_blank())
  p <- p + theme_alignment(grid = FALSE, label = TRUE, border = FALSE) +
    scale_y_continuous(breaks = 5, label = subchr)
  if(!missing(xlab))
    p <- p + ggplot2::xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + opts(title = main)
  p
}


