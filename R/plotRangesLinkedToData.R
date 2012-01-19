plotRangesLinkedToData <- function(data, stat.col, stat.label, solid.size = 1.3,...,
                                   annotation = list(),
                                   width.ratio = 0.8, 
                                   heights){
  
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  args <- args[!names(args) %in% c("stat.col", "stat.label", "annotation", "solid.size")]

  if(missing(heights))
      heights <- unit(c(2.5, 0.5, 1, rep(1, length(annotation))), "null")

  gr <- transformGRangesForEvenSpace(data)
  wd <- width(range(gr))
  ## we need to find midpoint first
  N <- length(gr)
  wid <- wd/N/2 * width.ratio
  df <- as.data.frame(gr)
  ## FIXME: need to allow specified by stat.col
  ## head(df)
  if(is.numeric(stat.col))
    stat.col <- stat.col + 5
  df.new <- melt(df, measure.vars = stat.col)  

  if(missing(stat.label))
    stat.label <- stat.col
  df.new$.ggbio.group <- rep(stat.label, each = nrow(df))
  p <- ggplot(df.new)

  args <- args[names(args) != "data"]
  args.seg.solid <- c(list(x = substitute(x.new-wid, list(wid = wid)),
                           xend = substitute(x.new+wid, list(wid = wid)),
                           color = substitute(.ggbio.group),
                           y = substitute(value)),
                      args)

  args.seg.solid$yend  <-  args.seg.solid$y

  p <- p + geom_segment(do.call(aes, args.seg.solid), size = solid.size)  


  df.dash <- data.frame(x = df.new[c(-N, -2*N), "x.new"] + wid,
                        xend = df.new[c(-1, -(N+1)), "x.new"] - wid,
                        y = df.new[c(-N, -2*N), "value"],
                        yend = df.new[c(-1, -(N+1)), "value"],
                        .ggbio.group = df.new[c(-N, -2*N),".ggbio.group"])
  args.dash.a <- args[!names(args) %in%  c("linetype", "y")]

  args.dash <- c(list(x = substitute(x),
                      y = substitute(y),
                      xend = substitute(xend),
                      yend = substitute(yend),
                      linetype = 3,
                      color = substitute(.ggbio.group)),
                 args.dash.a)

  p.stat <- p + geom_segment(data = df.dash, do.call(aes, args.dash))
  p.stat <- p.stat  +  theme_bw() + opts(panel.grid.minor=theme_blank()) +
    scale_colour_discrete(name = "Group")    



  ## link track
  df$midpoint <- (df$start + df$end)/2
  p.link <- ggplot(df)
  args.link <- list(x = substitute(midpoint),
                    xend = substitute(x.new),
                    y = 0,
                    yend = 10)

  p.link <- p.link + geom_segment(do.call(aes, args.link)) + theme_null()

  grl <- GRangesList(data)
  ## names(grl) <- "1"
  ## p.single <- autoplot(grl) +  opts(panel.grid.minor=theme_blank()) + ylab(" ") +
  ##   theme_bw()
  p.single <- autoplot(grl) + theme_alignments() + scale_y_continuous(breaks = NA)
  if(length(annotation)){
    ## annotation <- lapply(annotation, function(p) p + theme_bw())
    ## tracks(p.stat,p.link,p.single)
    args.tracks <- c(list(p.stat, p.link, p.single),
                     annotation, list(heights = heights))
  }else{
    args.tracks <- c(list(p.stat, p.link, p.single),
                     list(heights = heights))
  }
  do.call(tracks, args.tracks)
}




