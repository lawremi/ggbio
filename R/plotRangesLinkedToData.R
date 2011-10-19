plotRangesLinkedToData <- function(data, stat.col, stat.label, ..., annotation = list(),
                                   width.ratio = 0.8,
                                   heights = c(400, 100, 100, rep(300, length(annotation)))){
  args <- as.list(match.call(call = sys.call(sys.parent()))[c(-1,-2)])
  args <- args[!names(args) %in% c("stat.col", "stat.label", "annotation")]
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
  ## df.new <- melt(df, id.vars = c("seqnames", "start", "end",
  ##                    "width", "strand", "x.new") )
  if(missing(stat.label))
    stat.label <- stat.col
  df.new$.ggbio.group <- rep(stat.label, each = nrow(df))
  p <- ggplot(df.new)
  ## args$data <- df
  args <- args[names(args) != "data"]
  args.seg.solid <- c(list(x = substitute(x.new-wid, list(wid = wid)),
                           xend = substitute(x.new+wid, list(wid = wid)),
                           color = substitute(.ggbio.group),
                           y = substitute(value)),
                      args)
  args.seg.solid$yend  <-  args.seg.solid$y
  p <- p + geom_segment(do.call(aes, args.seg.solid))

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
  ## y = substitute(value))
  p.stat <- p + geom_segment(data = df.dash, do.call(aes, args.dash))
  p.stat <- p.stat  +  opts(#panel.background=theme_blank(), 
                            panel.grid.minor=theme_blank()) +  theme_bw()

  ## link track
  df$midpoint <- (df$start + df$end)/2
  p.link <- ggplot(df)
  args.link <- list(x = substitute(midpoint),
                    xend = substitute(x.new),
                    y = 0,
                    yend = 10)
  p.link <- p.link + geom_segment(do.call(aes, args.link), color = "gray")+
     theme_bw() + 
       opts(##panel.background=theme_blank(), 
         panel.grid.minor=theme_blank(),
         ## taxis.text.y = theme_blank(),         
         panel.grid.major = theme_blank()
         )  + ylab(" ") 
  ## scale_y_continuous(breaks= 5, labels = 5)            
  
  ## single model
  grl <- GRangesList(data)
  names(grl) <- "1"
  p.single <- qplot(grl) +  opts(panel.grid.minor=theme_blank()) + ylab(" ")+
    theme_bw()

  if(length(annotation)){
    annotation <- lapply(annotation, function(p) p + theme_bw())
    ## tracks(p.stat,p.link,p.single)
    args.tracks <- c(list(p.stat, p.link, p.single),
                     annotation, list(heights = heights))
  }else{
    args.tracks <- c(list(p.stat, p.link, p.single),
                     list(heights = heights))
  }
  do.call(tracks, args.tracks)
  ## tracks(p.stat, p.link, p.single, heights = heights)
}



