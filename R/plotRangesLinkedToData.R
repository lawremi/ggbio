## TODO: exons label
## check (done)
## significant
##  log y
plotRangesLinkedToData <- function(data, stat.col, stat.label,
                                   stat.ylab,
                                   sig, sig.col = c("black", "red"),
                                   stat.coord.trans = coord_trans(),
                                   solid.size = 1.3,...,
                                   annotation = list(),
                                   width.ratio = 0.8,
                                   track.skip = -1,
                                   theme.stat = theme_grey(),
                                   theme.align = theme_grey(),
                                   heights){
  
  args <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  args <- args[!names(args) %in% c("stat.col", "stat.label", "annotation", "solid.size",
                                   "stat.coord.trans","sig.col","stat.ylab",
                                   "heights", "track.skip")]

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

  if(missing(stat.col) & missing(stat.label))
    stop("stat.col or stat.label must be provided" )
  if(!missing(stat.col)){
    if(is.numeric(stat.col))
      stat.col <- stat.col 
    else
      stop("stat.col must be numeric values")
    if(missing(stat.label))
      stat.label <- colnames(values(data))[stat.col]
    stat.col <- stat.col + 5
  }else{
    if(all(stat.label %in% colnames(values(data))))
      stat.col <- match(stat.label, colnames(values(data))) + 5
    else
      stop("stat.label must be the column names of element meta data")
  }

  df.new <- melt(df, measure.vars = stat.col)  

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
  p.stat <- p.stat  +  theme.stat + opts(panel.grid.minor=theme_blank()) +
    labs(colour = "group")
  p.stat <- p.stat + stat.coord.trans
  if(!missing(stat.ylab))
    p.stat <- p.stat + ylab(stat.ylab)
  ## link track
  df$midpoint <- (df$start + df$end)/2
  p.link <- ggplot(df)
  if(missing(sig)){
    args.link <- list(x = substitute(midpoint),
                      xend = substitute(x.new),
                      y = 0,
                      yend = 10)
    p.link <- p.link + geom_segment(do.call(aes, args.link)) + theme_null()    
  }else{
    args.link <- list(x = substitute(midpoint),
                      xend = substitute(x.new),
                      y = 0,
                      yend = 10,
                      color = as.name(sig))
    p.link <- p.link + geom_segment(do.call(aes, args.link)) + theme_null()        
    p.link <- p.link + scale_color_manual(values = c("FALSE" = sig.col[1],
                                            "TRUE" = sig.col[2])) 
  }
  p.link <- p.link + opts(legend.position = "none")

  ## grl <- GRangesList(data)
  if(missing(sig)){
    ## grl
    p.single <- autoplot(data, geom = "alignment") + theme.align + scale_y_continuous(breaks = NA)
  }else{
    args <- list(data = data,
                 fill = as.name(sig),
                 color = as.name(sig),
                 geom = "alignment")
   p.single <- do.call(autoplot, args) +
     theme.align + scale_y_continuous(breaks = NULL)    
   p.single <- p.single +
     scale_color_manual(values = c("FALSE" = sig.col[1], "TRUE" = sig.col[2])) +
       scale_fill_manual(values = c("FALSE" = sig.col[1], "TRUE" = sig.col[2])) +
         opts(legend.position = "none")
}
  if(length(annotation)){
    ## annotation <- lapply(annotation, function(p) p + theme_bw())
    ## tracks(p.stat,p.link,p.single)
    args.tracks <- c(c(list(p.stat, p.link, p.single), annotation),
                     list(heights = heights),
                     list(track.skip = track.skip))
  }else{
    args.tracks <- c(list(p.stat, p.link, p.single),
                     list(heights = heights),
                     list(track.skip = track.skip))
  }
  do.call(tracks, args.tracks)
}




