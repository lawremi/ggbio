## default show y axis of the first track and hide others
setMethod("plotFragLength", c("character", "GRanges"),
          function(data, model,
                   gap.ratio = 0.0025,
                   geom = c("segment", "point", "line"),
                   type = c("normal", "cut"),
                   heights = c(400, 100),
                   annotation = TRUE){
  ## default show it, but only keep first one
    show.axis.text.y <- FALSE
  ## geom <- match.arg(geom)
  type <- match.arg(type)
  message("Compute fragment length...")
  lst <- biovizBase:::getFragLength(data, model)
  message("Plotting...")
  gr.fraglength <- lst$gr
  if(type == "cut"){
    frag <- lst$fragLength
    reads <- lst$reads # combinded paried reads
    g.gap <- gaps(c(ranges(reads), ranges(model)))
    chr <- unique(as.character(seqnames(model)))
    cut.fun <- shrinkageFun(g.gap, maxGap(GRanges(chr, g.gap), gap.ratio))
    reads.cut <- cut.fun(reads)
    grl <- split(reads.cut, values(reads.cut)$mapid)
    grl.r <- range(grl)
    gr.res <- unlist(grl.r)
    names(gr.res) <- names(grl)
    values(gr.res)$.fragLength <- frag[names(gr.res)]
    gr.fraglength <- gr.res
    model <- cut.fun(model)
  }
  ## since model is always there
  ## so make sure it's overlaped
  gr.fraglength <- keepSeqlevels(gr.fraglength,
                                 unique(as.character(seqnames(gr.fraglength))))
  gr.fraglength <- subsetByOverlaps(gr.fraglength, range(model), type = "within")
    if(is(model, "GRanges"))
      model <- GRangesList(model)
    names(model) <- "1"
    p.exon <- autoplot(model) + ylab(" ") + theme_bw() +theme(panel.grid.minor=element_blank(),
                                             panel.grid.major=element_blank())
                                               ## scale_y_continuous(breaks = c(0),
                                               ##                   labels = " x")
                                               ## theme(axis.text.y = element_blank())

    df <- as.data.frame(gr.fraglength)
    p <- ggplot(df)
    if("segment" %in% geom){
      p <- p + geom_segment(aes(x = start,
                                y = .fragLength,
                                xend = end,
                                yend = .fragLength), color = "gray")

    if(annotation)
      p <- p + theme(panel.grid.minor=element_blank()) +  theme_bw()
    }
    if("point" %in% geom){
      p <- p + geom_point(aes(x = (start + end)/2, y = .fragLength), size = 1.2,
                          color = "gray30") +  theme_bw()
    if(annotation)      
     p <- p + theme(panel.grid.minor=element_blank())
    }
    if("line" %in% geom){
      p <- p + geom_line(aes(x = (start + end)/2, y = .fragLength), size = 1.2,
                          color = "gray30")  +  theme_bw()
    if(annotation)      
     p <- p + theme(panel.grid.minor=element_blank())
    }
    p <- p + ylab("Estimated Fragmeng Length") 
    if(annotation)
      tracks(p, p.exon, heights = heights, show.axis.text.y = show.axis.text.y)
    else{
      p <- p + xlab("Genomic Coordinates")
      p
    }
})
