setGeneric("plotFragLength", function(data, model, ...) standardGeneric("plotFragLength"))
setMethod("plotFragLength", c("character", "GRanges"),
          function(data, model,
                   maxGap = 0L,
                   geom = c("segment", "point", "line"),
                   type = c("normal", "cut")){
  geom <- match.arg(geom)
  type <- match.arg(type)
  lst <- biovizBase:::getFragLength(data, model)
  gr.fraglength <- lst$gr
  ## gr.cut <- lst$gr.cut
  ## model.cut <- lst$model.cut
  model <- lst$model
  ## p.exon <- qplot(model.cut)
  if(type == "normal"){
    p.exon <- qplot(model) + ylab("") + opts(panel.grid.minor=theme_blank()) +  theme_bw()
    df <- as.data.frame(gr.fraglength)
    p <- ggplot(df)
    if(geom == "segment")
      p <- p + geom_segment(aes(x = start,
                                y = .fragLength,
                                xend = end,
                                yend = .fragLength), color = "gray")+
    opts(panel.grid.minor=theme_blank()) +  theme_bw()                                  
    if(geom == "point")
      p <- p + geom_point(aes(x = (start + end)/2, y = .fragLength), size = 1.2,
                          color = "gray30") +
     opts(panel.grid.minor=theme_blank()) +  theme_bw()                            
    if(geom == "line")
      p <- p + geom_line(aes(x = (start + end)/2, y = .fragLength), size = 1.2,
                          color = "gray30") +
           opts(panel.grid.minor=theme_blank()) +  theme_bw()
    tracks(p, p.exon)
  }
})
