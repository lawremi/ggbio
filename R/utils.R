## tracks
tracks <- function(..., check.xlim = TRUE,
                   remove.extraXlim = TRUE,
                   legend = FALSE,
                   xlim.fix, ylim.fix){
  ## sicne this works for GRanges, genomic data
  ## so just check simple field
  ## need to check if y is labeld, it need to be labeled
  dots <- list(...)
  params <- c("nrow", "ncol", "widths", "heights", "default.units", 
              "respect", "just")
  layout.call <- intersect(names(dots), params)
  params.layout <- dots[layout.call]
  
  if (is.null(names(dots))) 
    not.grobnames <- FALSE
  else not.grobnames <- names(dots) %in% layout.call

  grobs <- dots[!not.grobnames]
  if(missing(xlim.fix)){
    lst <- lapply(grobs, function(obj){
      data.frame(xmin = min(obj$data$start),xmax = max(obj$data$end))
    })
    res <- do.call("rbind", lst)
    xlim.fix <- c(min(res$xmin), max(res$xmax))
    extr <- 0.05*(diff(xlim.fix))
    xlim.fix <- c(c(xlim.fix[1] - extr), c(xlim.fix[2] + extr))
  }
  s <- scale_x_continuous(limits = xlim.fix)
  if(!missing(ylim.fix))
    s <- scale_y_continuous(limits = xlim.fix)
  ## need fix legend
  if(check.xlim){
    N <- length(grobs)
    ## leg.lst <- lapply(seq_len(N),
    ##                   function(i) {
    ##                       ggplotGrob(grobs[[i]] + opts(keep="legend_box"))
    ##                   })
    ##  ## one needs to provide the legend with a well-defined width
    ## legend=gTree(children=do.call("gList",leg.lst), cl="legendGrob")
    ## params.layout <- c(params.layout, list(legend = substitute(legend)))
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(legend)
                      grobs[[i]] <- grobs[[i]] + s
                    else
                      grobs[[i]] <- grobs[[i]] + s +
                        opts(legend.position = "none")
                    if(i %in% 1:(N-1))
                      grobs[[i]] <- grobs[[i]]+opts(axis.text.x = theme_blank(),
                                                    axis.title.x=theme_blank(),
                                                    axis.ticks = theme_blank())
                    grobs[[i]]
                  })
    widthDetails.legendGrob <- function(x) unit(10, "cm")    
    ## grid.arrange(lst[[1]],lst[[2]], lst[[3]], legend = legend)
    res <- do.call(grid.arrange, c(lst, params.layout))
  }else{
    res <- grid.arrange(...)
  }
  invisible(lst)
}


plotSpliceSum <- function(data, model, ..., group.name,
                          show.label = FALSE){
getGap <- function(data){
  res <- split(data, values(data)[,"model.group"])
  gps.lst <- lapply(res, function(x){
    gps <- gaps(ranges(x))
    if(length(gps)){
      gr <- GRanges(unique(as.character(seqnames(x))), gps)
      values(gr)$.levels <- unique(values(x)$.levels)
      values(gr)$.model.group <- unique(values(x)$.model.group)
      values(gr)$freq <- unique(values(x)$freq)
      gr
    }else{
      NULL
    }
  })
  gps <- do.call("c", gps.lst)          #remove NULL
  gps <- do.call("c", unname(gps))
  values(gps)$type <- "gaps"
  gps
}

getModelRange <- function(data){
  seqs <- unique(as.character(seqnames(data)))
  ir <- unlist(range(ranges(split(temp.n, values(data)$model.group),
                            ignore.strand = TRUE)))
  freqs <- values(data)$freq[match(names(ir), values(data)$model.group)]
  .lvs <- values(data)$.levels[match(names(ir), values(data)$model.group)]
  ## with levels
  gr <- GRanges(seqs, ir, freq = freqs, .levels = .lvs)
  gr
}
args <- as.list(match.call())[-1]
args.sum <- args[names(args) %in% c("data", "model", "group.name")]
args <- args[!(names(args) %in% c("data","model", "group.name"))]
## data = dt; model = md
## spliceSummary(data, model, group.name = group.name)
temp <- do.call("spliceSummary", args.sum)$summary
temp.n <- addLevels(temp, group = "model.group")
temp.gap <- getGap(temp.n)
temp.gap <- resize(temp.gap, width = width(temp.gap)+2L, fix = "center")
gr.label <- getModelRange(temp.n)
## draw temp.n first
p <- ggplot(as.data.frame(temp.n))
args.rect <- c(args, list(xmin = substitute(start),
                          xmax = substitute(end),
                          ymin = substitute(.levels - 0.4),
                          ymax = substitute(.levels + 0.4)))
args.rect <- args.rect[!(names(args.rect) %in% c("size", "color"))]
p <- p + geom_rect(do.call("aes", args.rect))
## draw gaps
args.seg <- c(args, list(x = substitute(start),
                         xend = substitute(end),
                         y = substitute(.levels),
                         yend = substitute(.levels)))
p <- p + geom_segment(data = as.data.frame(temp.gap),
                      do.call("aes", args.seg))
## ## draw labels
if(show.label){
args.lab <- c(args, list(x = substitute(end),
                         y = substitute(.levels),
                         label = substitute(freq)))
p <- p + geom_text(data = as.data.frame(gr.label),
                   do.call("aes", args.lab), hjust = -1)
}
p  
}

## ======================================================================
##        For "Seqlogo"
## ======================================================================

## ======================================================================
##        For "Overview"
## ======================================================================
plotOverview <- function(obj, hotRegion, cytobandColor,
                         subchr,
                         cytoband = FALSE){
  ## make sure the levels are not redundant
  ## obj <- shrinkSeqlevels(obj)
  if(cytoband){
    if(missing(cytobandColor)){
      cytobandColor <- getOption("visnabBase")$cytobandColor
      cytobandColor <- unlist(cytobandColor)
    }
    if(!isIdeogram(obj))
        stop("Need cytoband information, please check the getIdeogram function")
    df <- as.data.frame(obj)
    df$seqnames <- factor(as.character(df$seqnames),
                          levels = sortChr(unique(as.character(df$seqnames))))
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
    if(!isSimpleOverview(obj)){
      message("Reduce to simple genome, ignoring cytoband")
      obj <- sortChr(reduce(obj))
      if(!isSimpleOverview(obj))
        stop("Cannot reduce to simple genome, please check your ideogram")
    }
    df <- as.data.frame(obj)
    df$seqnames <- factor(as.character(df$seqnames),
                          levels = sortChr(unique(as.character(df$seqnames))))
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

plotSingleChrom <- function(obj, subchr, zoom.region){
  if(!missing(subchr))
    obj <- obj[seqnames(obj) == subchr]
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
  p
}

geom_hotregion <- function(data,...){
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  args <- args[!names(args) %in% "data"]
  aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
  if(length(aes.lst)){
    idx <- which(aes.lst)
    aes.lst <- eval(args[[idx]])
  }else{
    aes.lst <- list()
  }
  args <- c(aes.lst, list(xmin = substitute(start),
                       xmax = substitute(end),
                       ymin = 0,
                       ymax = 10))

  df <- as.data.frame(data)
  ## this is a hack to make sure geom_rect can show segments too
  ## need color and fill when it's just segment
  if(any(c("colour", "fill") %in% names(args))){
    if(!all(c("colour", "fill") %in% names(args))){
      idx <- which(c("colour", "fill") %in% names(args))
      known <- c("colour", "fill")[idx]
      unknown <- c("colour", "fill")[-idx]
      args[[unknown]] <- args[[known]]
    }
    geom_rect(data = df, do.call(aes, args))
  }
  else
    geom_rect(data = df, do.call(aes, args), color = "black", fill = "black")

}
