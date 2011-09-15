## tracks
tracks <- function(..., check.xlim = TRUE,
                   remove.extraXlim = TRUE,
                   legend = FALSE,
                   xlim.fix, ylim.fix){
  dots <- list(...)
  dots <- c(list(ncol = 1), dots)
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
      x <- eval(obj$layers[[1]]$mapping$x, obj$data)
      if(!is.null(x))
        data.frame(xmin = min(x),xmax = max(x))
      else
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
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(legend)
                      grobs[[i]] <- grobs[[i]] + s
                    else
                      grobs[[i]] <- grobs[[i]] + s +
                        opts(legend.position = "none")
                    if(i %in% 1:(N-1))
                      grobs[[i]] <- grobs[[i]] + opts(axis.text.x = theme_blank(),
                                                    axis.title.x=theme_blank())
                    if(i == 1){
                      grobs[[i]] <- grobs[[i]] +
                        opts(plot.margin = unit(c(1, 1.8, 0, 0), "lines"))
                    }else{
                      grobs[[i]] <- grobs[[i]] +
                        opts(plot.margin = unit(c(0, 1.8, 0, 0), "lines"))
                    }
                    grobs[[i]]+ opts(axis.text.y = theme_blank(),
                                     axis.ticks = theme_blank())
                    
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

rangesCentric <- function(gr, which, id.name){
  ## first subset, need to give notice
  gr <- subsetByOverlaps(gr, which)
  which <- which[order(start(which))]
  gr <- gr[order(start(gr))]
  ## which <- subsetByOverlaps(which, gr)
  ## FIXME: give message here
  ## need to be exclusive
  which <- reduce(which)
  mx <- matchMatrix(findOverlaps(gr, which))
  values(gr)$.id.name <- NA
  values(gr)$.id.name[mx[,1]] <- mx[,2]
  if(!missing(id.name) && (id.name %in% colnames(values(which))))
    values(gr)$.id.name[mx[,1]] <- values(which)[mx[,2],id.name]
  values(which)$.id.name <- seq_len(length(which))
  ## cannot be putted into GRangesList, unequal, should be in a GenomicRangesList
  list(gr = gr, which = which)
}
