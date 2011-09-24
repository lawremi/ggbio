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
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  args.sum <- args[names(args) %in% c("data", "model", "group.name")]
  args <- args[!(names(args) %in% c("data","model", "group.name"))]
  ## data = dt; model = md
  ## spliceSummary(data, model, group.name = group.name)
  ## splice.fun <- selectMethod("spliceSummary", "GenomicRanges")
  ## names(args.sum)[names(args.sum) == "data"] <- "obj"
  if(missing(group.name))
    group.name <- "read.group"
  temp <- spliceSummary(data, model, group.name = group.name)$summary
  temp.n <- addSteppings(temp, group = "model.group")
  temp.gap <- getGap(temp.n)
  temp.gap <- resize(temp.gap, width = width(temp.gap)+2L, fix = "center")
  gr.label <- getModelRange(temp.n)
  p <- ggplot(as.data.frame(temp.n))
  args.rect <- c(args, list(xmin = substitute(start),
                            xmax = substitute(end),
                            ymin = substitute(.levels - 0.4),
                            ymax = substitute(.levels + 0.4)))
  args.rect <- args.rect[!(names(args.rect) %in% c("size", "color"))]
  p <- p + geom_rect(do.call("aes", args.rect))
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
