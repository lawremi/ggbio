plotSpliceSum <- function(data, ..., group.name,
                          show.label = FALSE,
                          geom = c("full", "single.arc", "single.triangle")){
  ## get gaps with the same .level for the same group
  args <- as.list(match.call(call = sys.call(sys.parent())))[-1]
  args.sum <- args[names(args) %in% c("data", "model", "group.name")]
  args <- args[!(names(args) %in% c("data","model", "group.name"))]
  if(geom == "full"){
    temp.n <- addSteppings(data, group = group.name)
    ## gaps onlyworks for full
    ## temp.gap <- getGap(temp.n, "model.group")
    ## temp.gap <- resize(temp.gap, width = width(temp.gap)+2L, fix = "center")
    ## gr.label <- getModelRange(temp.n)
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
  }
  if(geom == "single.arc"){
    ## getting a single model
    exon_id <- "exon_id"
    data <- model.new
    gr <- unlist(data)
    exon.id <- unique(as.character(values(gr)[,exon_id]))
    model.single <- gr[match(exon.id, as.character(values(gr)[,exon_id]))]
    model.single <- sort(model.single)
    sts.single <- as.numeric(start(model.single))
    eds.single <- as.numeric(end(model.single))
    exon.id.single <- as.character(values(model.single)[,exon_id])
    ## need to draw single model first
    p <- ggplot(as.data.frame(model.single))
    p <- p + geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 10))
    r.lv <- 
    lapply(model.new, function(gr){
      exon.this <- as.character(values(gr)[,exon_id])
      id <- match(exon.this, exon.id.single)
      sts.this <- sts.single[id]
      eds.this <- eds.single[id]
      r <- 2
      ## ggplot2::qplot(c(0,2), fun=sin, stat="function", geom="line")
      ggplot2::qplot(data = data.frame(x = c(0,2)),
                     fun=function(x) sin(acos(x/r))*r, stat="function", geom="line")
      ggplot(data  = data.frame(x=c(0, 10)), aes(x)) + 
               stat_function(fun=function(x) sin(x) )
    })
    
  }
  p  
}


  getGap <- function(data, group.name){
    res <- split(data, values(data)[,group.name])
    gps.lst <- lapply(res, function(x){
      gps <- gaps(ranges(x))
      if(length(gps)){
        gr <- GRanges(unique(as.character(seqnames(x))), gps)
        ## wait this request a .levels?
        values(gr)$.levels <- unique(values(x)$.levels)
        ## values(gr)$.model.group <- unique(values(x)$.model.group)
        if(".freq" %in% colnames(values(x)))
          values(gr)$.freq <- unique(values(x)$.freq)
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
  ## suppose we have freq?
  getModelRange <- function(data, group.name){
    seqs <- unique(as.character(seqnames(data)))
    ir <- unlist(range(ranges(split(data, values(data)[,group.name]),
                              ignore.strand = TRUE)))
    ## freqs <- values(data)$freq[match(names(ir), values(data)[,group.name])]
    .lvs <- values(data)$.levels[match(names(ir), values(data)[,group.name])]
    ## with levels
    gr <- GRanges(seqs, ir, .levels = .lvs)
    values(gr)$.label <- names(gr)
    gr
  }


