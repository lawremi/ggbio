plotGrandLinear <- function(obj, ..., facets, space.skip = 0.01, geom = NULL,
                            cutoff = NULL, cutoff.color = "red",
                            cutoff.size = 1, legend = FALSE, xlim, ylim, 
                            xlab, ylab, main,
                            highlight.gr = NULL,
                            highlight.name = NULL,
                            highlight.col = "red",
                            highlight.label = TRUE,
                            highlight.label.size = 5,
                            highlight.label.offset = 0.05,
                            highlight.label.col = "black",
                            spaceline = FALSE){

  if(is.null(geom))
    geom <- "point"

  
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  two.color <- c("#0080FF", "#4CC4FF")
  ## two.color <- c("gray20", "gray50")  
  .is.seq <- FALSE
  if(!"colour" %in% names(args.aes)){
    if(!any(c("color", "colour") %in% names(args.non))){
      .color <- two.color
      args.aes$color <- as.name("seqnames")
       .is.seq <- TRUE      
    }else{
      if(length(args.non$color) > 1){
        .color <- args.non$color
        args.aes$color <- as.name("seqnames")
       .is.seq <- TRUE
        args.non <- args.non[!names(args.non) %in% c("colour", "color")]
      }
    }
  }else{
    if(as.character(args.aes$colour) == "seqnames")
      args.aes$colour <- as.name("seqnames")
  }


  if(!"y" %in% names(args.aes))
    stop("need to provide y")
  
  args.non$coord <- "genome"
  args.non$space.skip <- space.skip
  args.non$geom <- geom
  args.non$object <- obj


  aes.res <- do.call(aes, args.aes)
  p <- do.call(autoplot, c(list(aes.res), args.non))

  if(!legend)
    p <- p + theme(legend.position = "none")

  if(!missing(ylab))
    p <- p + ylab(ylab) 
  if(!is.null(cutoff))
    p <-  p + geom_hline(yintercept = cutoff, color = cutoff.color,
                         size = cutoff.size)

  chrs <- names(seqlengths(obj))
  if(.is.seq){
    N <- length(chrs)
    cols <- rep(.color, round(N/length(.color)) + 1)[1:N]
    names(cols) <- chrs
    p <- p + scale_color_manual(values = cols)
  }
  
  if(!missing(facets)){
    args$facets <- facets
    args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- .buildFacetsFromArgs(obj, args.facets)
    p <- p + facet
  }
  p <- p +  theme(panel.grid.minor=element_blank())
  ## highlights
  
  if(!is.null(highlight.gr)){
    idx <- findOverlaps(obj, highlight.gr)
    .h.pos <- lapply(split(queryHits(idx), subjectHits(idx)), function(id){
      gr <- GRanges(as.character(seqnames(p@data))[id][1],
                    IRanges(start = min(start(p@data[id])),
                              end = max(end(p@data[id]))))
      val <- max(as.numeric(values(p@data[id])[,as.character(args.aes$y)])) 
      val <- val * (1 + highlight.label.offset)
      values(gr)$val <- val
      gr
    })
    .h.pos <- suppressWarnings(do.call("c", unname(.h.pos)))
    if(length(.h.pos)){
    if(is.null(highlight.name)){
      highlight.name <- names(highlight.gr)
    }else{
      highlight.name <- values(highlight.gr)[,highlight.name]
    }
    p <- p +  geom_point(data = mold(p@data[queryHits(idx)]),
                 do.call(aes, list(x = substitute(midpoint),
                                   y = as.name(args.aes$y))),
                         color = highlight.col)

    if(!is.null(highlight.name)){
   
  
      seqlevels(.h.pos, pruning.mode="coarse") <- seqlevels(obj)
      suppressWarnings(seqinfo(.h.pos) <- seqinfo(obj))
      .trans <- transformToGenome(.h.pos, space.skip = space.skip)
      values(.trans)$mean <- (start(.trans) + end(.trans))/2
      values(.trans)$names <- highlight.name
      p <- p + geom_text(data = mold(.trans), size = highlight.label.size,
                         vjust = 0, color = highlight.label.col,
                         do.call(aes, list(x = substitute(mean),
                                           y = as.name("val"),
                                           label = as.name("names"))))
    }

    }}
  if(spaceline){
    vline.df <- p@ggplot$data
    vline.df <- do.call(rbind, by(vline.df, vline.df$seqnames, function(dd){
      data.frame(start = min(dd$start),
                 end = max(dd$end))
    }))
    ## compute gap
    gap <- (vline.df$start[-1] + vline.df$end[-nrow(vline.df)])/2
    p <- p + geom_vline(xintercept = gap, alpha = 0.5, color = 'gray70') + theme(panel.grid = element_blank())
  }
  if(!missing(main))
    p <- p + labs(title = main)
  if(!missing(xlim))
    p <- p + xlim(xlim)
  if(!missing(ylim))
    p <- p + ylim(ylim)
  if(missing(xlab))
    xlab <- ""
  p <- p + ggplot2::xlab(xlab)
  p
}


