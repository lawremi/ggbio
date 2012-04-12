plotGrandLinear <- function(obj, ..., facets, space.skip = 0.01, geom = NULL,
                            cutoff = NULL, cutoff.color = "red",
                            cutoff.size = 1, legend = FALSE, xlim, ylim, 
                            xlab = "Genomic Coordinates",
                            ylab = substitute(y), main, theme){

  if(is.null(geom))
    geom <- "point"


  
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  two.color <- c("#0080FF", "#4CC4FF")
  .is.seq <- FALSE
  if(!"colour" %in% names(args.aes)){
    if(!any(c("color", "colour") %in% names(args.non))){
      .color <- two.color
      args.aes$color <- as.name(".ori.seqnames")
       .is.seq <- TRUE      
    }else{
      if(length(args.non$color) > 1){
        .color <- args.non$color
        args.aes$color <- as.name(".ori.seqnames")
       .is.seq <- TRUE
        args.non <- args.non[!names(args.non) %in% c("colour", "color")]
      }
    }
  }else{
    if(as.character(args.aes$colour) == "seqnames")
      args.aes$colour <- as.name(".ori.seqnames")
  }


  if(!"y" %in% names(args.aes))
    stop("need to provide y")
  
  args.non$coord <- "genome"
  args.non$space.skip <- space.skip
  args.non$geom <- geom
  args.non$object <- obj


  aes.res <- do.call(aes, args.aes)
  p <- do.call(autoplot, c(list(aes.res), args.non))

  if(!missing(theme))
    p <- p + theme
  if(!legend)
    p <- p + opts(legend.position = "none")

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
  p <- p +  opts(panel.grid.minor=theme_blank())
  if(!missing(main))
    p <- p + opts(title = main)
  if(!missing(xlim))
    p <- p + xlim(xlim)
  if(!missing(ylim))
    p <- p + ylim(ylim)
  p
}


