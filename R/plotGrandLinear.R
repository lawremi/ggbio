plotGrandLinear <- function(obj, y, ..., facets, space.skip = 0.01, geom = NULL,
                            color.type = c("twocolor", "identity", "seqnames"),
                            two.color = c("#0080FF", "#4CC4FF"),
                            cutoff = NULL, cutoff.color = "red",
                            cutoff.size = 1, legend = FALSE, xlim, ylim, 
                            xlab = "Genomic Coordinates",
                            ylab = substitute(y), main, theme){

  if(is.null(geom))
    geom <- "point"
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  color.type <- match.arg(color.type)
  if(missing(y))
    stop("need to provide y")
  else
    args.aes$y <- as.name(deparse(substitute(y)))
  args.non$coord <- "genome"
  args.non$space.skip <- space.skip
  args.non$geom <- geom
  args.non$object <- obj
  if(color.type %in% c("seqnames", "twocolor")){
    args.aes$color <- substitute(.ori.seqnames)
    aes.res <- do.call(aes, args.aes)
      p <- do.call(autoplot, c(list(aes.res), args.non))
  }else{
        aes.res <- do.call(aes, args.aes)    
      p <- do.call(autoplot, c(list(aes.res), args.non))
  }
  if(!missing(theme))
    p <- p + theme
  if(!legend)
    p <- p + opts(legend.position = "none")

  p <- p + ylab(ylab) 
  if(!is.null(cutoff))
    p <-  p + geom_hline(yintercept = cutoff, color = cutoff.color,
                         size = cutoff.size)

  chrs <- names(seqlengths(obj))
  if(color.type == "twocolor"){
    N <- length(chrs)
    id1 <- seq(from = 1, by = 2, to = N)
    id2 <- seq(from = 2, by = 2, to = N)
    color1 <- two.color[1]
    color2 <- two.color[2]
    cols <- c(rep(color1, length(id1)), rep(color2, length(id2)))
    names(cols) <- c(chrs[id1], chrs[id2])
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


