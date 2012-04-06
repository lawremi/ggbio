plotGrandLinear <- function(obj, y, facets, 
                            size, shape, color, alpha,
                            ...,                            
                            geom = c("point", "line"),
                            color.type = c("twocolor", "identity", "seqnames"),
                            two.color = c("#0080FF", "#4CC4FF"),
                            cutoff = NULL,
                            cutoff.color = "red",
                            cutoff.size = 1,
                            legend = FALSE,
                            xlim, ylim, 
                            xlab = "Genomic Coordinates",
                            ylab = substitute(y),
                            main,
                            theme){

  geom <- match.arg(geom)
  args.dots <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  if(geom == "line")
    args.dots <- c(group = substitute(seqnames),
                   args.dots)
  color.type <- match.arg(color.type)
  df <- transformGRangesToDfWithTicks(obj)$df
  ticks <- transformGRangesToDfWithTicks(obj)$ticks
  idx <- order(df$midpoint)
  df <- df[idx,]
  chrs <- unique(as.character(df$seqnames))
  if(missing(y))
    stop("need to provide y")
  if(color.type %in% c("seqnames", "twocolor")){
    args <- list(data = df, geom = geom,
                 x = substitute(midpoint),
                 y = substitute(y), 
                 color = substitute(seqnames))
    args.s <- args.dots[names(args.dots) %in% c("size", "color", "alpha", "shape")]
    args <- c(args, args.s)
    p <- do.call(qplot, args)
  }else{
    args.s <- args.dots[names(args.dots) %in% c("size", "color", "alpha", "shape")]
    args <- list(data = df, geom = geom,
                 x = substitute(midpoint),
                 y = substitute(y))
    args <- c(args, args.s)
    p <- do.call(qplot, args)
    ## p + facet_grid(category ~ .)
    ## p + facet_grid( . ~ category)
  }
  if(!missing(theme))
    p <- p + theme
  if(!legend)
    p <- p + opts(legend.position = "none")

  p <- p + ylab(ylab) 
  if(!is.null(cutoff))
    p <-  p + geom_hline(yintercept = cutoff, color = cutoff.color,
                         size = cutoff.size)
  
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
  ## if(!missing(title))
  ##   p <- p + opts(title = title)
  if(!missing(facets)){
    args.facets <- args.dots[names(args.dots) %in%
                            c("margins", "scales", "space",
                              "labeller", "as.table")]
    args.facets <- c(facets = facets,
                    args.facets)
    p <- p + do.call(facet_grid, args.facets)
  }
  p <- p +  scale_x_continuous(name = xlab,
                               breaks = ticks,
                               labels = names(ticks))
  p <- p +  opts(panel.background=theme_blank(), 
                   panel.grid.minor=theme_blank())
  if(!missing(main))
    p <- p + opts(title = main)
  if(!missing(xlim))
    p <- p + xlim(xlim)
  if(!missing(ylim))
    p <- p + ylim(ylim)
  p
}


