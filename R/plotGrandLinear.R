plotGrandLinear <- function(obj, y, title, facet, 
                            size, shape, color, alpha,
                            ...,                            
                            geom = c("point", "line"),
                            color.type = c("twocolor", "identity", "seqnames"),
                            two.color = c("#0080FF", "#4CC4FF"),
                            cutoff = NULL,
                            cutoff.color = "red",
                            cutoff.size = 1,
                            legend = FALSE,
                            ## axis.text.x.angle = 0,
                            xlab = "Chromosome",
                            ylab = substitute(y),
##                           expression(-log[10](italic(p))),
                            theme_bw = TRUE){


  args.dots <- as.list(match.call(call = sys.call(sys.parent()))[-1])
  if(geom == "line")
    args.dots <- c(group = substitute(seqnames),
                   args.dots)
  color.type <- match.arg(color.type)
  geom <- match.arg(geom)
  ## adding extra spacing for compact view
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
    p <- do.call(ggplot2::qplot, args)
  }else{
    ## change lalter
    ## ggplot2::qplot(data = df, geom = geom, x = midpoint, y = pvalue)
    args.s <- args.dots[names(args.dots) %in% c("size", "color", "alpha", "shape")]
    args <- list(data = df, geom = geom,
                 x = substitute(midpoint),
                 y = substitute(y))
    args <- c(args, args.s)        
    p <- do.call(ggplot2::qplot, args)
  }
  if(theme_bw)
    p <- p + theme_bw()
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
  if(!missing(title))
    p <- p + opts(title = title)
  if(!missing(facet)){
    args.facet <- args.dots[names(args.dots) %in%
                            c("margins", "scales", "space",
                              "labeller", "as.table")]
    args.facet <- c(facets = facet,
                    args.facet)
    p <- p + do.call(facet_grid, args.facet)
  }
  p <- p +  scale_x_continuous(name = xlab,
                               breaks = ticks,
                               labels = names(ticks))
  ## make it default to remove minor grid line
  p <- p +  opts(panel.background=theme_blank(), 
                   panel.grid.minor=theme_blank())
  ## p <- p + opts(axis.text.x =
  ##               theme_text(angle = axis.text.x.angle, hjust = 0))
  p
}

