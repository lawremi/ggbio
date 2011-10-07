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
                            xlab = "Chromosome",
                            ylab = substitute(y),
##                            expression(-log[10](italic(p))),
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
  p
}

transformGRangesToDfWithTicks <- function(gr, fixed.length = NULL){
  if(is.null(fixed.length)){
    fixed.length <- getSpaceByData(gr)    
  }else{
    if(is.null(names(fixed.length)))
      stop("Please name your fixed.length with seqnames")
  }
  chr.order <- unique(as.character(seqnames(gr)))
  sft <- getShiftSpace(fixed.length[chr.order])
  df <- as.data.frame(gr)
  df$midpoint <- (df$start + df$end)/2
  ticks.pos <- fixed.length/2
  ## ticks.pos <- data.frame(pos = ticks.pos, seqnames = )
  df <- shiftDfBySpace(df, sft)
  df$seqnames <- factor(as.character(df$seqnames),
                        levels = chr.order)
  ticks <- sft[names(ticks.pos)] + ticks.pos
  list(df = df, ticks = ticks)
}

## compute default length, data-wide
getSpaceByData <- function(gr){
  gr <- gr.snp
  grl <- split(gr, seqnames(gr))
  ## ifseqlengths(gr)
  res <- unlist(width(range(grl)))
  res
}



getSpaceBySeqlengths <- function(gr){
  res <- seqlengths(gr)
  if(any(is.na(res))){
    idx <- is.na(res)
    res <- getSpaceByData(gr)[names(res[idx])]
  }
  res
}

getShiftSpace <- function(val){
  N <- length(val)
  ## avoid integer overflow, covert to numeric
  res <- c(0, cumsum(as.numeric(val[-N])))
  res <- res + 1
  names(res) <- names(val)
  res
}

shiftDfBySpace <- function(df, space){
  ## GRanges need to be coerced to df
  ## overcome the integer limits of ranges
  ## make it vectorized
  chrs <- as.character(df$seqnames)
  shifts <- space[chrs]
  df$start <- df$start + shifts
  df$end <- df$end + shifts
  if("midpoint" %in% colnames(df))
    df$midpoint <- df$midpoint + shifts
  df
}


