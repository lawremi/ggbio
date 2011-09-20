## ======================================================================
##        For "Overview"
## ======================================================================

##' Plot stacked overview for genome with or without cytoband.
##'
##' This function requires two column of the \code{gieStain} and
##' \code{name}. Which you could get from \code{getIdeogram} function
##' in pacakge \code{biovizBase}.
##' @title Plot stacked overview for genome
##' @param obj A \code{GenomicRanges} object, which include extra
##' information about cytoband.
##' @param cytoband Logical value. Default is FALSE. If TRUE, plotting
##' cytoband.
##' @return A \code{ggplot} object.
##' @author Tengfei Yin
##' @examples
##' library(GenomicRanges)
##' data(hg19IdeogramCyto)
##' ## make shorter and clean labels
##' old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
##' new.chrs <- gsub("chr", "", old.chrs)
##' lst <- as.list(new.chrs)
##' names(lst) <- old.chrs
##' new.ideo <- renameSeqlevels(hg19IdeogramCyto, lst)
##' ## with cytoband
##' p <- plotOverview(new.ideo, cytoband = TRUE)
##' print(p)
##' ## with nocytoband
##' p <- plotOverview(new.ideo, cytoband = FALSE)
##' print(p)
plotOverview <- function(obj, 
                         cytoband = FALSE){

  if(cytoband){
    cytobandColor <- getOption("biovizBase")$cytobandColor
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
    if(!isSimpleIdeogram(obj)){
      message("Reduce to simple genome, ignoring cytoband")
      obj <- sortChr(reduce(obj))
      if(!isSimpleIdeogram(obj))
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

##' Plot single chromosome with cytoband
##'
##' User could provide the whole ideogram and use subchr to point to
##' particular chromosome.
##' @title Plot single chromosome with cytoband
##' @param obj A \code{GenomicRanges} object, which include extra
##' information about cytoband.
##' @param subchr A single character of chromosome names to show.
##' @param zoom.region A numeric vector of length 2 indicating zoomed
##' region.
##' @param xlabel A logical value. Show the x label or not.
##' @return A \code{ggplot} object.
##' @author Tengfei Yin
##' @examples
##' data(hg19IdeogramCyto)
##' vp1 <- viewport(width = 1, height = 0.14)
##' p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1")
##' print(p, vp = vp1)
plotSingleChrom <- function(obj, subchr, zoom.region, xlabel = FALSE){
  ## do we need subchr here
  if(!missing(subchr)){
    keepSeqlevels(obj, "chr1")
    obj <- obj[seqnames(obj) == subchr]
    obj
    ## seqlevels(obj) <- sortChr(unique(as.character(seqnames(obj))))
    seqlevels(obj)
    names(seqlevels(obj))
  }
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
  if(!xlabel)
    p <- p + opts(axis.text.x = theme_blank(),
                  axis.title.x=theme_blank())
  p
}

##' Adding hotregion which is a \code{GRanges} object for stacked
##' overview (genome-wide)
##'
##' The overplayed region may contain single position which is not
##' interval, this will be plotted as segments instead of rectangle.
##' @title Adding hotregion for stacked overview (genome-wide)
##' @param data A \code{\link{GRanges}} object, which you want to
##' overlay on the stacked overview.
##' @param ... Extra parameters passed to geom in
##' ggplot2. e.g. aes(color = score)
##' @return A 'Layer'
##' @author Tengfei Yin
##' @examples
##' library(GenomicRanges)
##' data(hg19IdeogramCyto)
##' ## make shorter and clean labels
##' old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
##' new.chrs <- gsub("chr", "", old.chrs)
##' lst <- as.list(new.chrs)
##' names(lst) <- old.chrs
##' new.ideo <- renameSeqlevels(hg19IdeogramCyto, lst)
##' p <- plotOverview(new.ideo, cytoband = FALSE)
##' data(darned_hg19_subset500)
##' ## rename 
##' new.darned <- renameSeqlevels(darned_hg19_subset500, lst)
##' p <- p + geom_hotregion(new.darned)
##' print(p)
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
