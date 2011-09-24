##' In most genome browsers, they all have such a view that including
##' many tracks, could be any anntation data along genomic
##' coordinate. So we try to provide a convenient constructor for
##' building tracks, which here in this package is simply vertically
##' binding of several plots. It's essentially a
##' \code{grid.arrange}. So if users want to have more delicate
##' control over their tracks, they need manipulate the graphics in
##' ggplot2 level or grid levels.
##'
##' \code{tracks} function has some extra features and limitations
##' compare to \code{grid.arrange}.
##' \itemize{
##' \item{}{
##' Always sitting on genomic or protein space.
##' }
##' \item{}{
##' Always using ncol = 1 as default arguments.
##' }
##' \item{}{
##' For now, since the unbalanced legend and labels in ggplot2 has
##'   been solved (maybe just I haven't found such features). We simply
##'   remove legend and y axis labels to make sure all tracks are aligned
##'   exactly in the same way.
##' }
##' \item{}{
##' Remove the x-axis for most track except the last one.
##' } 
##' \item{}{
##' Does the ajustment of margins for you automatically.
##' }
##' \item{}{
##'   Doesn't like \code{qplot}, tracks doesn't return \code{ggplot}
##'   object. so processing your plot before you pass them to
##'   \code{tracks}.
##' }
##' }
##' @title Tracks for genomic graphics
##' @param ... Plots of class ggplot2, trellis, or grobs, and valid
##'          arguments to grid.layout.
##' @param check.xlim Default is TRUE, make sure all tracks are on the
##' same scale. if FALSE, just like common grid.arrange function.
##' @param legend Default is FALSE, remove legend, this make sure all
##' tracks are aligned exactly based on the same position.
##' @param xlim Limits on x.
##' @param ylim Limits on y.
##' @seealso \code{\link{grid.arrange}}
##' @return return a frame grob; side-effect (plotting) if plot=T.
##' @author Tengfei Yin
##' @examples
##' library(BSgenome.Hsapiens.UCSC.hg19)
##' gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
##' p1 <- qplot(Hsapiens, name = gr, geom = "text")
##' p2 <- qplot(Hsapiens, name = gr, geom = "point")
##' p3 <- qplot(Hsapiens, name = gr, geom = "segment")
##' p4 <- qplot(Hsapiens, name = gr, geom = "rectangle")
##' tracks(p1, p2, p3, p4)
tracks <- function(..., check.xlim = TRUE,
                   legend = FALSE,
                   xlim, ylim){
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
  
  if(missing(xlim)){
    lst <- lapply(grobs, function(obj){
      x <- eval(obj$layers[[1]]$mapping$x, obj$data)
      if(!is.null(x))
        data.frame(xmin = min(x),xmax = max(x))
      else
        data.frame(xmin = min(obj$data$start),xmax = max(obj$data$end))
    })
    res <- do.call("rbind", lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    extr <- 0.05*(diff(xlim))
    xlim <- c(c(xlim[1] - extr), c(xlim[2] + extr))
  }
  s <- scale_x_continuous(limits = xlim)
  if(!missing(ylim))
    s <- scale_y_continuous(limits = ylim)
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

