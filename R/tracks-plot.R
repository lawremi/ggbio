tracks <- function(...,
                   heights,
                   show.title = TRUE,
                   theme = NULL,
                   legend = FALSE,     #
                   xlim, ylim){
  dots <- list(...)
  nrow <- length(dots)
  if(missing(heights))
    heights <- unit(rep(1, nrow), "null")
  grobs <- dots
  if(missing(xlim)){
    lst <- lapply(grobs, function(obj){
      res <- getLimits(obj)
      data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
    })
    res <- do.call(rbind, lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    xlim <- scales::expand_range(xlim, mul = 0.05)
  }
  s <- scale_x_continuous(limits = xlim)    
  ## need fix legend
    N <- length(grobs)
    lst <- lapply(seq_len(N),
                  function(i) {
                    grobs[[i]] <- grobs[[i]] + s
                    if(!is.null(theme))
                      grobs[[i]] <- grobs[[i]] + theme
                    if(i %in% 1:(N-1)){
                      grobs[[i]] <- grobs[[i]] + 
                        ## scale_x_continuous(breaks = NA)+
                          opts(axis.text.x = theme_blank()) + xlab("")
                    }
                    ## if(i == 1){
                    ##   grobs[[i]] <- grobs[[i]] 
                    ##     ## opts(plot.margin = unit(c(1, 1.8, 0, 0), "lines"))
                    ## }else{
                    ##   grobs[[i]] <- grobs[[i]]
                    ##     opts(plot.margin = unit(c(0, 1.8, 0, 0), "lines"))
                    ##  }
                    ## if(!show.axis.text.y)
                    ##   grobs[[i]] <- grobs[[i]] + opts(axis.text.y = theme_blank())
                    ## if(!show.ticks)
                    ##   grobs[[i]] <- grobs[[i]] + opts(axis.ticks = theme_blank())
                    ## if(!legend)
                    ##   grobs[[i]] <- grobs[[i]] + opts(legend.position = "none")
                    grobs[[i]] 
                  })
  
  res <- do.call(align.plots, c(lst, list(heights = heights)))
  res
}

