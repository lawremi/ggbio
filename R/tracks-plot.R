tracks <- function(...,
                   show.axis.text.y = FALSE,
                   show.ticks = FALSE,
                   ## check.xlim = TRUE,
                   legend = FALSE,     #
                   xlim, ylim){
  ## FIXME: later.
  check.xlim <- TRUE
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
      res <- getLimits(obj)
      data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
    })
    res <- do.call(rbind, lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    xlim <- scales::expand_range(xlim, mul = 0.05)
  }
  s <- scale_x_continuous(limits = xlim)    
  if(!missing(ylim))
    s <- scale_y_continuous(limits = ylim)
  ## need fix legend
  if(check.xlim){
    N <- length(grobs)
    lst <- lapply(seq_len(N),
                  function(i) {
                    ## if(legend)
                    ##   grobs[[i]] <- grobs[[i]] + s
                    ## else
                    ##   grobs[[i]] <- grobs[[i]] + s +
                    ##     opts(legend.position = "none")
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
                    if(!show.axis.text.y)
                      grobs[[i]] <- grobs[[i]] + opts(axis.text.y = theme_blank())
                    if(!show.ticks)
                      grobs[[i]] <- grobs[[i]] + opts(axis.ticks = theme_blank())
                    if(!legend)
                      grobs[[i]] <- grobs[[i]] + opts(legend.position = "none")
                    grobs[[i]]
                  })
    ## need to do tricks with legend
    ## one needs to provide the legend with a well-defined width
    ## legend=gTree(children=gList(leg), cl="legendGrob")
    ## widthDetails.legendGrob <- function(x) unit(1, "cm")
    ## res <- do.call(grid.arrange, c(lst, params.layout,
    ##                                list(legend = legend)))
    res <- do.call(grid.arrange, c(lst, params.layout))
  }else{
    res <- grid.arrange(...)
  }
  res
}

