## tracks
tracks <- function(..., check.xlim = TRUE,
                   remove.extraXlim = TRUE,
                   legend = FALSE,
                   xlim.fix, ylim.fix){
  ## sicne this works for GRanges, genomic data
  ## so just check simple field
  ## need to check if y is labeld, it need to be labeled
  dots <- list(...)
  params <- c("nrow", "ncol", "widths", "heights", "default.units", 
              "respect", "just")
  layout.call <- intersect(names(dots), params)
  params.layout <- dots[layout.call]
  
  if (is.null(names(dots))) 
    not.grobnames <- FALSE
  else not.grobnames <- names(dots) %in% layout.call

  grobs <- dots[!not.grobnames]
  if(missing(xlim.fix)){
    lst <- lapply(grobs, function(obj){
      data.frame(xmin = min(obj$data$start),xmax = max(obj$data$end))
    })
    res <- do.call("rbind", lst)
    xlim.fix <- c(min(res$xmin), max(res$xmax))
    extr <- 0.05*(diff(xlim.fix))
    xlim.fix <- c(c(xlim.fix[1] - extr), c(xlim.fix[2] + extr))
  }
  s <- scale_x_continuous(limits = xlim.fix)
  if(!missing(ylim.fix))
    s <- scale_y_continuous(limits = xlim.fix)
  ## need fix legend
  if(check.xlim){
    N <- length(grobs)
    ## leg.lst <- lapply(seq_len(N),
    ##                   function(i) {
    ##                       ggplotGrob(grobs[[i]] + opts(keep="legend_box"))
    ##                   })
    ##  ## one needs to provide the legend with a well-defined width
    ## legend=gTree(children=do.call("gList",leg.lst), cl="legendGrob")
    ## params.layout <- c(params.layout, list(legend = substitute(legend)))
    lst <- lapply(seq_len(N),
                  function(i) {
                    if(legend)
                      grobs[[i]] <- grobs[[i]] + s
                    else
                      grobs[[i]] <- grobs[[i]] + s +
                        opts(legend.position = "none")
                    if(i %in% 1:(N-1))
                      grobs[[i]] <- grobs[[i]]+opts(axis.text.x = theme_blank(),
                                                    axis.title.x=theme_blank(),
                                                    axis.ticks = theme_blank())
                    grobs[[i]]
                  })
    widthDetails.legendGrob <- function(x) unit(10, "cm")    
    ## grid.arrange(lst[[1]],lst[[2]], lst[[3]], legend = legend)
    res <- do.call(grid.arrange, c(lst, params.layout))
  }else{
    res <- grid.arrange(...)
  }
  invisible(lst)
}

