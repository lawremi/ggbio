getLimits <- function(obj){
  x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
  ## x
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval(obj$mapping$x, obj$data)
  ## y
  if(!is.null(obj$mapping$y) && length(obj$data))
    y <- eval(obj$mapping$y, obj$data)
  
  if(!is.null(obj$mapping$xmin) && length(obj$data))
    xmin <- eval(obj$mapping$xmin, obj$data)

  
  if(!is.null(obj$mapping$ymin) && length(obj$data))
    ymin <- eval(obj$mapping$ymin, obj$data)
  
  if(!is.null(obj$mapping$xmax) && length(obj$data))
    xmax <- eval(obj$mapping$xmax, obj$data)
  
  if(!is.null(obj$mapping$ymax) && length(obj$data))
    ymax <- eval(obj$mapping$ymax, obj$data)
  
  if(!is.null(obj$mapping$xend) && length(obj$data))
    xend <- eval(obj$mapping$xend, obj$data)
  
  if(!is.null(obj$mapping$yend) && length(obj$data))
    yend <- eval(obj$mapping$yend, obj$data)
  else
    yend <- NULL

  ## if(length(obj$layer)>1){
  l.res <- getLimitsFromLayer(obj)
  
  res <- suppressWarnings(list(xlim = c(min(c(l.res$xmin, x, xmin)),
                                 max(c(l.res$xmax,x, xmax, xend))),
                               ylim = c(min(c(l.res$ymin, y, ymin)),
                                 max(c(l.res$ymax, y, ymax, yend)))))
  
  if(any(unlist(res) %in% c(Inf, -Inf)))
    res <- evalLan(obj)
  
  if(length(obj$coordinates$limits$x) == 2)
    res$xlim <- obj$coordinates$limits$x
  
  if(length(obj$coordinates$limits$y) == 2)
    res$ylim <- obj$coordinates$limits$y
  ## scales
  l.res.s <- suppressWarnings(getLimitsFromScales(obj))
  l.res.s <- as.list(l.res.s)
  l.res.s <- lapply(l.res.s, function(x){
    if(x %in% c(-Inf, Inf)){
      NULL
    }else{
      x
    }
  })
  if(!is.null(l.res.s$xmin) & !is.null(l.res.s$xmax))
    res$xlim <- c(l.res.s$xmin, l.res.s$xmax)
  if(!is.null(l.res.s$ymin) & !is.null(l.res.s$ymax))
    res$ylim <- c(l.res.s$ymin, l.res.s$ymax)

  res

}

getLimitsFromScales <- function(obj){
  scal <- obj$scales$scales
  lst <- lapply(scal, function(x){
    if(!is.null(x$limits)){
      limits <- x$limits
    if(any(x$aesthetics %in% c("x", "xmin", "xmax", "xend", "xintercept",
                               "xmin_final", "xmax_final"))){
      res <- data.frame(xmin = limits[1],
                        xmax = limits[2],
                        ymin = NA,
                        ymax = NA)
    }
      if(any(x$aesthetics %in% c("y", "ymin", "ymax", "yend",
                                 "yintercept", "ymin_final", "ymax_final"))){
      res <- data.frame(ymin = limits[1],
                        ymax = limits[2],
                        xmin = NA,
                        xmax = NA)
      }
  }else{
    res <- NULL
  }
    res
  })
  lst <- lst[!is.null(lst)]
  res <- do.call("rbind", lst)
  res <- data.frame(xmin = min(res$xmin[!is.na(res$xmin)]),
                    xmax = max(res$xmax[!is.na(res$xmax)]),
                    ymin = min(res$ymin[!is.na(res$ymin)]),
                    ymax = max(res$ymax[!is.na(res$ymax)]))
  res
}

getLimitsFromLayer <- function(obj){
  layers <- obj$layer
  lst <- lapply(layers, function(layer){
    if(length(obj$data) | length(layer$data)){
      
    if(length(layer$data))
      dt <- layer$data
    else
      dt <- obj$data
    if(!is.null(layer$mapping)){
    if(!is.null(layer$mapping$x))
      x <- eval(layer$mapping$x, dt)
    else
      x <- NULL
    
    if(!is.null(layer$mapping$y))
      y <- eval(layer$mapping$y, dt)
    else
      y <- NULL
    
    if(!is.null(layer$mapping$xmin))
      xmin <- eval(layer$mapping$xmin, dt)
    else
      xmin <- NULL
    
    if(!is.null(layer$mapping$ymin))
      ymin <- eval(layer$mapping$ymin, dt)
    else
      ymin <- NULL
    
    if(!is.null(layer$mapping$xmax))
      xmax <- eval(layer$mapping$xmax, dt)
    else
      xmax <- NULL
    
    if(!is.null(layer$mapping$ymax))
      ymax <- eval(layer$mapping$ymax, dt)
    else
      ymax <- NULL
    
    if(!is.null(layer$mapping$xend))
      xend <- eval(layer$mapping$xend, dt)
    else
      xend <- NULL
    
    if(!is.null(layer$mapping$yend))
      yend <- eval(layer$mapping$yend, dt)
    else
      yend <- NULL
    
    res <- data.frame(xmin = min(c(x, xmin)),
                      xmax = max(c(x, xmax, xend)),
                      ymin = min(c(y, ymin)),
                      ymax = max(c(y, ymax, yend)))
  }else{
    res <- NULL
  }
  }else{
    res <- NULL
  }
  })
  lst <- lst[!is.null(lst)]
  res <- do.call("rbind", lst)
  res
}




evalLan <- function(obj){
  x <- obj$mapping$x
  y <- obj$mapping$y
  if(is.language(x) & is.language(y)){
    xlim <- range(eval(x))
    ylim <- range(eval(y))
  }
  list(xlim = xlim, ylim = ylim)
}




getGeomFun <- function(geom){
  match.fun(paste("geom_", geom, sep = ""))
}
getStatFun <- function(stat){
  match.fun(paste("stat_", stat, sep = ""))
}
getDrawFunFromGeomStat <- function(geom, stat){
  ## how about allways start from geom??
  if(!is.null(stat)){
    .fun <- getStatFun(stat)      
  }else{
    .fun <- getGeomFun(geom)
  }
  .fun
}

.changeStrandColor <- function(p, args, fill = TRUE){
  strandColor <- getOption("biovizBase")$strandColor
  isStrand.color <- FALSE
  isStrand.fill <- FALSE
  ## default with no color
  idx <- c("color", "colour") %in% names(args)
  if((any(idx))){
    nms <- c("color", "colour")[idx][1]
    if(as.character(args[[nms]]) == "strand")
      isStrand.color <- TRUE
  }
  if(("fill" %in% names(args))){
    if(as.character(args$fill) == "strand")
      isStrand.fill <- TRUE
  }
  if(isStrand.color)
    p <- c(list(p), list(scale_color_manual(values = strandColor)))
  if(fill){
    if(isStrand.fill)
      p <- c(p, list(scale_fill_manual(values = strandColor)))
  }
  p
}


## need to consider a length 1 facets formula
.buildFacetsFromArgs <- function(object, args){
  isOneSeq <- length(unique(as.character(seqnames(object)))) == 1
  args.facets <- args
  args.facets$facets <- strip_formula_dots(args$facets)
  facets <- args.facets$facets
  if(length(facets)){
    ## allvars <- all.vars(as.formula(facets))
    ## if(length(allvars) == 1){
    biovizBase:::.checkFacetsRestrict(facets, object)
    if(is(facets, "GRanges")){
      args.facets$facets <- substitute(~.bioviz.facetid)
      ## ok, default is "free"
      if(!("scales" %in% names(args.facets)))
        args.facets$scales <- "free"
      facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                            TRUE, FALSE)
      if(facet.logic)
        facet <- do.call(facet_wrap, args.facets)
      else
        facet <- do.call(facet_grid, args.facets)
    }else{
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      allvars <- all.vars(as.formula(args.facets$facets))
      
      if(isOneSeq & biovizBase:::isFacetByOnlySeq(args.facets$facets)){
        facet <- NULL
      }else{
      facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                            TRUE, FALSE)
      if(facet.logic){
        facet <- do.call(facet_wrap, args.facets)
      }else{
        facet <- do.call(facet_grid, args.facets)
      }
      facet <- do.call(facet_grid, args.facets)
    }
    }}else{
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      args.facets$facets <- substitute(~seqnames)
      allvars <- all.vars(as.formula(args.facets$facets))
      
      if(isOneSeq & biovizBase:::isFacetByOnlySeq(args.facets$facets)){
        facet <- NULL
      }else{
        facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                              TRUE, FALSE)
        if(facet.logic){
          facet <- do.call(facet_wrap, args.facets)
        }else{
          facet <- do.call(facet_grid, args.facets)
        }
      }
    }
  facet
}

setGeneric("highlight", function(obj, ...) standardGeneric("highlight"))

setMethod("highlight", "numeric", function(obj, col = "red", fill = "red", alpha = 1){
  xmin <- range(obj)[1]
  xmax <- range(obj)[2]
  annotation_custom(grob = rectGrob(gp = gpar(fill = fill, col = col, alpha = alpha)),
                    xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf)
})

setMethod("highlight", "data.frame", function(obj, col = "red", fill = "red", alpha = 1){
  if(ncol(obj) != 2)
    stop("obj(data.frame) passed to hightlight must be of column number 2, the
          first column is xmin, and second column is xmax")
  xmin <- obj[,1]
  xmax <- obj[,2]
  lapply(seq_len(nrow(obj)), function(i){
    annotation_custom(grob = rectGrob(gp = gpar(fill = fill,
                                        col = col, alpha = alpha)),
                      xmin = xmin[i], xmax = xmax[i], ymin = -Inf, ymax = Inf)
  })
})

setMethod("highlight", "GRanges", function(obj, col = "red", fill = "red", alpha = 1){
  if(length(unique(as.character(seqnames(obj))))>1)
    stop("GRanges contains more than one chromosomes.")
  ir <- ranges(obj)
  df <- data.frame(start(ir), end(ir))
  highlight(df, col = col, fill = fill, alpha = alpha)
})

## matrix
scale_fill_fold_change<- function(){
  s <- scale_fill_gradient2(low = "blue", mid = "white", high = "red")
  res <- c(list(s), list(guides(fill = guide_colorbar())))
                       ## list(scale_x_continuous(expand = c(0, 0))),
                       ## list(scale_y_continuous(expand = c(0, 0))))
}

need_color <- function(args){
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if("color" %in% c(names(args.non),names(args.aes))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}




trans_seq <- function(unit = c("mb", "kb", "bp")){
  unit <- match.arg(unit)
  function(x){
    res <- switch(unit,
                  mb = {
                    x/1e6
                  },
                  kb = {
                    x/1000
                  },
                  bp = {
                    x
                  })
    res
  }
}

trans_seq_format<- function(unit = c("mb", "kb", "bp")){
  unit <- match.arg(unit)
  function(x){
    res <- switch(unit,
                  mb = {
                    x/1e6
                  },
                  kb = {
                    x/1000
                  },
                  bp = {
                    x
                  })
    paste(res, unit)
  }
}

trans_seq_rev<- function(unit = c("mb", "kb", "bp")){
  unit <- match.arg(unit)
  function(x){
    res <- switch(unit,
                  mb = {
                    x*1e6
                  },
                  kb = {
                    x*1000
                  },
                  bp = {
                    x
                  })
    res
  }
}

scale_x_sequnit <- function(unit = c("mb", "kb", "bp")){
  unit <- match.arg(unit)
  scale_x_continuous(breaks = trans_breaks(trans_seq(unit),
                                           trans_seq_rev(unit)),
                     labels = trans_format(trans_seq_format(unit), math_format(.x)))
}

get_digits <- function(x){
  floor(log10(x))
}


scale_by_xlim <- function(xlim, by.unit = TRUE){
    if(by.unit)
      .d <- max(xlim)
    else
      .d <- diff(xlim)
    if(.d > 1e6){
      res <- scale_x_sequnit("mb")
    }else if(.d <= 1e6 & .d > 1e3){
      res <- scale_x_sequnit("kb")
    }else{
      res <- scale_x_sequnit("bp")
    }
  res
}

sub_names <- function(data, name.expr){
  ## name.expr <- deparse(substitute(name.expr))
  .res <- c()  
  for(i in seq_len(nrow(data))){
    res <- data[i,]
    res <- as.list(res)
    res <- lapply(res, function(x) {
      if(is.numeric(x))
        return(as.character(as.name(x)))
      else
        return(as.character(x))
    })
    subfun <- function(res, name.expr){
      nm <- names(res[1])
      val <- res[[1]]
      name.expr <- gsub(nm, val, name.expr)
      if(!length(res) == 1)
        subfun(res[-1], name.expr)
      else
        return(name.expr)
    }
    .res <- c(subfun(res, name.expr), .res)
  }
  .res
}


getLegendGrob <- function(p){
  g = ggplotGrob(p)
  gg = grid::editGrob(getGrob(g, gPath("guide-box"), 
    grep=TRUE), vp=viewport())
}

arrangeGrobByParsingLegend <- function(..., nrow = NULL, ncol = NULL,
                                       widths = c(4, 1), legend.idx = NULL){
  lst <- list(...)
  if(length(lst) == 1 && is.list(lst[[1]]))
    lst <- lst[[1]]
  gg <- lapply(lst, getLegendGrob)  
  l.g <- lapply(lst, function(x){
    ggplotGrob(x + opts(legend.position = "none"))
  })
  
  if(!is.null(legend.idx))
    gg <- gg[legend.idx]
  gg2 <- do.call(arrangeGrob, c(gg, list(ncol = 1)))
  print(grid.arrange(do.call(arrangeGrob, c(l.g, list(nrow = nrow, ncol = ncol))),
                     gg2, ncol = 2, widths = widths))
}


scale_fill_giemsa <- function(fill = getOption("biovizBase")$cytobandColor){
  list(scale_fill_manual(values = fill))
}

