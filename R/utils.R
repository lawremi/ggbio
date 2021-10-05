setGeneric("getLimits", function(obj, ...) standardGeneric("getLimits"))
setMethod("getLimits", "GGbio", function(obj){
  .getLimits(obj@ggplot)
})
setMethod("getLimits", "ggplotPlot", function(obj){
  .getLimits(obj)
})
setMethod("getLimits", "ggbioPlot", function(obj){
  .getLimits(obj@ggplot)
})

.getLimits <- function(obj){

  x <- y <- xmin <- ymin <- xmax <- ymax <- xend <- yend <- NULL
  ## x
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval_tidy(obj$mapping$x, obj$data)
  if(!is.null(obj$mapping$x) && length(obj$data))
    x <- eval_tidy(obj$mapping$x, obj$data)
  ## y
  if(!is.null(obj$mapping$y) && length(obj$data))
    y <- eval_tidy(obj$mapping$y, obj$data)

  if(!is.null(obj$mapping$xmin) && length(obj$data))
    xmin <- eval_tidy(obj$mapping$xmin, obj$data)


  if(!is.null(obj$mapping$ymin) && length(obj$data))
    ymin <- eval_tidy(obj$mapping$ymin, obj$data)

  if(!is.null(obj$mapping$xmax) && length(obj$data))
    xmax <- eval_tidy(obj$mapping$xmax, obj$data)

  if(!is.null(obj$mapping$ymax) && length(obj$data))
    ymax <- eval_tidy(obj$mapping$ymax, obj$data)

  if(!is.null(obj$mapping$xend) && length(obj$data))
    xend <- eval_tidy(obj$mapping$xend, obj$data)

  if(!is.null(obj$mapping$yend) && length(obj$data))
    yend <- eval_tidy(obj$mapping$yend, obj$data)
  else
    yend <- NULL


  l.res <- suppressWarnings(getLimitsFromLayer(obj))

  res <- suppressWarnings(list(xlim = c(min(c(l.res$xmin, x, xmin), na.rm = TRUE),
                                 max(c(l.res$xmax,x, xmax, xend), na.rm = TRUE)),
                               ylim = c(min(c(l.res$ymin, y, ymin), na.rm = TRUE),
                                 max(c(l.res$ymax, y, ymax, yend), na.rm = TRUE))))

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
    x <- scal[[1]]
    if(!is.null(x$limits)){
      limits <- x$limits
      res <- NULL
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
  res <- data.frame(xmin = min(res$xmin, na.rm = TRUE),
                    xmax = max(res$xmax, na.rm = TRUE),
                    ymin = min(res$ymin, na.rm = TRUE),
                    ymax = max(res$ymax, na.rm = TRUE))
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
      x <- eval_tidy(layer$mapping$x, dt)
    else
      x <- NULL

    if(!is.null(layer$mapping$y))
      y <- eval_tidy(layer$mapping$y, dt)
    else
      y <- NULL

    if(!is.null(layer$mapping$xmin))
      xmin <- eval_tidy(layer$mapping$xmin, dt)
    else
      xmin <- NULL

    if(!is.null(layer$mapping$ymin))
      ymin <- eval_tidy(layer$mapping$ymin, dt)
    else
      ymin <- NULL

    if(!is.null(layer$mapping$xmax))
      xmax <- eval_tidy(layer$mapping$xmax, dt)
    else
      xmax <- NULL

    if(!is.null(layer$mapping$ymax))
      ymax <- eval_tidy(layer$mapping$ymax, dt)
    else
      ymax <- NULL

    if(!is.null(layer$mapping$xend))
      xend <- eval_tidy(layer$mapping$xend, dt)
    else
      xend <- NULL

    if(!is.null(layer$mapping$yend))
      yend <- eval_tidy(layer$mapping$yend, dt)
    else
      yend <- NULL

    res <- data.frame(xmin = min(c(x, xmin), na.rm = TRUE),
                      xmax = max(c(x, xmax, xend), na.rm = TRUE),
                      ymin = min(c(y, ymin), na.rm = TRUE),
                      ymax = max(c(y, ymax, yend), na.rm = TRUE))
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
  xlim <- ylim <- NULL
  if(is_quosure(x) & is_quosure(y)){
    xlim <- range(eval_tidy(x))
    ylim <- range(eval_tidy(y))
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

do.ggcall <- function(fun, args) {
    do.call(fun, filterArgs(fun, args))
}

filterArgs <- function(fun, args,
                       layerArgs=args[names(args) %in% c("geom", "stat")])
{
    resolveGeneric <- function(fun, args) {
        if (is(fun, "genericFunction")) {
            method <- selectMethod(fun, class(args$data))
            if (method@defined == "ANY") {
                ggfun <- get0(fun@generic, getNamespace("ggplot2"),
                              mode="function")
                if (!is.null(ggfun)) { # a generic overriding a ggplot2 function
                    fun <- ggfun
                }
            }
        }
        fun
    }
    fun <- resolveGeneric(fun, args)
    ggplot2 <- !is(fun, "genericFunction")
    if (ggplot2) {
        aes <- vapply(args, is, "uneval", FUN.VALUE=logical(1L))
        args[aes] <- lapply(args[aes], filterArgs, fun=fun, layerArgs=layerArgs)
        if (is.null(names(args))) {
            args <- args[aes]
        } else {
            args <- ggplot2:::rename_aes(args)
            layer <- do.call(fun, layerArgs)
            validArgs <- c(names(formals(fun)),
                           layer$geom$aesthetics(),
                           layer$stat$aesthetics(),
                           layer$geom$parameters(TRUE),
                           layer$stat$parameters(TRUE))
            args <- args[names(args) %in% validArgs | aes]
        }
    }
    args
}

.changeStrandColor <- function(p, args, fill = TRUE){
  strandColor <- getOption("biovizBase")$strandColor
  isStrand.color <- FALSE
  isStrand.fill <- FALSE
  ## default with no color
  idx <- c("color", "colour") %in% names(args)
  if((any(idx))){
    nms <- c("color", "colour")[idx][1]
    if(quo_name(args[[nms]]) == "strand")
      isStrand.color <- TRUE
  }
  if(("fill" %in% names(args))){
    if(quo_name(args$fill) == "strand")
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

sub_names <- function(data, name.expr){
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
    .res <- c(.res, subfun(res, name.expr))
  }
  .res
}

## subset chr
setGeneric("subsetByChrs", function(obj, ...) starndardGeneric("subByChr"))
setMethod("subsetByChrs", "GRanges", function(obj, subchr){
  if(missing(subchr))
    subchr <- as.character(seqnames(obj)[1])
  res <- obj[seqnames(obj) %in% subchr]
  res <- keepSeqlevels(res, subchr)
  res
})

setMethod("subsetByChrs", "Seqinfo", function(obj, subchr){
  if(missing(subchr))
    subchr <- as.character(seqnames(obj)[1])
  res <- obj[subchr]
  res
})




ggsave <- function (filename, plot = last_plot(),
                       device = default_device(filename), path = NULL, scale = 1,
                       width = par("din")[1], height = par("din")[2],
                       units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
{
    ## print(class(plot))
    if (!inherits(plot, "ggplot") & !is(plot, "Tracks"))
        stop("plot should be a ggplot2 plot or tracks object");
    eps <- ps <- function(..., width, height) grDevices::postscript(...,
        width = width, height = height, onefile = FALSE, horizontal = FALSE,
        paper = "special")
    tex <- function(..., width, height) grDevices::pictex(...,
        width = width, height = height)
    pdf <- function(..., version = "1.4") grDevices::pdf(...,
        version = version)
    svg <- function(...) grDevices::svg(...)
    wmf <- function(..., width, height) grDevices::win.metafile(...,
        width = width, height = height)
    emf <- function(..., width, height) grDevices::win.metafile(...,
        width = width, height = height)
    png <- function(..., width, height) grDevices::png(..., width = width,
        height = height, res = dpi, units = "in")
    jpg <- jpeg <- function(..., width, height) grDevices::jpeg(...,
        width = width, height = height, res = dpi, units = "in")
    bmp <- function(..., width, height) grDevices::bmp(..., width = width,
        height = height, res = dpi, units = "in")
    tiff <- function(..., width, height) grDevices::tiff(...,
        width = width, height = height, res = dpi, units = "in")
    default_device <- function(filename) {
        pieces <- strsplit(filename, "\\.")[[1]]
        ext <- tolower(pieces[length(pieces)])
        match.fun(ext)
    }
    units <- match.arg(units)
    convert_to_inches <- function(x, units) {
        x <- switch(units, `in` = x, cm = x/2.54, mm = x/2.54/10)
    }
    convert_from_inches <- function(x, units) {
        x <- switch(units, `in` = x, cm = x * 2.54, mm = x *
            2.54 * 10)
    }
    if (!missing(width)) {
        width <- convert_to_inches(width, units)
    }
    if (!missing(height)) {
        height <- convert_to_inches(height, units)
    }
    if (missing(width) || missing(height)) {
        message("Saving ", prettyNum(convert_from_inches(width *
            scale, units), digits = 3), " x ", prettyNum(convert_from_inches(height *
            scale, units), digits = 3), " ", units, " image")
    }
    width <- width * scale
    height <- height * scale
    if (limitsize && (width >= 50 || height >= 50)) {
        stop("Dimensions exceed 50 inches (height and width are specified in inches/cm/mm, not pixels).",
            " If you are sure you want these dimensions, use 'limitsize=FALSE'.")
    }
    if (!is.null(path)) {
        filename <- file.path(path, filename)
    }
    device(file = filename, width = width, height = height, ...)
    on.exit(capture.output(dev.off()))
    print(plot)
    invisible()
}


## combineAes(keep, lost)
combineAes <- function(keep, lose) {
    keep.nms <- names(keep)
    lose.nms <- names(lose)

    nms <- intersect(lose.nms, keep.nms)

    if (length(nms))
        return(c(keep, lose[setdiff(lose.nms, keep.nms)]))
    else
        return(c(keep, lose))
}

## mark a plot as a blank plot which doesn't


## ## add a new geom text
## ## fixme: hjust doesn't work
## btextGrob <- function (label,x = unit(0.5, "npc"), y = unit(0.5, "npc"),
##                        just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
##                        default.units = "npc", name = NULL, gp = gpar(), vp = NULL,  fx=1.1, fy= 1.5,
##                        fc = "white", alp = 1) {
## ##  require(grid)
##   if (!is.unit(x))
##     x <- unit(x, default.units)
##   if (!is.unit(y))
##     y <- unit(y, default.units)
##   grob(label = label, x = x, y = y, just = just, hjust = hjust,
##        vjust = vjust, rot = rot, check.overlap = check.overlap,
##        name = name, gp = gp, vp = vp, cl = "text")
##   w1 <- unit(1, "strwidth", "A")
##   tg <- textGrob(label = label, x = x - 0.5 * w1, y = y, just = just, hjust = hjust,
##                  vjust = vjust, rot = rot, check.overlap = check.overlap)
##   w <- unit(rep(1, length(label)), "strwidth", as.list(label))
##   h <- unit(rep(1, length(label)), "strheight", as.list(label))
##   rg <- rectGrob(x=x + 0.5* (1 - hjust) * w - 0.5*w1, y=y, width=fx*w, height=fy*h,
##                  gp=gpar(fill=fc, alpha=alp,  col=NA))

##   gTree(children=gList(rg, tg), vp=vp, gp=gp, name=name)
## }

## GeomText2 <- proto(ggplot2:::GeomText, {
##   objname <- "text2"

##   draw <- function(., data, scales, coordinates, ..., fc = "white", alp = 1,
##                    parse = FALSE, na.rm = FALSE) {
##     data <- remove_missing(data, na.rm,
##                            c("x", "y", "label"), name = "geom_text2")

##     lab <- data$label
##     if (parse) {
##       lab <- parse(text = lab)
##     }

##     with(coord_transform(coordinates, data, scales),
##          btextGrob(lab, x, y, default.units="native",
##                    hjust=hjust, vjust=vjust, rot=angle,
##                    gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt,
##                              fontfamily = family, fontface = fontface, lineheight = lineheight),
##                    fc = fc, alp = alp)
##     )
##   }

## })

## geom_text2 <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
##                         parse = FALSE,  ...) {
##   GeomText2$new(mapping = mapping, data = data, stat = stat,position = position,
##                 parse = parse, ...)
## }


zoomLevelToGeom <- function(zoomLevel, track = c("BSgenome",
                                           "VRanges")){
    track <- match.arg(track)
    .level1 <- 100 # text
    .level2 <- 500 # rect
    .level3 <- 2000 # segment
    geom <- switch(track,
                   "BSgenome" = {
                       if(zoomLevel < .level1){
                           g <- "text"
                       }else if(zoomLevel >= .level1 && zoomLevel < .level2){
                           g <- "rect"
                       }else if(zoomLevel >= .level2 && zoomLevel < .level3){
                           g <- "segment"
                       }else{
                           g <- "none"
                       }
                   },
                   "VRanges" = {
                       if(zoomLevel < .level1){
                           g <- "text"
                       }else if(zoomLevel >= .level1 && zoomLevel < .level3){
                           g <- "rect"
                       }else{
                           g <- "none"
                       }
                   })
    geom
}

by2 <- function(...) {
    ans <- by(...)
    class(ans) <- "list"
    ans
}

# returns NULL_OR_list
# if x, y and main are missing then NULL will be returned.
Labels <- function(x, y, main, fallback) {
    labels <- c()
    xflag <- yflag <- mainflag <- FALSE
    if (!missing(fallback)) {
        if ("x" %in% names(fallback) && missing(x)) {
            xflag <- TRUE
            x <- fallback[["x"]]
        }
        if ("y" %in% names(fallback) && missing(y)) {
            yflag <- TRUE
            y <- fallback[["y"]]
        }
        if ("main" %in% names(fallback) && missing(main)) {
            mainflag <- TRUE
            main <- fallback[["main"]]
        }
    }

    # When flag is TRUE, label must be created
    if (!missing(x) || xflag) {
        stopifnot(is.character(x))
        labels <- c(labels, list(xlab(x)))
    }
    if (!missing(y) || yflag) {
        stopifnot(is.character(y))
        labels <- c(labels, list(ylab(y)))
    }
    if (!missing(main) || mainflag) {
        stopifnot(is.character(main))
        labels <- c(labels, list(labs(title = main)))
    }
    return(labels)
}

remove_args <- function(args, remove) {
    args[!names(args) %in% remove]
}

build_facet <- function(data, args) {
    args <- subsetArgsByFormals(args, facet_grid, facet_wrap)
    .buildFacetsFromArgs(data, args)
}

make_addStepping <- function(gr, args, group.selfish, ...) {
    if("group" %in% names(args)) {
        addStepping(gr, group.name = quo_name(args$group),
                    group.selfish = group.selfish, ...)
    } else {
        addStepping(gr, ...)
    }
}

group_df <- function(df, group) {
    .df.sub <- df[, c("stepping", group)]
    .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
}

scale_y_continuous_by_group <- function(df, group, group.selfish) {
     if(group != "stepping" & group.selfish) {
      list(scale_y_continuous(breaks = df$stepping,
                              labels = as.character(df[, group])))
    } else {
      list(scale_y_continuous(breaks = NULL))
    }
}
