## TODO: exons label
## check (done)
## significant
##  log y, stat.col, stat.label, where is it.

setGeneric("plotRangesLinkedToData",
           function(data, ...) standardGeneric("plotRangesLinkedToData"))

setMethod("plotRangesLinkedToData", "RangedSummarizedExperiment",
          function(data, ...,
                   stat.y = seq_len(ncol(data)),
                   stat.ylab = names(assays(data)[stat.assay]),
                   stat.assay = 1L)
          {
            gr <- rowRanges(data)
            m <- assays(data, stat.assay)
            mcols(gr) <- cbind(mcols(gr), DataFrame(m))
            plotRangesLinkedToData(gr, stat.y = stat.y, stat.ylab = stat.ylab,
                                   ...)
          })

setMethod("plotRangesLinkedToData", "GenomicRangesORGRangesList",
          function(data, ...,
                   stat.y = seq_len(ncol(mcols(data))),
                   stat.ylab, sig, sig.col = c("black", "red"),
                   stat.coord.trans = coord_trans(),
                   annotation = list(), width.ratio = 0.8,
                   theme.stat = theme_gray(), theme.align = theme_gray(),
                   linetype = 3, heights)
          {
            args <- list(...)
            args.aes <- parseArgsForAes(args)
            args.non <- parseArgsForNonAes(args)
            ## args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
            ## facet <- .buildFacetsFromArgs(data, args.facets)
            if(missing(heights))
              heights <- unit(c(2.5, 0.5, 1, rep(1, length(annotation))), "null")
            gr <- transformGRangesForEvenSpace(data)
            wd <- width(range(gr))
            ## we need to find midpoint first
            N <- length(gr)
            wid <- wd/N/2 * width.ratio
            df <- as.data.frame(gr)
            ## FIXME: need to allow specified by stat.y
            ## head(df)
            if (is.character(stat.y)){
              if (all(stat.y %in% colnames(mcols(gr)))) {
                stat.y <- match(stat.y, colnames(mcols(gr)))
              } else {
                stop("if 'stat.y' is character, it must match column names ",
                     "in mcols(data)")
              }
            }
            if (!is.numeric(stat.y))  {
              stop("'stat.y' must be a numeric or character index into the ",
                   "columns of mcols(data)")
            }
            stat.y <- stat.y + 5
            df.new <- melt(df, measure.vars = stat.y)  
            stat.label <- colnames(df)[stat.y]
            df.new$.ggbio.group <- rep(stat.label, each = nrow(df))
            p <- ggplot(df.new)
            args.aes.seg <- args.aes[!names(args.aes) %in%
                                     c("x", "xend", "color", "y")]
            args.aes.seg <-
              c(args.aes.seg,
                c(list(x = substitute(x.new-wid, list(wid = wid)),
                       xend = substitute(x.new+wid, list(wid = wid)),
                       color = substitute(.ggbio.group),
                       y = substitute(value))))
            
            args.aes.seg$yend <- args.aes.seg$y
            aes.res.seg <- do.call(aes, args.aes.seg)
            p <- p + do.ggcall(ggplot2::geom_segment,
                             c(list(aes.res.seg), args.non))
            df.dash <- data.frame(x = df.new[c(-N, -2*N), "x.new"] + wid,
                                  xend = df.new[c(-1, -(N+1)), "x.new"] - wid,
                                  y = df.new[c(-N, -2*N), "value"],
                                  yend = df.new[c(-1, -(N+1)), "value"],
                                  .ggbio.group = df.new[c(-N, -2*N),
                                    ".ggbio.group"])
            args.dash.a <- args.aes[!names(args.aes) %in%  c("linetype", "y")]
            args.dash <- c(list(x = substitute(x),
                                y = substitute(y),
                                xend = substitute(xend),
                                yend = substitute(yend),
                                color = substitute(.ggbio.group)),
                           args.dash.a)
            p.stat <- p + do.call(ggplot2::geom_segment,
                                  c(list(data = df.dash),
                                    c(list(do.call(aes, args.dash)),
                                      linetype = linetype)))
            
            p.stat <- p.stat  +  theme.stat +
              theme(panel.grid.minor=element_blank()) +
              labs(colour = "group")
            p.stat <- p.stat + stat.coord.trans
            if(!missing(stat.ylab))
              p.stat <- p.stat + ylab(stat.ylab)
            ## link track
            df$midpoint <- (df$start + df$end)/2
            p.link <- ggplot(df)
            if(missing(sig)){
              args.link <- list(x = substitute(midpoint),
                                xend = substitute(x.new),
                                y = 0,
                                yend = 10)
              p.link <- p.link + ggplot2::geom_segment(do.call(aes, args.link)) +
                theme_null()    
            }else{
              args.link <- list(x = substitute(midpoint),
                                xend = substitute(x.new),
                                y = 0,
                                yend = 10,
                                color = as.name(sig))
              p.link <- p.link + ggplot2::geom_segment(do.call(aes, args.link)) +
                theme_null()        
              p.link <- p.link +
                scale_color_manual(values = c("FALSE" = sig.col[1],
                                     "TRUE" = sig.col[2])) 
            }
            p.link <- p.link + theme(legend.position = "none")
            
            ## grl <- GRangesList(data)
            if(missing(sig)){
              ## grl
              p.single <- autoplot(data, geom = "alignment") + theme.align +
                scale_y_continuous(breaks = NULL)
            }else{
              args.sig <- c(list(object = data),
                            list(do.call(aes, list(fill = as.name(sig),
                                                   color = as.name(sig)))),
                            list( geom = "alignment"))
              p.single <- do.call(autoplot, args.sig) +
                theme.align + scale_y_continuous(breaks = NULL)    
              p.single <- p.single +
                scale_color_manual(values = c("FALSE" = sig.col[1],
                                     "TRUE" = sig.col[2])) +
                  scale_fill_manual(values = c("FALSE" = sig.col[1],
                                      "TRUE" = sig.col[2])) +
                    theme(legend.position = "none")
            }
            p.link <- p.link +   theme(plot.margin = unit(c(0, 1,
                                         0,  0.5), "lines"),
                                       panel.margin = unit(c(0, 0.25, 0, 0.25),
                                         "lines")
                                       ) 
            
            if(length(annotation)){
              ## annotation <- lapply(annotation, function(p) p + theme_bw())
              ## tracks(p.stat,p.link,p.single)
              args.tracks <- c(c(list(p.stat, p.link, p.single), annotation),
                               list(heights = heights))
            }else{
              args.tracks <- c(list(p.stat, p.link, p.single),
                               list(heights = heights))
            }
            do.call(tracks, args.tracks)
          })




