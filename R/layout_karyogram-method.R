setGeneric("layout_karyogram", function(data,...)
           standardGeneric("layout_karyogram"))
setMethod("layout_karyogram", "GRanges",
          function(data,..., xlab, ylab, main,
                   facets = seqnames ~ .,
                   cytoband = FALSE,
                   geom = "rect", stat = NULL, ylim = NULL,
                   rect.height = 10
                   ) {

            ## geom <- match.arg(geom)
            args <- list(...)
            args.aes <- parseArgsForAes(args)
            args.non <- parseArgsForNonAes(args)
            if(!"rect.height" %in% names(args.non))
              args.non$rect.height <- rect.height /2
            if(is.null(ylim)){
              ## compute y lim from data
              if("y" %in% names(args.aes)){
                .y <- values(data)[, as.character(args.aes$y)]
                .y.r <- range(.y)
                .ideo.range <- expand_range(.y.r, mul = 0.05)
              }else{
                .ideo.range <- c(0, rect.height)
              }
            }else{
              if("y" %in% names(args.aes)){
                .y <- values(data)[, as.character(args.aes$y)]
                .y <- scales::rescale(.y, to = ylim)
                .y.r <- range(.y)
                .ideo.range <- expand_range(.y.r, mul = 0.05)
                values(data)[, as.character(args.aes$y)] <- .y
              }else{
              .ideo.range <- ylim
            }
            }

            ## check facets
            if(cytoband){
              geom <- NULL
              cytobandColor <- getOption("biovizBase")$cytobandColor
              if(!isIdeogram(data))
                stop("Need cytoband information, please check the getIdeogram function")
              df <- mold(data)
              df.rect <- subset(df, gieStain != "acen")
              df.tri <- subset(df, gieStain == "acen")
              df.tri.p <- df.tri[substr(df.tri$name, 1, 1) == "p",]
              df.tri.q <- df.tri[substr(df.tri$name, 1, 1) == "q",]
              ## p.ideo <- ggplot(df.rect)
#               dd.lst <- split(data, seqnames(data))
              ## to make it pretty, it's not time efficient
#               e.cut <- min(unlist(lapply(dd.lst, function(dd){
#                 min(width(dd[1]), width(dd[length(dd)]))
#                 })))
#
#               df.left <- do.call(rbind, lapply(dd.lst, function(dd){
#                 dd.df <- mold(dd)
#                 dd.df.rect <- dd.df[order(dd.df$start),]
#                 dd.df.left <- dd.df.rect[1, ]
#                 dd.df.left$end <- dd.df.left$end - e.cut
#                 dd.df.left$width <- e.cut
#                 dd.df.left
#               }))
#               df.right <- do.call(rbind, lapply(dd.lst, function(dd){
#                 dd.df <- mold(dd)
#                 dd.df.rect <- dd.df[order(dd.df$start),]
#                 dd.df.right <- dd.df.rect[nrow(dd.df.rect), ]
#                 dd.df.right$start <- dd.df.right$end - e.cut
#                 dd.df.right$width <- e.cut
#                 dd.df.right
#               }))
#               df.rect <- do.call(rbind, lapply(dd.lst, function(dd){
#                 dd.df <- mold(dd)
#                 dd.df.rect <- dd.df[order(dd.df$start),]
#                 df.l <- dd.df.rect[1,]
#                 df.l$start <- df.l$start + e.cut
#                 df.r <- dd.df.rect[nrow(dd.df.rect), ]
#                 df.r$end <- df.r$end -e.cut
#                 dd.df.rect <- dd.df.rect[c(-1, -1 * nrow(dd.df.rect)), ]
#                 dd.df.rect <- rbind(df.l, dd.df.rect, df.r)
#               }))

              ## main
              p.ideo <- list(do.call(ggplot2::geom_rect, c(list(data = df.rect),
                                              list(do.call(aes,list(xmin = as.name("start"),
                                                                    ymin =.ideo.range[1],
                                                                    xmax = as.name("end"),
                                                                    ymax = .ideo.range[2],
                                                                    fill = as.name("gieStain")))),
                                                           list(color = NA, alpha = 0.7))))

              ## draw line
              df.p <- df.rect[substr(df.rect$name, 1, 1) == "p",]
              df.q <- df.rect[substr(df.rect$name, 1, 1) == "q",]

              if(nrow(df.p)){

                df.p.d <- do.call(rbind, by(df.p, df.p$seqnames, function(dd){
                  data.frame(x = min(dd$start),
                             y = .ideo.range[1],
                             y2 = .ideo.range[2],
                             xend = max(dd$end),
                             yend = .ideo.range[1],
                             yend2 = .ideo.range[2],
                             seqnames = unique(dd$seqnames))
                }))


                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.p.d),
                                                                          list(aes(x = x, y = y, xend = xend, yend = yend)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))
                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.p.d),
                                                                          list(aes(x = x, y = y2, xend = xend, yend = yend2)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))
                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.p.d),
                                                                          list(aes(x = x, y = y, xend = x, yend = y2)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))




              }

              if(nrow(df.q)){
                df.q.d <- do.call(rbind, by(df.q, df.q$seqnames, function(dd){
                  data.frame(x = min(dd$start),
                             y = .ideo.range[1],
                             y2 = .ideo.range[2],
                             xend = max(dd$end),
                             yend = .ideo.range[1],
                             yend2 = .ideo.range[2],
                             seqnames = unique(dd$seqnames))
                }))


                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.q.d),
                                                                          list(aes(x = x, y = y, xend = xend, yend = yend)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))
                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.q.d),
                                                                          list(aes(x = x, y = y2, xend = xend, yend = yend2)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))

                p.ideo <- c(p.ideo, list(do.call(ggplot2::geom_segment, c(list(data = df.q.d),
                                                                          list(aes(x = xend, y = y, xend = xend, yend = y2)),
                                                                          list(color = "black",
                                                                               alpha = 1, size = 0.3)))))





              }





              lst <- lapply(seq_len(nrow(df.tri.p)), function(i){
                with(df.tri.p[i,],
                     data.frame(x = start,
                                y = 0,
                                xend = start,
                                yend = 10,
                                height = abs(start - end),
                                seqnames = seqnames,
                                strand = strand,
                                name = name,
                                gieStain = gieStain)

                     )
              })
              df.tri.p2 <- do.call(rbind, lst)


              lst <- lapply(seq_len(nrow(df.tri.q)), function(i){
                with(df.tri.q[i,],
                     data.frame(x = end,
                                y = 0,
                                xend = end,
                                yend = 10,
                                height = - abs(start - end),
                                seqnames = seqnames,
                                strand = strand,
                                name = name,
                                gieStain = gieStain)

                     )
              })
              df.tri.q2 <- do.call(rbind, lst)

              ## border
              ##browser()
              p.ideo <- c(p.ideo,
                          ifelse(nrow(df.tri.p2),
                          list(do.call(geom_arch_flip2, c(list(data = df.tri.p2),
                                            list(aes(x = x,
                                                     y = y ,
                                                     xend = xend,
                                                     yend = yend,
                                                     height = height)
                                                ),
                                         list(color = "black", size = 0.5)))),
                                 list(NULL)))
              p.ideo <- c(p.ideo,
                          ifelse(nrow(df.tri.p2),
                                 list(geom_arch_flip(data = df.tri.p2,
                                                     aes(x = x,
                                                         y = y ,
                                                         xend = xend,
                                                         yend = yend,
                                                         height = height,
                                                         fill = gieStain
                                                       ))),
                                 list(NULL)))

            ## q
            p.ideo <- c(p.ideo,
                        ifelse(nrow(df.tri.q2),
                               list(do.call(geom_arch_flip2, c(list(data = df.tri.q2),
                                                                   list(aes(x = x,
                                                                            y = y ,
                                                                            xend = xend,
                                                                            yend = yend,
                                                                            height = height
                                                                     )),
                                                                   list(color = "black", size = 0.5)))),
                               list(NULL)))
             p.ideo <- c(p.ideo,
                          ifelse(nrow(df.tri.q2),list(geom_arch_flip(data = df.tri.q2,
                                            aes(x = x,
                                                y = y ,
                                                xend = xend,
                                                yend = yend,
                                                height = height,
                                                fill = gieStain))),
                                 list(NULL)))


              p.ideo <- c(p.ideo,
                          list(theme(axis.text.y = element_blank(),
                                    axis.title.y=element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.grid.minor = element_line(colour = NA),
                                    panel.grid.major = element_line(colour = NA)),
                               scale_fill_manual(values = cytobandColor)),
                          list(facet_grid(seqnames ~ .)))


            }else {
              ideo.gr <- getIdeoGR(data)
              extra.factor <- setdiff(all.vars(as.formula(facets)), c("seqnames", "."))
              if(length(extra.factor)){
                lst <- lapply(unique(values(data)[,extra.factor]), function(i){
                  values(ideo.gr)[, extra.factor] <- i
                  ideo.gr
                })
                ideo.gr <- do.call(c, lst)
              }
              names(ideo.gr) <- NULL
              df <- as.data.frame(ideo.gr)
              aes.ideo <- do.call(aes, list(xmin = substitute(start),
                                            ymin = .ideo.range[1],
                                            xmax = substitute(end),
                                            ymax = .ideo.range[2]))
              p.ideo <- do.call(ggplot2::geom_rect, c(list(data = df),
                                                      list(aes.ideo),
                                                      list(fill = "white", color = "black")))
            }
            if(!is.null(geom)){
              df <- mold(data)
            if(geom == "rect"){

              ## check xmin, ymin, ymax, y
              args.aes.rect <- combineAes(args.aes, list(xmin = substitute(start),
                                       xmax = substitute(end),
                                       ymin = .ideo.range[1],
                                       ymax = .ideo.range[2]))
              args.aes.seg <- combineAes2(args.aes, list(x = substitute(start),
                                               xend = substitute(start),
                                               y = .ideo.range[1],
                                               yend = .ideo.range[2]))

              ## this hack is to get over 1-pixel problem
              p.addon <- do.call(ggplot2::geom_segment,
                                 c(list(data = df), list(do.call(aes, args.aes.seg)),args.non))

              p.addon <- c(list(p.addon), list(do.call(ggplot2::geom_rect,
                          c(list(data = df), list(do.call(aes, args.aes.rect)),args.non))))
            }else{
              .drawFun <- getDrawFunFromGeomStat(geom, stat)
              aes.res <- do.call(aes, args.aes)
              args.res <- c(list(data = df), list(aes.res), args.non)
              p.addon <- do.call(.drawFun, args.res)
            }
            p <- list(p.addon , facet_grid(facets))
          }else{
            p <- list(p.ideo,  facet_grid(facets))
          }
            o <- theme(axis.text.y = element_blank(),
                      axis.title.y=element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.minor = element_line(colour = NA),
                      panel.grid.major = element_line(colour = NA),
                      strip.text.y=element_text(angle=0))
            p <- list(p, list(o), list(scale_x_sequnit()))
          })



## ## ======================================================================
## ##        For "Overview"
## ## ======================================================================
plotStackedOverview <- function(obj, ..., xlab, ylab, main, geom = "rect",
                         cytoband = FALSE, rescale = TRUE, rescale.range = c(0, 10)){
  args <- list(...)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  facets <- seqnames ~ .
  if(missing(obj)){
    obj <- getIdeogram(cytobands = cytobands)
    cat("-------get following seqnames------\n")
    message(paste(seqnames(seqinfo(obj)), collapse = "\n"))
    ## obj <- keepSeqlevels(obj, unique(seqnames()))
    idx <- order(seqlengths(obj), decreasing = TRUE)
    nms <- names(seqlengths(obj))[idx]
    obj <- keepSeqlevels(obj, nms)
    p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, facets = facets, geom =  NULL)
  }else{
  if(!is(obj, "GRanges"))
    stop("only GRanges supported now")
  ## tweak with y
  if(rescale){
  if("y" %in% names(args.aes)){
    values(obj)[, as.character(args.aes$y)] <-
      rescale(values(obj)[, as.character(args.aes$y)],rescale.range)

  }}
  p <- ggplot() + layout_karyogram(obj, cytoband = cytoband, facets = facets, geom = NULL)
  args.non$geom <- geom
  args.non$facets <- facets
  if(!cytoband){
    args.res <- c(list(data = obj), list(do.call(aes, args.aes)),args.non)
    p <- p + do.call(layout_karyogram,args.res)
  }
}
  if(!missing(xlab))
    p <- p + xlab(xlab)
  if(!missing(ylab))
    p <- p + ggplot2::ylab(ylab)
  if(!missing(main))
    p <- p + labs(title = main)

  p
}

plotKaryogram <- plotStackedOverview

