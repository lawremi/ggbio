setGeneric("layout_karyogram", function(data,...)
           standardGeneric("layout_karyogram"))
setMethod("layout_karyogram", "GRanges", 
          function(data,..., xlab, ylab, main,
                   facets = seqnames ~ .,
                   cytoband = FALSE,
                   geom = NULL, stat = NULL, ylim = NULL,
                   rect.height = 10,
                   stack.level = 0
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
              .ideo.range <- ylim
            }
            
            ## check facets
            if(cytoband){
              cytobandColor <- getOption("biovizBase")$cytobandColor
              ## cytobandColor <- getCytoColor(unique(values(data)$gieStain))
              ## TODO: change to
              if(!isIdeogram(data))
                stop("Need cytoband information, please check the getIdeogram function")
              df <- as.data.frame(data)
              df.rect <- subset(df, gieStain != "acen")
              df.tri <- subset(df, gieStain == "acen")
              df.tri.p <- df.tri[substr(df.tri$name, 1, 1) == "p",]
              df.tri.q <- df.tri[substr(df.tri$name, 1, 1) == "q",]
              ## p.ideo <- ggplot(df.rect)

              p.ideo <- list(do.call(ggplot2::geom_rect, c(list(data = df.rect),
                                              list(do.call(aes,list(xmin = as.name("start"),
                                                                    ymin =.ideo.range[1],
                                                                    xmax = as.name("end"),
                                                                    ymax = .ideo.range[2],
                                                                    fill = as.name("gieStain")))),
                                                           list(color = "black"))))

              p.ideo <- c(p.ideo,
                          ifelse(nrow(df.tri.p),
                                 list(geom_polygon(data = df.tri.p,
                                      do.call(aes,list(x = substitute(c(start, start, end)),
                                                       y = c(.ideo.range[1], .ideo.range[2],
                                                            mean(.ideo.range)),
                                                           fill = as.name("gieStain"))))),
                                 list(NULL)),
                          ifelse(nrow(df.tri.q),
                          list(geom_polygon(data = df.tri.q,
                                            do.call(aes,
                                                    list(x = substitute(c(start, end, end)),
                                                         y = c(mean(.ideo.range),
                                             .ideo.range[2], .ideo.range[1]),
                                                         fill = as.name("gieStain"))))),
                                 list(NULL)),
                          list(opts(axis.text.y = theme_blank(),
                                    axis.title.y=theme_blank(),
                                    axis.ticks = theme_blank(),
                                    panel.grid.minor = theme_line(colour = NA),
                                    panel.grid.major = theme_line(colour = NA)),
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
              df <- fortify(data)              
            if(geom == "rect"){
              args.aes.rect <- c(args.aes, list(xmin = substitute(start),
                                       xmax = substitute(end),
                                       ymin = .ideo.range[1],
                                       ymax = .ideo.range[2]))
              args.aes.seg <- c(args.aes, list(x = substitute(start),
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
            o <- opts(axis.text.y = theme_blank(),
                      axis.title.y=theme_blank(),
                      axis.ticks = theme_blank(),
                      panel.grid.minor = theme_line(colour = NA),
                      panel.grid.major = theme_line(colour = NA))
            p <- list(p, list(o))
          })

