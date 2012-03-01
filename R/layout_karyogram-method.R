setGeneric("layout_karyogram", function(data,...)
           standardGeneric("layout_karyogram"))
setMethod("layout_karyogram", "GRanges", 
          function(data,..., xlab, ylab, main,
                   facets = seqnames ~ ., cytoband = FALSE,
                   geom = NULL, stat = NULL, ylim = NULL,
                   rect.height = 10
                   ) {
              
            ## geom <- match.arg(geom)
            args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
            args <- args[!names(args) %in% c("data", "geom")]
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
                ## if("rect.height" %in% names(args.non))-
                  .ideo.range <- c(0, rect.height)
                ## else
                ##   .ideo.range <- c(0, 10)
              }
            }else{
              .ideo.range <- ylim
            }
            
            ## check facets
            if(cytoband){
              cytobandColor <- getOption("biovizBase")$cytobandColor
              ## TODO: change to
              if(!isIdeogram(data))
                stop("Need cytoband information, please check the getIdeogram function")
              df <- as.data.frame(data)
              df.rect <- subset(df, gieStain != "acen")
              df.tri <- subset(df, gieStain == "acen")
              df.tri.p <- df.tri[substr(df.tri$name, 1, 1) == "p",]
              df.tri.q <- df.tri[substr(df.tri$name, 1, 1) == "q",]
              p.ideo <- ggplot(df.rect)
              p.ideo <- p.ideo + facet_grid(seqnames ~ .) +
                geom_rect(aes(xmin = start,
                              ymin = .ideo.range[1],
                              xmax = end,
                              ymax = .ideo.range[2],
                              fill = gieStain),
                          color = "black")
              p.ideo <- p.ideo +  geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                                       y = c(rep(.ideo.range[1], length(start)),
                                         rep(.ideo.range[2],length(start)),
                                         rep(mean(.ideo.range),length(start))), fill = gieStain))
              p.ideo <- p.ideo +  geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                                       y = c(rep(mean(.ideo.range), length(start)),
                                         rep(.ideo.range[2],length(start)),
                                         rep(.ideo.range[1],length(start))), fill = gieStain))
              
              p.ideo <- p.ideo + geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                                      y = c(.ideo.range[1], .ideo.range[2],
                                        mean(.ideo.range)), fill = gieStain))+
                       geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                                      y = c(mean(.ideo.range),
                                        .ideo.range[2], .ideo.range[1]), fill = gieStain))+
                                        opts(axis.text.y = theme_blank(),
                                             axis.title.y=theme_blank(),
                                             axis.ticks = theme_blank(),
                                             panel.grid.minor = theme_line(colour = NA),
                                             panel.grid.major = theme_line(colour = NA))+
                                               scale_fill_manual(values = cytobandColor)
              
            }else {
              ideo.gr <- getIdeoGR(data)
              extra.factor <- setdiff(all.vars(as.formula(facets)), "seqnames")
              lst <- lapply(unique(values(data)[,extra.factor]), function(i){
                values(ideo.gr)[, extra.factor] <- i
                ideo.gr
              })
              ideo.gr <- do.call(c, lst)
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
            

            if(geom == "rect"){
              df <- as.data.frame(data)              
              args.aes <- c(args.aes, list(xmin = substitute(start),
                                       xmax = substitute(end),
                                       ymin = .ideo.range[1],
                                       ymax = .ideo.range[2]))
              if(any(c("colour", "fill") %in% names(args.aes))){
                if(!all(c("colour", "fill") %in% names(args.aes))){
                  idx <- which(c("colour", "fill") %in% names(args.aes))
                  known <- c("colour", "fill")[idx]
                  unknown <- c("colour", "fill")[-idx]
                  args.aes[[unknown]] <- args.aes[[known]]
                }
                p.addon <- do.call(ggplot2::geom_rect,
                                   c(list(data = df), list(do.call(aes, args.aes)),args.non))
              }else
                p.addon <- do.call(ggplot2::geom_rect,
                                   c(list(data = df), list(do.call(aes, args.aes)),args.non))
            }else{
            
            ## if(geom == "area"){
            ##   if("y" %in% names(args.aes)){
            ##     y.val <- eval(args.aes$y, df)
            ##     y.val.new <- rescale(y.val, to = c(0, 10))
            ##     df.temp <- df
            ##     df.temp[,as.character(args.aes$y)] <- y.val.new
            ##     df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
            ##   }
            ##   args <- c(args.aes, list(x = substitute(midpoint)))
            ##   p.addon <- geom_area(data = df.temp, do.call(aes, args))
            ## }
            
            ## if(geom == "line"){
            ##   if("y" %in% names(args.aes)){
            ##     y.val <- eval(args.aes$y, df)
            ##     y.val.new <- rescale(y.val, to = c(0, 10))
            ##     df.temp <- df
            ##     df.temp[,as.character(args.aes$y)] <- y.val.new
            ##     df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
            ##   }
            ##   args <- c(args.aes, list(x = substitute(midpoint)))
            ##   p.addon <- geom_line(data = df.temp, do.call(aes, args))
            ## }
            .drawFun <- getDrawFunFromGeomStat(geom, stat)
            aes.res <- do.call(aes, args.aes)
            args.res <- c(list(data = data), list(aes.res), args.non)
            p.addon <- do.call(.drawFun, args.res)
          }
            o <- opts(axis.text.y = theme_blank(),
                      axis.title.y=theme_blank(),
                      axis.ticks = theme_blank(),
                      panel.grid.minor = theme_line(colour = NA),
                      panel.grid.major = theme_line(colour = NA))
            p <- list(p.ideo,  p.addon , facet_grid(facets), o)            
            p
          })

