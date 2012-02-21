setGeneric("layout_karyogram", function(data,...)
           standardGeneric("layout_karyogram"))
setMethod("layout_karyogram", "GRanges", 
          function(data,..., xlab, ylab, main,
                   facets = seqnames ~ ., cytoband = FALSE,
                   geom = c("rectangle","line", "area")) {
            geom <- match.arg(geom)
            args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
            args <- args[!names(args) %in% c("data", "geom")]
            aes.lst <- parseArgsForAes(args)
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
                              ymin = 0,
                              xmax = end,
                              ymax = 10,
                              fill = gieStain),
                          color = "black")
              p.ideo <- p.ideo +  geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                                       y = c(rep(0, length(start)),
                                         rep(10,length(start)),
                                         rep(5,length(start))), fill = gieStain))
              p.ideo <- p.ideo +  geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                                       y = c(rep(5, length(start)),
                                         rep(10,length(start)),
                                         rep(0,length(start))), fill = gieStain))
              
              p.ideo <- p.ideo + geom_polygon(data = df.tri.p, aes(x = c(start, start, end),
                                      y = c(0, 10, 5), fill = gieStain))+
                       geom_polygon(data = df.tri.q, aes(x = c(start, end, end),
                                      y = c(5, 10, 0), fill = gieStain))+
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
              p.ideo <- 
                geom_rect(data = df, aes(xmin = start,
                              ymin = 0,
                              xmax = end,
                              ymax = 10), fill = "white", color = "black")
            }
            
            df <- as.data.frame(data)
            if(geom == "rectangle"){
              args <- c(aes.lst, list(xmin = substitute(start),
                                      xmax = substitute(end),
                                      ymin = 0,
                                      ymax = 10))
              if(any(c("colour", "fill") %in% names(args))){
                if(!all(c("colour", "fill") %in% names(args))){
                  idx <- which(c("colour", "fill") %in% names(args))
                  known <- c("colour", "fill")[idx]
                  unknown <- c("colour", "fill")[-idx]
                  args[[unknown]] <- args[[known]]
                }
                p.addon <- geom_rect(data = df, do.call(aes, args))
              }
              else
                p.addon <- geom_rect(data = df, do.call(aes, args), color = "black", fill = "black")
            }
            
            if(geom == "area"){
              if("y" %in% names(aes.lst)){
                y.val <- eval(aes.lst$y, df)
                y.val.new <- rescale(y.val, to = c(0, 10))
                df.temp <- df
                df.temp[,as.character(aes.lst$y)] <- y.val.new
                df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
              }
              args <- c(aes.lst, list(x = substitute(midpoint)))
              p.addon <- geom_area(data = df.temp, do.call(aes, args))
            }
            
            if(geom == "line"){
              if("y" %in% names(aes.lst)){
                y.val <- eval(aes.lst$y, df)
                y.val.new <- rescale(y.val, to = c(0, 10))
                df.temp <- df
                df.temp[,as.character(aes.lst$y)] <- y.val.new
                df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
              }
              args <- c(aes.lst, list(x = substitute(midpoint)))
              p.addon <- geom_line(data = df.temp, do.call(aes, args))
            }

            o <- opts(axis.text.y = theme_blank(),
                      axis.title.y=theme_blank(),
                      axis.ticks = theme_blank(),
                      panel.grid.minor = theme_line(colour = NA),
                      panel.grid.major = theme_line(colour = NA))
            p <- list(p.ideo,  p.addon , facet_grid(facets), o)            
            p
          })

