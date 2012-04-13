setGeneric("geom_chevron", function(data, ...) standardGeneric("geom_chevron"))
setMethod("geom_chevron", "GRanges", 
          function(data, ...,
                   xlab, ylab, main,
                   offset = 0.1, facets = NULL,
                   stat = c("stepping", "identity"),
                   chevron.height.rescale = c(0.1, 0.8),
                   group.selfish = TRUE){

            stat <- match.arg(stat)
            
            args <- list(...)
            args$facets <- facets
            
            args.aes <- parseArgsForAes(args)            
            args.non <- parseArgsForNonAes(args)
            args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
            facet <- .buildFacetsFromArgs(data, args.facets)
            if(length(data)){
            getY <- function(n){
              switch(n,
                     {
                       y.offset <- 0
                       yend.offset <- offset
                       data.frame(y.offset = y.offset,
                                  yend.offset = yend.offset)
                     },
                     {
                       y.offset <- offset
                       yend.offset <- 0
                       data.frame(y.offset = y.offset,
                                  yend.offset = yend.offset)
                     })
            }
            getY2 <- function(df){
              res <- df[,offset]
              os <- rescale(res, chevron.height.rescale)
              lst <- lapply(1:nrow(df), function(i){
                n <- df[i,".bioviz.chevron"]
                switch(n,
                       {
                         y.offset <- 0
                         yend.offset <- os[i]
                         data.frame(y.offset = y.offset,
                                    yend.offset = yend.offset)
                       },
                       {
                         y.offset <- os[i]
                         yend.offset <- 0
                         data.frame(y.offset = y.offset,
                                    yend.offset = yend.offset)
                       })
              })
                do.call("rbind", lst)
              }
              
            if(stat == "stepping"){
              group.name <- NULL
              if("group" %in% names(args.aes))
                group.name <- as.character(args.aes$group)
              if(!"stepping" %in% colnames(values(data))){
                if(length(group.name))
                  data <- addStepping(data, group.name = group.name,
                                      group.selfish = group.selfish)
                else
                  data <- addStepping(data)
              }
              aes.lst <- args.aes
              data.new <- breakGr(data)
              names(data.new) <- NULL
              df <- as.data.frame(data.new)
              
              if(!is.numeric(offset)){
                offset <- as.character(offset)
                if(offset %in% colnames(values(data)))    
                  ydf <- getY2(df)
                else
                  stop("offset must be a numeric value or one of the colnames")
              }else{
                ydf <- do.call("rbind", lapply(df$.bioviz.chevron, getY))
              }
              df <- cbind(df, ydf)
              args <- c(aes.lst, list(x = substitute(start),
                                      xend = substitute(end),
                                      y = substitute(stepping + y.offset),
                                      yend = substitute(stepping + yend.offset)))
              args.res <- c(list(data = df), list(do.call(aes, args)),
                            args.non)
              p <- c(list(do.call(ggplot2::geom_segment, args.res)), list(ggplot2::ylab("")))

              if("group" %in% names(args.aes))
                gpn <- as.character(args.aes$group)
              else
                gpn <- "stepping"
              
              .df.lvs <- unique(df$stepping)
              .df.sub <- df[, c("stepping", gpn)]
              .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]

              if(gpn != "stepping" & group.selfish){
                p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                                   labels = as.character(.df.sub[, gpn]))))
              } else{
                p <- c(p, list(scale_y_continuous(breaks = NULL)))
              }

            }
            if(stat == "identity"){
              if(!"y" %in% names(args.aes)){
                if(!all(c("y","yend", "x", "xend") %in% names(args.aes))){
                  stop("aes(x =, xend= , y =, yend= ) is required for stat 'identity',
              you could also specify aes(y =) only as alternative")
                }
              }else{
                .y <- args.aes$y
                args.aes$x <- as.name("start")
                args.aes$xend <- as.name("end")
                args.aes$y <- substitute(y + offset,
                                            list(y = .y, offset = as.name("y.offset")))
                args.aes$yend <- substitute(yend + offset ,
                                            list(yend = .y, offset = as.name("yend.offset")))

              }
              
              data.new <- breakGr(data)
              names(data.new) <- NULL
              df <- as.data.frame(data.new)
              if(!is.numeric(offset)){
                offset <- as.character(offset)
                if(offset %in% colnames(values(data)))    
                  ydf <- getY2(df)
                else
                  stop("offset must be a numeric value or one of the colnames")
              }else{
                ydf <- do.call("rbind", lapply(df$.bioviz.chevron, getY))
              }
              df <- cbind(df, ydf)
              .y <- args.aes$y
              .yend <- args.aes$yend
              args.aes$y <- substitute(y + y.offset, list(y = .y))
              args.aes$yend <- substitute(yend + yend.offset, list(yend = .yend))
              args.res <- c(list(data = df), list(do.call(aes, args.aes)),
                            args.non)
              p <- c(list(do.call(ggplot2::geom_segment, args.res)),
                     list(ggplot2::ylab("")))

            }}else{
              p <- NULL
            }
            p <- c(list(p) , list(facet))            
            if(!missing(xlab))
              p <- c(p, list(ggplot2::xlab(xlab)))
            else
              p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))
            if(!missing(ylab))
              p <- c(p, list(ggplot2::ylab(ylab)))
            if(!missing(main))
              p <- c(p, list(opts(title = main)))
            p
          })

## 
breakGr <- function(gr){
  mids <- start(gr) + width(gr)/2
  res1 <- res2 <- gr
  end(res1) <- mids
  values(res1)$.bioviz.chevron <- 1
  start(res2) <- mids
  values(res2)$.bioviz.chevron <- 2
  res <- c(res1, res2)
}

