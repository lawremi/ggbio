setGeneric("geom_chevron", function(data, ...) starndGeneric("geom_chevron"))
setMethod("geom_chevron", "GenomicRanges", 
          function(data, ...,  offset = 0.1, facets = NULL,
                   stat = c("stepping", "identity"),
                   chevron.height = c(0.1, 0.8)){

            stat <- match.arg(stat)
            
            args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
            args.aes <- parseArgsForAes(args)
            args.non <- parseArgsForNonAes(args)
            args.non <- args.non[!names(args.non) %in% c("data", "offset", "facets",
                                                        "stat", "chevron.height")]            
            args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
            facet <- .buildFacetsFromArgs(data, args.facets)

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
              os <- rescale(res, chevron.height)
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
              if(!".levels" %in% colnames(values(data))){
                if(length(group.name))
                  data <- addSteppings(data, group.name = group.name)
                else
                  data <- addSteppings(data)
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
                                      y = substitute(.levels + y.offset),
                                      yend = substitute(.levels + yend.offset)))
              args.res <- c(list(data = df), list(do.call(aes, args)),
                            args.non)
              p <- do.call(geom_segment, args.res)

            }
            if(stat == "identity"){
              aes.lst <- args.aes
              if(! all(c("x", "xend", "y", "yend") %in% names(aes.lst))){
                stop("aes(x = , y = , xend = , yend = ) must be provided for stat  'identity', ")
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
              .y <- aes.lst$y
              .yend <- aes.lst$yend
              aes.lst$y <- substitute(y + y.offset, list(y = .y))
              aes.lst$yend <- substitute(yend + yend.offset, list(yend = .yend))
              args.res <- c(list(data = df), list(do.call(aes, aes.lst)),
                            args.non)
              p <- do.call(geom_segment, args.res)
            }
            p <- c(list(p) , list(facet))            
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

