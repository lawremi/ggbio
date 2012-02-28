
## .geom_rect <- function(data, args, rect.height = 0.4, stat = c("stepping", "identity")){
##   stat <- match.arg(stat)
##   args.aes <- parseArgsForAes(args)
##   args.non <- parseArgsForNonAes(args)
##   rect.height <- force(rect.height)
##   if(stat == "stepping"){
##     args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "ymin", "ymax", "data"))]
##     args.non <- args.non[!(names(args.non) %in% c("xmin", "xmax", "ymax", "ymax", "data"))]
##     if("group" %in% names(args.aes))
##       gpn <- as.character(args$group)
##     else
##       gpn <- "stepping"
##     args.aes <- args.aes[names(args.aes) != "group"]
##     args.aes <- c(args.aes, list(xmin = substitute(start),
##                                  xmax = substitute(end),
##                                  ymin = substitute(stepping - rect.height),
##                                  ymax = substitute(stepping + rect.height)))
##     args.aes <- args.aes[names(args.aes) != "size"]
##     aes <- do.call(aes, args.aes)
##     args.res <- c(list(data = data), list(aes),
##                   args.non)
##     p <- list(do.call(geom_rect,args.res))
##     p <- .changeStrandColor(p, args.aes)
##     .df.lvs <- unique(data$stepping)
##     .df.sub <- data[, c("stepping", gpn)]
##     .df.sub <- .df.sub[!duplicated(.df.sub),]
##     if(gpn != "stepping")
##       p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
##                                          labels = as.character(.df.sub[, gpn]))))
##     else
##       p <- c(p, list(scale_y_continuous(breaks = NA)))
##   }
  
##   if(stat == "identity"){
##     ## args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "data"))]
##     ## args.non <- args.non[!(names(args.non) %in% c("xmin", "xmax", "data"))]
##     if("group" %in% names(args.aes))
##       gpn <- as.character(args$group)
##     else
##       gpn <- "stepping"
##     args.aes <- args.aes[names(args.aes) != "group"]
##     ## args.aes <- c(args.aes, list(xmin = substitute(start),
##     ##                              xmax = substitute(end)))
##     args.aes <- args.aes[names(args.aes) != "size"]
##     aes <- do.call(aes, args.aes)
##     args.res <- c(list(data = data), list(aes),
##                   args.non)
##     p <- list(do.call(geom_rect,args.res))
##     p <- .changeStrandColor(p, args.aes)
##   }
##    p
## }


## .geom_segment <- function(data, args, stat = c("identity", "stepping")){
##   stat <- match.arg(stat)
##   args.aes <- parseArgsForAes(args)
##   args.non <- parseArgsForNonAes(args)
##   if(stat == "stepping"){  
##     args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "data"))]
##     args.non <- args.non[!(names(args.non) %in% c("x", "y", "data"))]
##     if("group" %in% names(args.aes))
##       gpn <- as.character(args$group)
##     else
##       gpn <- "stepping"
##     args.aes <- args.aes[names(args.aes) != "group"]
##     args.aes <- c(args.aes, list(x = substitute(start),
##                                  xend = substitute(end),
##                                  y = substitute(stepping),
##                                  yend = substitute(stepping)))
##     args.aes <- args.aes[names(args.aes) != "size"]
##     aes <- do.call(aes, args.aes)
##     args.res <- c(list(data = data), list(aes),
##                   args.non)
##     p <- list(do.call(geom_segment,args.res))
##     p <- .changeStrandColor(p, args.aes)
##     .df.lvs <- unique(data$stepping)
##     .df.sub <- data[, c("stepping", gpn)]
##     .df.sub <- .df.sub[!duplicated(.df.sub),]
##     if(gpn != "stepping")
##       p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
##                                          labels = as.character(.df.sub[, gpn]))))
##     else
##       p <- c(p, list(scale_y_continuous(breaks = NA)))
##   }
##   if(stat == "identity"){
##     args.aes <- args.aes[!(names(args.aes) %in% c("x", "data"))]
##     args.non <- args.non[!(names(args.non) %in% c("x", "data"))]
##     if("group" %in% names(args.aes))
##       gpn <- as.character(args$group)
##     else
##       gpn <- "stepping"
##     args.aes <- args.aes[names(args.aes) != "group"]
##     args.aes <- c(args.aes, list(x = substitute(start),
##                                  xend = substitute(end)))
##     args.aes$yend <- args$y                               
##     args.aes <- args.aes[names(args.aes) != "size"]
##     aes <- do.call(aes, args.aes)
##     args.res <- c(list(data = data), list(aes),
##                   args.non)
##     p <- list(do.call(geom_segment,args.res))
##     p <- .changeStrandColor(p, args.aes)
##   }
##   p
## }


## geom_area <- function(data, ...){
##   args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
##   ## args <- force(args)
##   args.aes <- parseArgsForAes(args)
##   args.non <- parseArgsForNonAes(args)  
##   df.bg <- df.bg[order(df.bg$start),]
##   df.bg <- rbind(df.bg[1,], df.bg)
##   df.bg <- rbind(df.bg, df.bg[nrow(df.bg),])
##   df.bg[c(1, nrow(df.bg)),as.character(args.aes$y)] <- 0
##   args.res <- c(list(data = df.bg), do.call(aes, args.aes), args.non)
##   do.call(geom_polygon, args.res)
## }


