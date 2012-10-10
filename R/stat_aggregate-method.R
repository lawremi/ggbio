setGeneric("stat_aggregate", function(data, ...) standardGeneric("stat_aggregate"))

setMethod("stat_aggregate", "GRanges", function(data, ..., xlab, ylab, main, by, FUN,
                                                maxgap=0L, minoverlap=1L,
                                                type=c("any", "start", "end", "within", "equal"),
                                                select=c("all", "first", "last", "arbitrary"),
                                                y = NULL,
                                                window = NULL, facets = NULL, 
                                                method = c("mean", "median","max",
                                                  "min", "sum", "count", "identity"),
                                                geom = NULL){




  type <- match.arg(type)
  select <- match.arg(select)
  
  if(is.null(geom))
    geom <- "histogram"

  if(is.null(window))
    window <- as.integer(width(range(ranges(data)))/20)

  args <- list(...)
  args$facets <- facets
  args$geom <- geom
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)

  if(!length(y) && "y" %in% names(args.aes))
    y <- as.character(args.aes$y)
    
  if(!("scales" %in% names(args.facets)))
    args.facets$scales <- "free_x"
  facet <- .buildFacetsFromArgs(data, args.facets)
  grl <- splitByFacets(data, facets)

  args.extra <- args[!names(args) %in% c("start", "by", "FUN",
                                        "end", "data", "width", "delta",
                                        "simplify", "type", "geom", "window")]

  method <- match.arg(method)


  if(geom %in% c("boxplot"))
    method <- "identity"

  if(missing(FUN)){
    if(!(method %in% c("count", "identity")) & is.null(y))
      stop("need to provide y value for method ", method)
    .FUN <- switch(method,
                   identity = {
                     function(x){
                       x
                   }
                   },
                   mean = {
                     function(x){
                         mean(values(x)[, y])
                     }
                   },
                   median = {
                     function(x){
                       median(values(x)[, y])
                     }
                   },
                   max = {
                     function(x){
                       max(values(x)[, y])
                     }
                   },
                   min = {
                     function(x){
                       min(values(x)[, y])
                     }
                   },
                   sum = {
                     function(x){
                       sum(values(x)[, y])
                     }
                   },
                   count = {
                     function(x){
                       length(x)
                     }
                   })
  }else{
    .FUN <- FUN
  }


  lst <- lapply(grl, function(dt){
        snm <- unique(seqnames(dt))
        seqs <- seq(from = min(start(dt)), to = max(end(dt)), by = window)
        .by <- IRanges(start = seqs,
                       width = window)
        if(select != "all"){
          hits <- findOverlaps(ranges(dt), .by, 
                               maxgap = maxgap, minoverlap = minoverlap,
                               type = type, select = select)
          res <- rep(NA, length(.by))
          names(res) <- c(1:length(.by))
          res2 <- unlist(lapply(split(dt, hits), .FUN))
          res[names(res2)] <- res2
        }else{
          hits <- findOverlaps(ranges(dt), .by, maxgap = maxgap, minoverlap = minoverlap,
                               type = type, select = select)
          res <- rep(NA, length(.by))
          names(res) <- c(1:length(.by))
          res2 <- unlist(lapply(base::by(as.data.frame(hits), subjectHits(hits), function(x){
            x[,1]
          }), function(id){
            x <- dt[id]
            .FUN(x)
          }))
          res[names(res2)] <- res2          
        }
      if(!geom %in% c("boxplot")){
        df <- as.data.frame(.by)
        df$.value <- res
        df$.mid <- start(.by) + width(.by)/2
        df$seqnames <-snm
      }else{
        if(select != "all"){
        grq <- GRanges(snm, .by)
        idx <- findOverlaps(dt, grq, maxgap = maxgap, minoverlap = minoverlap,
                               type = type, select = select)
        values(dt)$.mid <- start(grq[idx]) + width(grq[idx])
        df <- as.data.frame(dt)
      }else{
        grq <- GRanges(snm, .by)
        idx <- findOverlaps(dt, grq, maxgap = maxgap, minoverlap = minoverlap,
                               type = type, select = select)
        dt <- dt[queryHits(idx)]
        values(dt)$.mid <- start(grq[subjectHits(idx)]) +
          width(grq[subjectHits(idx)])
        df <- as.data.frame(dt)
      }
      }
       df 
  })
  res <- do.call(rbind, lst)
  if(".value" %in% colnames(res)  && all(is.na(res$.value))){
    stop("no hits found, please tweak with parametters select and type.")
  }
  if(!geom %in% c("boxplot", "histogram", "bar")){
    args.aes$x <- substitute(.mid)
    args.aes$y <- substitute(.value)
  }else{
    ## args.aes$x <- substitute(as.factor(.mid))
    args.aes$x <- substitute(factor(.mid))
    if(geom == "boxplot"){
    if(!"y" %in% names(args.aes))
      stop("for geom boxplot, y must be provied in aes()")
  }else{
    args.aes$y <- substitute(.value)
  }
  }
  aes.res <- do.call(ggplot2::aes, args.aes)
  args.res <- c(list(data = res),
                list(aes.res),
                args.non)
  if(!geom %in% c("boxplot")){
    p <- do.call(ggplot2::stat_identity, args.res)
   }else{
     args.res <- args.res[!names(args.res) %in% "geom"]
     p <- do.call(stat_boxplot, args.res)
   }
  p <- c(list(p) , list(facet))
  if(missing(xlab)) 
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))
  
  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  if(!missing(main))
    p <- c(p, list(labs(title = main)))
  p <- ggbio:::setStat(p)
  p
})

