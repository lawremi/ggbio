setGeneric("stat_aggregate", function(data, ...) standardGeneric("stat_aggregate"))

setMethod("stat_aggregate", "GenomicRanges", function(data, by, FUN, start = NULL,
                                                      end = NULL, width = NULL,  y = NULL,
                                                      delta = NULL, ..., simplify = TRUE,
                                                      window = NULL, facets = NULL, 
                                                      type = c("mean", "median","max",
                                                        "min", "sum", "count", "identity"),
                                                      geom){



  args <- as.list(match.call(call = sys.call(sys.parent(2))))[-1]
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)

  args.non <- args.non[!names(args.non) %in% c("data", "facets", "y", "x")]
  facet <- .buildFacetsFromArgs(data, args.facets)
  grl <- splitByFacets(data, facets)

  args.extra <- args[!names(args) %in% c("start", "by", "FUN",
                                        "end", "data", "width", "delta",
                                        "simplify", "type", "geom", "window")]

  type <- match.arg(type)
  if(geom %in% c("boxplot"))
    type <- "identity"
  if(missing(FUN)){
    if(!(type %in% c("count", "identity")) & is.null(y))
      stop("need to provide y value for type ", type)
    .FUN <- switch(type,
                   identity = {
                     function(x){
                       x
                   }
                   },
                   mean = {
                     function(x){
                       if(length(x)){
                         mean(values(x)[, y])
                       }else{
                         NULL
                       }
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
    if(!is.null(window)){
        snm <- unique(seqnames(dt))
        seqs <- seq(from = min(start(dt)), to = max(start(dt)), by = window)
        ## seqs <- seqs[-length(seqs)]
        .by <- IRanges(start = seqs,
                       width = window)
      if(!geom %in% c("boxplot")){
        res <- aggregate(dt, by = .by, FUN = .FUN)
        df <- as.data.frame(.by)
        df$.value <- res
        df$.mid <- start(.by) + width(.by)/2
        df$seqnames <-snm
      }else{
        grq <- GRanges(snm, .by)
        idx <- findOverlaps(dt, grq, select = "first")
        values(dt)$.mid <- start(grq[idx]) + width(grq[idx])
        df <- as.data.frame(dt)
      }
    }else{
      if(!geom %in% c("boxplot")){
      res <- aggregate(dt,  FUN = .FUN, start = start,
                       end = end, width = width, frequency = frequency,
                       delta = delta, simplyfy = simplyfy)
      df <- as.data.frame(.by)
      df$.value <- res
      df$.mid <- start(.by) + width(.by)/2
      df$seqnames <-snm
    }else{
      .by <- IRanges(start = start, end = end, width = width)
      grq <- GRanges(snm, .by)
      idx <- findOverlaps(dt, grq, select = "first")
      values(dt)$.mid <- start(grq[idx]) + width(grq[idx])
      df <- as.data.frame(dt)
    }
    }
    df
  })

  res <- do.call(rbind, lst)


  if(!geom %in% c("boxplot")){
    args.aes$x <- substitute(.mid)
    args.aes$y <- substitute(.value)
  }else{
    args.aes$x <- substitute(as.factor(.mid))
    if(!"y" %in% names(args.aes))
      stop("for geom boxplot, y must be provied in aes()")
  }
  
    
  aes <- do.call(aes, args.aes)
  args.res <- c(list(data = res),
                list(aes),
                args.non)

  if(!geom %in% c("boxplot")){
    p <- do.call(stat_identity, args.res)
   }else{
     args.res <- args.res[!names(args.res) %in% "geom"]
     p <- do.call(stat_boxplot, args.res)
   }
  p <- c(list(p) , list(facet))
  p
})

