setGeneric("geom_arrow", function(data, ...) standardGeneric("geom_arrow"))

setMethod("geom_arrow", "GenomicRanges", function(data, ...,
                                                  angle = 30,
                                               length = unit(0.15, "cm"),
                                               ends = "last", type = "open", 
                                                  stat = c("stepping", "identity"),
                                                  facets = NULL, N = 10){

  stat <- match.arg(stat)
  
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.non <- args.non[!names(args.non) %in% c("data", "facets", "N", "stat",
                                               "length", "angle", "ends", "type")]            
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)

  if(stat == "stepping"){
    grl <- splitByFacets(data, facets)
    res <- endoapply(grl,
                     function(dt){
                       if("group" %in% names(args.aes))
                         dt <- addSteppings(dt, group.name = as.character(args.aes$group))
                       else
                         dt <- addSteppings(dt)
                     })
    res <- unlist(res)
    ## df <- fortify(data = res)
    data <- res
    df <- as.data.frame(data)
    ## rest?
    lst <- apply(df, 1, function(x){
      x <- as.data.frame(t(x))
      res <- approx(c(as.numeric(as.character(x$start)), as.numeric(as.character(x$end))),
                    rep(as.numeric(as.character(x$.levels)), 2),n = N)
      ## res.x <- res$x
      res.df <- do.call(rbind,lapply(1:N, function(i){
        x
      }))
      ## head(res.df)
      res.df$temp.x <- res$x
      .res <- res.df[-N,]
      .res$temp.x2 <- res.df[-1, "temp.x"]
      .res
    })
    res <- do.call(rbind,lst)
    args.aes$x <- as.name("temp.x")
    args.aes$xend <- as.name("temp.x2")
    args.aes$y <- args.aes$yend <- as.name(".levels")
    args.non$arrow <- arrow(length = length, ends = ends, type = type, angle = angle)
    aes.temp <- do.call(aes, args.aes)
    p <- do.call(geom_segment, c(list(data = res), list(aes.temp), args.non))
  }
  if(stat == "identity"){
    ## df <- fortify(data = res)
    if(!all(names(args.aes) %in% c("x", "xend", "y", "yend")))
      stop("aes(x =, y =, xend = , yend = ) is required when stat is 'identity'")
    df <- as.data.frame(data)
    ## rest?
    lst <- apply(df, 1, function(x){

      x <- as.data.frame(t(x), stringsAsFactors = FALSE)
      res <- approx(c(as.numeric(as.character(x$start)), as.numeric(as.character(x$end))),
                    rep(0, 2),n = N)
      ## res.x <- res$x
      res.df <- do.call(rbind,lapply(1:N, function(i){
        x
      }))
      ## head(res.df)
      res.df$start <- res$x
      .res <- res.df[-N,]
      .res$end <- res.df[-1, "start"]
      cols <- colnames(df)[unlist(lapply(1:ncol(df),
                                          function(i) is.numeric(df[,i])))]

      .res[,cols] <- as.data.frame(data.matrix(.res[,cols]))
      .res
    })
    res <- do.call(rbind,lst)
    args.non$arrow <- arrow(length = length, ends = ends, type = type, angle = angle)
    aes.temp <- do.call(aes, args.aes)
    p <- do.call(geom_segment, c(list(data = res), list(aes.temp), args.non))
  }
  p <- c(list(p) , list(facet))  
  p
})

