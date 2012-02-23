setGeneric("stat_coverage", function(data, ...) standardGeneric("stat_coverage"))

setMethod("stat_coverage", "GRanges", function(data, ..., xlim, facets = NULL, 
                                               geom = NULL){


  if(is.null(geom))
    geom <- "area"
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args$geom <- geom  
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  ## args.facets <- args.facets[names(args.facets) != "facets"]
  args.non <- args.non[!names(args.non) %in% c("data", "facets")]
  facet <- .buildFacetsFromArgs(data, args.facets)
  grl <- splitByFacets(data, facets)


  if(missing(xlim))
    xlim <- c(min(start(ranges(data))),
              max(end(ranges(data))))
  if(!length(facets))
    facets <- as.formula(~seqnames)
  allvars <- all.vars(as.formula(facets))
  allvars.extra <- allvars[!allvars %in% c(".", "seqnames")]
  lst <- lapply(grl, function(dt){
    vals <- coverage(keepSeqlevels(dt, unique(as.character(seqnames(dt)))))
    if(any(is.na(seqlengths(dt)))){
      seqs <- xlim[1]:max(end(dt))
      vals <- vals[[1]][seqs]
      vals <- as.numeric(vals)                           
      vals <- c(vals, rep(0, xlim[2]-max(end(dt))))
      seqs <- xlim[1]:xlim[2]
    }else{
      seqs <- xlim[1]:xlim[2]
      vals <- vals[[1]][seqs]
      vals <- as.numeric(vals)                           
    }
    if(geom == "area"){
      seqs <- c(seqs[1], seqs, seqs[length(seqs)])
      vals <- c(0, vals, 0)
    }

    if(length(unique(values(dt)$.id.name)))                
      res <- data.frame(vals = vals, seqs = seqs,
                        seqnames =
                        as.character(seqnames(dt))[1],
                        .id.name = unique(values(dt)$.id.name))
    else
      res <- data.frame(vals = vals, seqs = seqs,
                        seqnames =
                        as.character(seqnames(dt))[1])
    res[,allvars.extra] <- rep(unique(values(dt)[, allvars.extra]),
                               nrow(res))
    res
  })

  res <- do.call(rbind, lst)
  ## df <- fortify(data = res)
  args.aes <- args.aes[!(names(args.aes) %in% c("x", "y"))]
  args.aes <- c(args.aes, list(x = substitute(seqs),
                       y = substitute(vals)))
  aes <- do.call(aes, args.aes)
  args.res <- c(list(data = res),
                list(aes),
                args.non)
  p <- do.call(stat_identity, args.res)
  p <- c(list(p) , list(facet))
  p
})


