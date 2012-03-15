## FIXME: add ..coverage.., and a new way
setGeneric("stat_coverage", function(data, ...) standardGeneric("stat_coverage"))

setMethod("stat_coverage", "GRanges", function(data, ...,xlim,
                                               xlab, ylab, main,
                                               facets = NULL, 
                                               geom = NULL){


  if(is.null(geom))
    geom <- "area"
  data <- keepSeqlevels(data, unique(as.character(seqnames(data))))
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
  facets <- strip_facets_dots(facets)
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
      res <- data.frame(coverage = vals, seqs = seqs,
                        seqnames =
                        as.character(seqnames(dt))[1],
                        .id.name = unique(values(dt)$.id.name))
    else
      res <- data.frame(coverage = vals, seqs = seqs,
                        seqnames =
                        as.character(seqnames(dt))[1])
    res[,allvars.extra] <- rep(unique(values(dt)[, allvars.extra]),
                               nrow(res))
    res
  })
  res <- do.call(rbind, lst)
  if(!"y"  %in% names(args.aes))
    args.aes$y <- as.name("coverage")
  
  if(!"x"  %in% names(args.aes))
    args.aes$x <- as.name("seqs")

  aes <- do.call(aes, args.aes)
  args.res <- c(list(data = res),
                list(aes),
                args.non)
  
  p <- do.call(stat_identity, args.res)
  p <- c(list(p) , list(facet))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("Coverage")))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  
  p
})


setMethod("stat_coverage", "GRangesList", function(data, ..., xlim,
                                                   xlab, ylab, main,
                                                   facets = NULL, 
                                               geom = NULL){
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  aes.res <- do.call(aes, args.aes)
  gr <- flatGrl(data)
  args.non$data <- gr
  p <- do.call(stat_coverage, c(list(aes.res), args.non))
  if(!missing(xlab))
    p <- c(p, list(ggplot2::xlab(xlab)))
  else
    p <- c(p, list(ggplot2::xlab("Genomic Coordinates")))

  if(!missing(ylab))
    p <- c(p, list(ggplot2::ylab(ylab)))
  else
    p <- c(p, list(ggplot2::ylab("Coverage")))
  if(!missing(main))
    p <- c(p, list(opts(title = main)))
  p
})

