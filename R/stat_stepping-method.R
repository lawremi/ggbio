setGeneric("stat_stepping", function(data, ...) standardGeneric("stat_stepping"))

setMethod("stat_stepping", "GRanges", function(data, ..., facets = NULL,
                                               rect.height = 0.4,
                                               geom = c("rect",
                                                 "alignment", "segment")){

  if(rect.height <= 0 | rect.height >= 0.5)
    stop("rect.height must be a value in (0,0.5)")
  geom <- match.arg(geom)
  args <- as.list(match.call(call = sys.call(sys.parent(2)))[-1])
  args.aes <- parseArgsForAes(args)
  ## args.non <- parseArgsForNonAes(args)
  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  args <- args[!names(args) %in% c("data", "facets", "rect.height", "geom")]
  ## facet <- .buildFacetsFromArgs(data, args.facets)  
  p <- switch(geom,
              rect = geom_rect(data, ..., facets = facets, rect.height = rect.height,
                stat = "stepping"),
              alignment = geom_alignment(data, ..., facets = facets, rect.height = rect.height,
                stat = "stepping"),
              segment = .geom_segment(data, ..., facets = facets, rect.height = rect.height,
                stat = "stepping"))
  ## p <- c(list(p) , list(facet))
  p
  
})






