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
  facet <- .buildFacetsFromArgs(args.facets, facets)  
  grl <- splitByFacets(data, facets)
  res <- endoapply(grl,
                      function(dt){
                        if("group" %in% names(args.aes))
                          dt <- addSteppings(dt, group.name = as.character(args.aes$group))
                        else
                          dt <- addSteppings(dt)
                        })
  res <- unlist(res)
  df <- fortify(data = res)
  p <- switch(geom,
              rect = .geom_rect(df, args, rect.height),
              alignment = geom_alignment(df, res, args, rect.height),
              segment = .geom_segment(df, args, stat = "stepping"))

  p <- c(list(p) , list(facet))
  p
  
})






