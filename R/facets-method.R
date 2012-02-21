setOldClass("formula")
setGeneric("splitByFacets", function(object, facets, ...) standardGeneric("splitByFacets"))

setMethod("splitByFacets", c("GRanges", "formula"), function(object, facets){
  .checkFacetsRestrict(facets)
  allvars <- all.vars(as.formula(facets))  
  if(allvars[1] == "."){
    res <- split(object, seqnames(object))
  }else{
    fts <- interaction(values(object)[,allvars[1]],values(object)[,allvars[2]])
    res <- split(object, fts)    
  }
  res
})

setMethod("splitByFacets", c("GRanges", "GRanges"), function(object, facets){
  if(length(facets)){
      object <- newDataAfterFacetByGr(object, facets)
      ## newDataAfterFAcetByGr will give a .bioviz.facetid
      if(args.facets$facets == "~.bioviz.facetid")
        grl <- split(object, values(object)$.bioviz.facetid)
      if(args.facets$facets == "~seqnames")
        grl <- split(object, seqnames(object))
    }else{
      stop("facets is an empty GenomicRanges object")
    }
  grl
})


setMethod("splitByFacets", c("GRanges", "NULL"), function(object, facets){
  res <- split(object, seqnames(object))
  res
})




.buildFacetsFromArgs <- function(args, facets){
  args.facets <- args
  if(length(facets)){
    .checkFacetsRestrict(facets)
    if(is(facets, "GRanges")){
      args.facets$facets <- substitute(~.bioviz.facetid)
      ## ok, default is "free"
      if(!("scales" %in% names(args.facets)))
        args.facets$scales <- "free"
      facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                            TRUE, FALSE)
      if(facet.logic)
        facet <- do.call(facet_wrap, args.facets)
      else
        facet <- do.call(facet_grid, args.facets)
    }else{
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      allvars <- all.vars(as.formula(args.facets$facets))
      facet <- do.call(facet_grid, c(args$facets, args.facets))      
    }}else{
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      ## if(length(seqname) > 1){
        facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                              TRUE, FALSE)
        if(facet.logic){
          args.facets <- c(args.facets, list(facets = substitute(~seqnames)))
          facet <- do.call(facet_wrap, args.facets)
        }else{
          args.facets <- c(args.facets, list(facets = substitute(.~seqnames)))
          facet <- do.call(facet_grid, args.facets)
        }
      ## }
    }
  facet
}


.checkFacetsRestrict <- function(facets){
  allvars <- all.vars(as.formula(facets))
  if(allvars[2] != "seqnames")
    stop("Column of facets formula can only be seqnames, such as . ~ seqnames, in default restrict mode, you can only change row varaibles")
  if(!allvars[1] %in% colnames(values(object)))
    stop(allvars[1]," doesn't exists in data columns")
}


