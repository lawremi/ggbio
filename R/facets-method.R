strip_facets_dots <- function(facets){
  allvars <- all.vars(as.formula(facets))
  idx <- ggplot2:::is_calculated_aes(allvars)
  if(sum(idx))
    facets[[which(idx)+1]] <- as.name(unlist(ggplot2:::strip_dots(allvars[idx])))
  facets
}

setOldClass("formula")
setGeneric("splitByFacets", function(object, facets, ...) standardGeneric("splitByFacets"))

setMethod("splitByFacets", c("GRanges", "formula"), function(object, facets){
  .checkFacetsRestrict(facets, object)
  facets <- strip_facets_dots(facets)
  allvars <- all.vars(as.formula(facets))
  if(length(allvars) > 1){
  if(allvars[1] == "."){
    res <- split(object, seqnames(object))
  }else{
    if(allvars[1] != "strand")
      fts <- interaction(as.character(values(object)[,allvars[1]]),as.character(seqnames(object)))
    else
      fts <- interaction(as.character(strand(object)),as.character(seqnames(object)))
    res <- split(object, fts)    
  }}
  if(length(allvars) == 1)
    res <- split(object, seqnames(object))
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


isFacetByOnlySeq <- function(facets){
  allvars <- all.vars(as.formula(facets))
  allvars <- allvars[!allvars %in% c(".", "seqnames")]
  if(!length(allvars))
    TRUE
  else
    FALSE
}



## need to consider a length 1 facets formula
.buildFacetsFromArgs <- function(object, args){
  isOneSeq <- length(unique(as.character(seqnames(object)))) == 1
  args.facets <- args
  args.facets$facets <- strip_facets_dots(args$facets)
  facets <- args.facets$facets
  if(length(facets)){
    ## allvars <- all.vars(as.formula(facets))
    ## if(length(allvars) == 1){
    .checkFacetsRestrict(facets, object)
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
      
      if(isOneSeq & isFacetByOnlySeq(args.facets$facets)){
        facet <- NULL
      }else{
      facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                            TRUE, FALSE)
      if(facet.logic){
        facet <- do.call(facet_wrap, args.facets)
      }else{
        facet <- do.call(facet_grid, args.facets)
      }
      facet <- do.call(facet_grid, args.facets)
    }
    }}else{
      if(!("scales" %in% names(args.facets)))
        args.facets <- c(args.facets, list(scales = "fixed"))
      args.facets$facets <- substitute(~seqnames)
      allvars <- all.vars(as.formula(args.facets$facets))
      
      if(isOneSeq & isFacetByOnlySeq(args.facets$facets)){
        facet <- NULL
      }else{
        facet.logic <- ifelse(any(c("nrow", "ncol") %in% names(args.facets)),
                              TRUE, FALSE)
        if(facet.logic){
          facet <- do.call(facet_wrap, args.facets)
        }else{
          facet <- do.call(facet_grid, args.facets)
        }
      }
    }
  facet
}


## that's for linear or default!
.checkFacetsRestrict <- function(facets, object){
  facets <- strip_facets_dots(facets)
  allvars <- all.vars(as.formula(facets))
  if(length(allvars) == 1){
    if(allvars[1] != "seqnames")
      stop("Column of facets formula can only be seqnames, such as . ~ seqnames, in default restrict mode, you can only change row varaibles")
  }
  if(length(allvars) > 1){
    if(allvars[2] != "seqnames" & allvars[2] != "."){
      stop("Column of facets formula can only be seqnames, such as . ~ seqnames, in default restrict mode, you can only change row varaibles")
    }
    if(allvars[1] != "."){
      if(!allvars[1] %in% c(colnames(values(object)), "strand"))
        stop(allvars[1]," doesn't exists in data columns")
      if(allvars[2] != "seqnames")
        stop("Column of facets formula can only be seqnames, such as . ~ seqnames, in default restrict mode, you can only change row varaibles")
    }
  }
}

## that's for layout_karyogram
.checkFacetsRestrictForKaryogram <- function(facets, object){
  allvars <- all.vars(as.formula(facets))
  if(length(allvars) == 1){
    if(allvars[1] != "seqnames")
      stop("seqnames must be present in layout karyogram")
  }
  if(length(allvars) > 1){
    if(!"seqnames" %in% allvars){
      stop("seqnames must be present in layout karyogram")
    }else{
      allvars.extra <- setdiff(allvars, "seqnames")
      if(!allvars.extra  %in% colnames(values(object)))
        stop("facet variable must be in the data")
    }
  }
}


