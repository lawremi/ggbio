## simply wrapper
theme_genome <- function(){
  list(facet_grid(.~seqnames),
       theme_pack_panels(),
       scale_x_continuous(breaks = NULL))       
}


## setGeneric("coord_genome", function(data,...)
##            standardGeneric("coord_genome"))

## setMethod("coord_genome", "GRanges", function(data, space.skip = 0.1){
##   object <- transformToGenome(object, space.skip = space.skip)
##   object <- biovizBase:::rescaleGr(object)
## })

