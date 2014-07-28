## FIXME: more flexible name.expr arguments
setGeneric("stat_gene", function(data, ...) standardGeneric("stat_gene"))
setMethod("stat_gene", "TxDb", function(data, ...){
  .Deprecated("geom_alignment")
  geom_alignment(data, ...)
})
