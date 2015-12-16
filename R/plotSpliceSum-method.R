setMethod("plotSpliceSum", c("character", "GRangesList"),
          function(data, model, ..., weighted = TRUE){
  freq <- biovizBase:::spliceSummary(data, model, weighted = weighted)
  autoplot(model, freq = freq, ...)
})
setMethod("plotSpliceSum", c("character", "TxDb"),
          function(data, model, which, ..., weighted = TRUE){
            exons <- exonsBy(model, by = "tx")
            exons <- subsetByOverlaps(exons, which)
            freq <- biovizBase:::spliceSummary(data, exons, weighted = weighted)
            autoplot(exons, freq = freq, ...)
          })

####============================================================
##  plotSpliceSum method from ggbio, R/plotSpliceSum-method.R
##
####------------------------------------------------------------
setMethod("plotSpliceSum", c("character", "EnsDb"),
          function(data, model, which, ..., weighted = TRUE){
              if(is(which, "GRanges")){
                  if(length(which) != 1)
                      stop("'which' has to be a single GRanges object.")
                  if(!is.na(genome(which))){
                      if(unname(genome(which)) != unique(unname(genome(model))))
                          stop(paste0("Genome versions do not fit! Argument 'which' has ",
                            unname(genome(which)), " argument 'model' ",
                            unname(unique(genome(which))), "!"))
                  }
                  ## Check if we've got the seqnames.
                  if(!(seqlevels(which) %in% seqlevels(model)))
                      stop(paste0(seqlevels(which), " does not match any seqlevel ",
                                  "in argument 'model'!"))
                  which <- GRangesFilter(which, condition="overlapping")
              }
              exons <- exonsBy(model, by="tx", filter=which)
              ## Check if features are all on one chromosome.
              if(length(seqlevels(unlist(exons))) > 1)
                  stop(paste0("Got features from ", length(seqlevels(unlist(exons))),
                              " different chromosomes. Please adjust 'which' such that",
                              " only features from one chromosome are fetched."))
              freq <- biovizBase:::spliceSummary(data, exons, weighted = weighted)
              autoplot(exons, freq = freq, ...)
          })

