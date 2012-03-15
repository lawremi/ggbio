setMethod("plotSpliceSum", c("character", "GRangesList"),
          function(data, model, ..., weighted = TRUE){
  freq <- biovizBase:::spliceSummary(data, model, weighted = weighted)
  autoplot(model, freq = freq, ...)
})
setMethod("plotSpliceSum", c("character", "TranscriptDb"),
          function(data, model, which, ..., weighted = TRUE){
            exons <- exonsBy(model, by = "tx")
            exons <- subsetByOverlaps(exons, which)
            freq <- biovizBase:::spliceSummary(data, exons, weighted = weighted)
            autoplot(exons, freq = freq, ...)
          })

