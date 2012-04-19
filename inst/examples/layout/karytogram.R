## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr simul
set.seed(1)
N <- 1000

## ======================================================================
##  simmulated GRanges
# ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

library(biovizBase)
data(hg19IdeogramCyto, package = "biovizBase")
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
new.ideo <- keepSeqlevels(new.ideo, c(as.character(1:22) , "X", "Y"))
new.ideo
ggplot() + layout_karyogram(new.ideo, cytoband = TRUE)
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE)

data(darned_hg19_subset500, package = "biovizBase")
idx <- is.na(values(darned_hg19_subset500)$exReg)
values(darned_hg19_subset500)$exReg[idx] <- "unknown"
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE)
ggplot() + layout_karyogram(darned_hg19_subset500, geom = "rect",
                            aes(color = exReg, fill = exReg))

## need to be consistency!
old.chrs <- seqnames(seqinfo(darned_hg19_subset500))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.chrs <- new.chrs[order(as.numeric(new.chrs))]
darned_hg19_subset500 <- renameSeqlevels(darned_hg19_subset500, new.chrs)
dn <- darned_hg19_subset500
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "rect",
                   aes(color = exReg, fill = exReg))

values(dn)$score <- rnorm(length(dn))
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "line",
                   aes(x = midpoint, y = score))
## To make it more perseivable
## method one, rescale your data, since default is 0, 10
values(dn)$score2 <- rescale(values(dn)$score, c(0, 10))
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "area",
                   aes(x = midpoint, y = score2), fill = "steelblue")


## make a different layout for karyogram? instead of stacked one?


## plotStackedOverview is a simple wrapper around this functions to create a stacked layout
plotStackedOverview(new.ideo, cytoband = TRUE)
plotStackedOverview(dn)
plotStackedOverview(dn, aes(color = exReg, fill = exReg))
## this will did the trick for you to rescale the space
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line")
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line", rescale.range = c(4, 6))
## no rescale
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line", rescale = FALSE,
                    xlab = "xlab", ylab = "ylab", main  = "main") + ylab("ylab")

## no object?
plotStackedOverview()
plotStackedOverview(cytoband = TRUE)


## autoplot API
autoplot(dn, layout = "karyogram")
autoplot(dn, layout = "karyogram", aes(color = exReg, fill = exReg))

autoplot(dn,  aes(x = midpoint, y = score), geom = "line", rescale.range = c(0, 10),
         layout = "karyogram")


ss <- sapply(values(dn)$seqReg, function(s){
  if(s == "O")
    return("Other")
  if(s == "I")
    return("Intron")
  if(s == "E")
    return("Exon")
})
values(dn)$seqReg <- ss
data("hg19Ideogram", package = "biovizBase")

.seql <- seqlengths(hg19Ideogram)
.chrs <- paste("chr", names(seqlengths(dn)), sep = "")
.seql.new <- .seql[.chrs]
names(.seql.new) <- gsub("chr", "", names(.seql.new))
idx <- end(dn) < .seql.new[as.character(seqnames(dn))]
dn <- dn[idx]
seqlengths(dn) <- .seql.new
autoplot(dn, layout = "karyogram", aes(color = seqReg, fill = seqReg)) + xlab("Genomic Coordinates")
## FIXME:
autoplot(new.ideo, layout = "karyogram", cytoband = TRUE) + xlab("")
ggsave("~/Desktop/stacked_darn.pdf")

## plotSingleChrom API
plotSingleChrom(hg19IdeogramCyto, subchr = "chr1")
plotSingleChrom(hg19IdeogramCyto, subchr = "chr1", xlabel = TRUE)
## zoom
plotSingleChrom(hg19IdeogramCyto, subchr = "chr1",  zoom.region = c(1e8, 1.5e8))


