library(ggbio)
library(GenomicRanges)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)

## library(devtools)
## load_all("~/Codes/gitrepos/ggbio")
##  GRanges
set.seed(1)
N <- 1000

ggplot() + stat_coverage(bf, geom = "point")
autoplot(bf, stat = "coverage")

