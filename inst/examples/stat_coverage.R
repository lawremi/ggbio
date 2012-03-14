library(biovizBase)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)
system.time(res <- estimateCoverage(bf))
head(res)
library(ggbio)
res1 <- res[seqnames(res) == "chr1"]
autoplot(res1, geom = "point", aes(y = score))
autoplot(res1[1:100])
res1[1:100]
