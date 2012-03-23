library(ggbio)
library(GenomicRanges)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
autoplot(bamfile, stat = "coverage")

