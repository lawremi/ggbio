## @knitr load
library(ggbio)
library(GenomicRanges)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)

## @knitr coverage_est
p1 <- autoplot(bamfile, geom = "line", method = "estimate")

## @knitr coverage_raw
data(genesymbol, package = "biovizBase")
p2 <- autoplot(bamfile,  method = "raw", which = genesymbol["ALDOA"])

## @knitr mismatch
library(BSgenome.Hsapiens.UCSC.hg19)
autoplot(bf, stat = "mismatch", which = genesymbol["ALDOA"], bsgenome = Hsapiens)

## @knitr NULL (fixme)
autoplot(bf, geom = "gapped.pair", which = genesymbol["ALDOA"])

## @knitr other
library(biovizBase)
autoplot(bf, geom = "segment", stat = "stepping", which = genesymbol["ALDOA"])

