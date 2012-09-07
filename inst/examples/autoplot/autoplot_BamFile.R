## @knitr load
library(ggbio)
library(GenomicRanges)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)

## @knitr coverage_est
p1 <- autoplot(bamfile, geom = "line", method = "estimate", which = c("chr1", "chr2", "chr3"))
p1  + coord_genome()
p2 <- autoplot(bamfile, geom = "line", method = "estimate", which = GRanges("chr1",IRanges(1, 1e6)))
p2

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


p1 <- autoplot(bamfile, geom = "line", method = "estimate", which = c("chr1", "chr2", "chr3"),
               coord = "genome")

p1 <- autoplot(bf, geom = "line", method = "estimate", which = c("chr1", "chr2", "chr3"),
               coord = "genome")

p1
