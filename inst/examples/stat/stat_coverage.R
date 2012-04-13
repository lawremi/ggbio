## @knitr load
library(ggbio)

## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
set.seed(1)
N <- 1000
library(GenomicRanges)
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


## @knitr geom
ggplot() + stat_coverage(gr)
ggplot() + stat_coverage(gr, geom = "point")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "histogram")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "area")
ggplot() + stat_coverage(gr, geom = "smooth")

## @knitr facet:sample
ggplot() + stat_coverage(gr, geom = "line", facets = sample ~ seqnames)

## @knitr facet:strand
ggplot() + stat_coverage(gr, geom = "line", facets = strand ~ seqnames)


## @knitr grl
grl <- split(gr, values(gr)$sample)
grl <- endoapply(grl, function(gr){
  nms <- setdiff(colnames(values(gr)), "sample")
  values(gr) <- values(gr)[nms]
  gr
})


## @knitr grl:default
ggplot() + stat_coverage(grl)

## @knitr grl:facet
ggplot() + stat_coverage(grl, geom = "area", facets = ..grl_name.. ~ seqnames,
                         aes(fill = ..grl_name..))



## @knitr BamFile
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)

## @knitr Bamfile:est
ggplot() + stat_coverage(bf, geom = "point", method = "estimate")

## @knitr Bamfile:raw
gr.wh <- GRanges("chr1", IRanges(1, 5e4))
ggplot() + stat_coverage(bf, geom = "line", which = gr.wh, method = "raw")



## @knitr NULLL
ggplot() + stat_coverage(gr) + stat_coverage(GRanges())
