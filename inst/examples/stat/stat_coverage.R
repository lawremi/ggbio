library(ggbio)
library(GenomicRanges)
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)
bf
## library(devtools)
## load_all("~/Codes/gitrepos/ggbio")
##  GRanges
set.seed(1)
N <- 1000

## ======================================================================
##  simmulated GRanges
## ======================================================================
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


grl <- split(gr, values(gr)$sample)
grl <- endoapply(grl, function(gr){
  nms <- setdiff(colnames(values(gr)), "sample")
  values(gr) <- values(gr)[nms]
  gr
})

ggplot() + stat_coverage(gr)
ggplot() + stat_coverage(gr, geom = "line")
ggplot() + stat_coverage(gr, geom = "point")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "point")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "histogram")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "area")
ggplot() + stat_coverage(gr, geom = "smooth")

ggplot() + stat_coverage(gr, geom = "point", facets = sample ~ seqnames)

## GRangesList
ggplot() + stat_coverage(grl, geom = "area", facets = ..grl_name.. ~ seqnames,
                         aes(fill = ..grl_name..))

ggplot() + stat_coverage(grl, geom = "area")

## FIXME:
myfun <- function(data, ...){
  ggplot() + stat_coverage(data, ...) ## doesn't work
}
myfun(gr, aes(y = ..coverage..), geom = "histogram", color = "blue")
myfun(grl)

## default is line
p <- ggplot() + stat_coverage(bf)
p
p+ xlim(c(0, 2.5e6))
ggplot() + stat_coverage(bf, geom = "point")

myfun(bf, geom = "point")

f <- function(y) function() y
lf <- vector("list", 5)

for (i in seq_along(lf)) lf[[i]] <- f(i)
lf[[1]]()  # returns 5
lf[[1]]

g <- function(y) { force(y); function() y }
lg <- vector("list", 5)
for (i in seq_along(lg)) lg[[i]] <- g(i)
lg[[1]]()  # returns 1

## This is identical to
g <- function(y) { y; function() y }
