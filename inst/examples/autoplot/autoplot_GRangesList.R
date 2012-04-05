library(ggbio)
library(GenomicRanges)
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

idx <- sample(1:length(gr), size = 200)

grl <- split(gr, values(gr)$sample)
grl <- endoapply(grl, function(gr){
  nms <- setdiff(colnames(values(gr)), "sample")
  values(gr) <- values(gr)[nms]
  gr
})

grl <- split(gr, values(gr)$pair)
## grl <- endoapply(grl, function(gr){
##   nms <- setdiff(colnames(values(gr)), "sample")
##   values(gr) <- values(gr)[nms]
##   gr
## })


autoplot(grl)
autoplot(grl, group.selfish = TRUE)
myfun <- function(data, ...){
  autoplot(data, ...)
}


myfun(grl, group.selfish = TRUE)
