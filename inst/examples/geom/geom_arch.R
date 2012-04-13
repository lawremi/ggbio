## @knitr load
set.seed(1)
N <- 100
library(ggbio)
library(GenomicRanges)

## @knitr simul
## =======================================
##  simmulated GRanges
## =======================================
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

## @knitr default
## =======================================
##  default
## =======================================
ggplot() + geom_arch(gr)

## @knitr facet_aes
## =======================================
##  facetting and aesthetics
## =======================================
ggplot() + geom_arch(gr, aes(color = value, height = value, size = value),
                     alpha = 0.2, facets = sample ~ seqnames)

## @knitr NULL
grn <- GRanges()
ggplot() + geom_rect(gr, rect.height = 0.5) + geom_arch(grn, xlab = "main", ylab = "main", main = "mian")
ggplot() + geom_arch(gr, rect.height = 0)

