## @knitr load
set.seed(1)
N <- 100
require(ggbio)
require(GenomicRanges)
## @knitr simul
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

gr <- c(gr[seqnames(gr) == "chr1"][sample(1:10, size = 1e4, replace = TRUE)],gr)

## @knitr default
ggplot() + stat_table(gr)
ggplot() + stat_table(gr, geom = "segment", aes(y = ..score.., color = ..score..))
ggplot() + stat_table(gr, aes(color = score))

## @knitr NULL
ggplot() + stat_table(gr) + stat_table(GRanges(), xlab = "xlab", ylab = "ylab", main = "main")
