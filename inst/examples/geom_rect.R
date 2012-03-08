set.seed(1)
N <- 100
library(ggbio)
library(GenomicRanges)
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

## ======================================================================
##  default
## ======================================================================
ggplot() + geom_rect(gr)
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_rect(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))
## ======================================================================
##  stat:identity
## ======================================================================
ggplot() + geom_rect(gr, stat = "identity", aes(y = value))
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, group = pair))
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)

