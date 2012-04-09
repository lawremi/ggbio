## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr simul
set.seed(1)
N <- 100
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(30:40, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))
grl <- split(gr, values(gr)$pair)

## @knitr exp
autoplot(grl)
autoplot(grl, group.selfish = TRUE)
autoplot(grl, group.selfish = TRUE, main.geom = "arrowrect", gap.geom = "segment")

## @knitr grl_name
autoplot(grl, aes(fill = ..grl_name..))
## equal to 
## autoplot(grl, aes(fill = grl_name))
