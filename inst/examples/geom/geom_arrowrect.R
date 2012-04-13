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


## @knitr default
## ======================================================================
##  default
## ======================================================================
ggplot() + geom_arrowrect(gr)

## @knitr facet_aes
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_arrowrect(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))

## @knitr stat:identity
## ======================================================================
##  stat:identity
## ======================================================================
ggplot() + geom_arrowrect(gr, stat = "identity", aes(y = value))

## @knitr stat:stepping
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_arrowrect(gr, stat = "stepping", aes(y = value, group = pair))

## @knitr group.selfish
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_arrowrect(gr, stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)


## @knitr NULLL
ggplot() + geom_rect(gr) + geom_arrowrect(GRanges(),xlab = "xlab", ylab = "ylab", main = "main")
