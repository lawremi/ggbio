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
ggplot() + geom_alignment(gr)

## @knitr facet_aes
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_alignment(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))

## @knitr stat:stepping
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_alignment(gr, stat = "stepping", aes(group = pair))

## @knitr group.selfish
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_alignment(gr, stat = "stepping", aes(group = pair), group.selfish = FALSE)

## @knitr main_gap
## =======================================
##  main/gap geom
## =======================================
ggplot() + geom_alignment(gr, main.geom = "arrowrect", gap.geom = "chevron")

## @knitr NULLL
ggplot() + geom_rect(gr) + geom_alignment(GRanges(),xlab = "xlab", ylab = "ylab", main = "main")
