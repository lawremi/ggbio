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
ggplot() + geom_chevron(gr)

## @knitr facet_aes
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_chevron(gr, facets = sample ~ seqnames, aes(color = strand))

## @knitr stat:identity
## ======================================================================
##  stat:identity
## ======================================================================
ggplot() + geom_chevron(gr, stat = "identity", aes(y = value))

## @knitr stat:stepping
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_chevron(gr, stat = "stepping", aes(group = pair))

## @knitr group.selfish
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_chevron(gr, stat = "stepping", aes(group = pair), group.selfish = FALSE,
                        xlab = "xlab", ylab = "ylab", main = "main")


## @knitr offset
## ======================================================================
##  offset
## ======================================================================
gr2 <- GRanges("chr1", IRanges(c(1, 10, 20), width = 5))
gr2.p <- gaps(gr2)
## resize to connect them
gr2.p <- resize(gr2.p, fix = "center", width = width(gr2.p)+2)
## @knitr offset:default
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p)

## @knitr offset:0
## notice the rectangle height is 0.8
## offset = 0 just like a line
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = 0)

## @knitr offset:0.4
## equal height
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = 0.4)

## @knitr chevron.height:default
## ======================================================================
##  chevron.height
## ======================================================================
values(gr2.p)$score <- c(100, 200)
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = "score")
## @knitr chevron.height
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = "score",
                                         chevron.height.rescale = c(0.4, 10))


## @knitr NULLL
ggplot() + geom_rect(gr) + geom_chevron(GRanges(),xlab = "xlab", ylab = "ylab", main = "main")
