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
## =======================================
##  facetting and aesthetics
## =======================================
grl <- GRangesList(gr1 = GRanges("1", IRanges(start = c(1, 100), width = 10)),
                   gr2 = GRanges(c("1", "3"), IRanges(start = c(20, 80), width = 10)),
                   gr3 = GRanges("3",IRanges(start = c(1, 100), width = 10)))
p <- ggplot() + geom_arch(grl)

p1 <- autoplot(unlist(grl), coord = "genome")
p1
tracks(p, p1)

grn <- GRanges()
ggplot() + geom_rect(gr, rect.height = 0.5) + geom_arch(grn, xlab = "main", ylab = "main", main = "mian")
ggplot() + geom_arch(gr, rect.height = 0)
autoplot(grl[[1]])

gr <- keepSeqlevels(gr, unique(as.character(seqnames(gr))))
gr <- GRanges("1", IRanges(2:3, width = 1))
autoplot(grl[[1]], coord = "genome", geom = "arch", aes(color = rearrangements),
         linked.to = "to.gr")


autoplot(grl, args = list(list(geom = "arch" ,  aes(color = rearrangements)),
                           list(geom = "point", aes(y = score, size = tumreads), color = "red"),
                           list(geom = "rect", fill = "steelblue", color = "steelblue"),
                           list(geom = "ideo", fill = "gray70"),
                           list(geom = "scale", size = 2),
                           list(geom = "text", aes(label  = .ori.seqnames), vjust = 0)),
         heights = c(1, 10, 6, 4, 2, 7), layout = "linear") + scale_size(range = c(1, 2.5))
