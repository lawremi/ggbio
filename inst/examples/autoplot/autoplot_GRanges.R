## @knitr load
library(ggbio)

## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
set.seed(1)
N <- 1000
library(GenomicRanges)
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

## @knitr default
autoplot(gr)



## @knitr geom/aes/facet
autoplot(gr, geom = "point", aes(y = score))
autoplot(gr, fill = "red")
autoplot(gr, aes(fill = value))
autoplot(gr, facets = sample ~ seqnames)
autoplot(gr[idx], geom = "chevron", offset = 1)
autoplot(gr[idx], geom = "arrowrect", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

## @knitr group
## make a minimal sample .
gra <- GRanges("chr1", IRanges(c(1,7,20), end = c(4,9,30)), group = c("a", "a", "b"))
## if you desn't specify group, then group based on stepping levels, and gaps are computed without
## considering extra group method
autoplot(gra, aes(fill = group), geom = "alignment")
## when use group method, gaps only computed for grouped intervals.
## default is group.selfish = TRUE, each group keep one row.
## in this way, group labels could be shown as y axis.
autoplot(gra, aes(fill = group, group = group), geom = "alignment")
## group.selfish = FALSE, save space
autoplot(gra, aes(fill = group, group = group), geom = "alignment", group.selfish = FALSE)
## without group method
autoplot(gra, aes(fill = group), geom = "alignment", group.selfish = FALSE)

## @knitr group-more
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)

## @knitr facet:strand
autoplot(gr, stat = "coverage", geom = "area", facets = strand ~ seqnames, aes(fill = strand))

## @knitr NULL
library(biovizBase)
autoplot(gr[idx], geom = "alignment", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair))
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

## @knitr stat
autoplot(gr, stat = "coverage", facets = sample ~ seqnames)
autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)
autoplot(gr, stat = "aggregate", y = "value")
## for boxplot, y need to be in aes()
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot", facets = sample ~ seqnames)

## @knitr NULL
autoplot(gr, stat = "coverage", facets = sample ~ seqnames, geom = "point")
autoplot(gr, stat = "table")
autoplot(gr, stat = "table", rect.height = 0.2)
autoplot(gr, stat = "table", rect.height = 0.2, fill = "red")

autoplot(gr, stat = "boxplot", aes(y = score))


autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)


autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot")
## FIXME: aggregate error
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 38)
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 2)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 10)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 30,
         geom = "histogram")


## @knitr coord:genome
## simple example
autoplot(gr, coord = "genome")

## @knitr coord:truncate_gaps
## simple example
autoplot(gr, coord = "genome")

## @knitr layout:circle
autoplot(gr, layout = "circle", geom = "ideo", rect.inter.n = 50, aes(fill = .ori.seqnames))


