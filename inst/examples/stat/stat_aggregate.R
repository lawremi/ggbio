## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr simul
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

## 36,37 doesn't work
ggplot() + stat_aggregate(gr, y = "value",fill = "gray40")
ggplot() + stat_aggregate(gr, window = 30,  y = "value",fill = "gray40", geom = "histogram")
ggplot() + stat_aggregate(gr, window = 100, fill = "gray40",y = "value",
                          type = "max", geom = "histogram")


ggplot() + stat_aggregate(gr, window = 100, fill = "gray40",y = "value",
                          type = "max", geom = "bar")


ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "bar")
ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "point")
ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "line")
ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "area")
ggplot() + stat_aggregate(gr, window = 100, aes(y = value), geom = "boxplot")

## @knitr NULL
ggplot() + stat_aggregate(gr, window = 100, aes(y = value),
                          geom = "boxplot", facets =  sample ~ seqnames) +
  ggplot() + stat_aggregate(gr, y = "value",fill = "gray40")

ggplot() + stat_aggregate(gr, window = 100, aes(y = value), geom = "boxplot")

