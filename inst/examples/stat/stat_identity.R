## @knitr load
set.seed(1)
N <- 50
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

## @knitr geom_point_start
ggplot() + stat_identity(gr, aes(x = start, y = value), geom = "point")

## @knitr geom_point_midpoint
ggplot() + stat_identity(gr, aes(x = midpoint, y = value), geom = "point")

## @knitr geom_rect_all
ggplot() + stat_identity(gr, aes(xmin = start, xmax = end,
                                 ymin = value - 0.5, ymax = value + 0.5),
                           geom = "rect")

## @knitr geom_rect_y
ggplot() + stat_identity(gr, aes(y = value), geom = "rect")

## @knitr geom_line
ggplot() + stat_identity(gr, aes(x = start, y = value),  geom = "line")

## @knitr geom_segment
ggplot() + stat_identity(gr, aes(y = value), geom = "segment")


## @knitr NULLL
ggplot() + stat_identity(gr, aes(y = value), geom = "segment") +
  stat_identity(GRanges(), aes(y = value), geom = "segment", xlab = "xlab", ylab = "lab",
                main = "main")

##
library(ggbio)
library(IRanges)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)
ggplot() + stat_identity(data = xRle)
ggplot() + stat_identity(data = xRle, aes(x = x, y = y), geom = "line")
ggplot() + stat_identity(data = xRle, geom = "line")
ggplot() + stat_identity(data = xRle, geom = "bar")
ggplot() + stat_identity(data = xRle, geom = "point")
ggplot() + stat_slice(data = xRle)
ggplot() + stat_slice(data = xRle, lower = 10)
ggplot() + stat_slice(data = xRle, lower = 2, geom = "rect")
ggplot() + stat_slice(data = xRle, lower = 2, geom = "heatmap")
ggplot() + stat_slice(data = xRle, lower = 2, geom = "bar")
ggplot() + stat_bin(data = xRle)
ggplot() + stat_bin(data = xRle, nbin = 100)
ggplot() + stat_bin(data = xRle, nbin = 100, geom = "heatmap")


ggplot() + stat_identity(data = xRleList)
ggplot() + stat_identity(data = xRleList, geom = "point")
ggplot() + stat_identity(data = xRleList, geom = "line")
ggplot() + stat_identity(data = xRleList, geom = "bar")
ggplot() + stat_identity(data = xRleList, geom = "area")

ggplot() + stat_slice(data = xRleList)
ggplot() + stat_slice(data = xRleList, lower = 8)
ggplot() + stat_slice(data = xRleList, lower = 8, geom = "rect")
ggplot() + stat_slice(data = xRleList, lower = 8, geom = "bar")
ggplot() + stat_slice(data = xRleList, lower = 8, geom = "heatmap")

ggplot() + stat_bin(data = xRleList)
ggplot() + stat_bin(data = xRleList, geom = "heatmap")
ggplot() + stat_bin(data = xRleList, geom = "heatmap", nbin = 400)

## autoplot
autoplot(xRle, nbin = 100)
autoplot(xRle, stat = "slice", lower = 0.1)
autoplot(xRle, stat = "slice", lower = 2, geom = "point")
autoplot(xRle, stat = "slice", lower = 2, geom = "line")
autoplot(xRle, stat = "slice", lower = 2, geom = "area")
autoplot(xRle, stat = "slice", lower = 2, geom = "heatmap")
autoplot(xRle, stat = "slice", lower = 2, geom = "rect")
autoplot(xRle, stat = "slice", lower = 2, geom = "bar")
autoplot(xRle, stat = "bin", geom = "heatmap",nbin = 100)

autoplot(xRle, nbin = 100)
autoplot(xRleList, stat = "slice", lower = 0.1)
autoplot(xRleList, stat = "slice", lower = 2, geom = "point")
autoplot(xRleList, stat = "slice", lower = 2, geom = "line")
autoplot(xRleList, stat = "slice", lower = 2, geom = "area")
autoplot(xRleList, stat = "slice", lower = 2, geom = "heatmap")
autoplot(xRleList, stat = "slice", lower = 2, geom = "rect")
autoplot(xRleList, stat = "slice", lower = 2, geom = "bar")
autoplot(xRleList, stat = "bin", geom = "heatmap",nbin = 100)

ggplot2::geom_rect
