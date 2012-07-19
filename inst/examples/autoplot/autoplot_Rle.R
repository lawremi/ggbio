## @knitr load
library(IRanges)
library(ggbio)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))

## @knitr create
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)

## @knitr NULL
autoplot(xRle)
autoplot(xRle, nbin = 80)
## slow for
autoplot(xRle, geom = "heatmap")

autoplot(xRle, stat = "identity")
autoplot(xRle, stat = "identity", geom = "point", color = "red")
autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5)
autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")

autoplot(xRleList)
autoplot(xRleList, nbin = 80)
autoplot(xRleList, geom = "heatmap")
autoplot(xRleList, stat = "identity")
autoplot(xRleList, stat = "identity", geom = "point", color = "red")
autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5)
autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")


