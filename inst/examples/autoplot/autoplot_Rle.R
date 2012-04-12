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
autoplot(xRle, geom = "line")
autoplot(xRle, geom = "segment")
autoplot(xRle, type = "viewMaxs", lower = 5)
autoplot(xRle, type = "viewMins", lower = 5)
autoplot(xRle, type = "viewMeans", lower = 5)
autoplot(xRle, type = "viewMeans", lower = 5, color = I("red"))
autoplot(xRle, type = "viewSums", lower = 5)
autoplot(xRle, type = "viewMaxs", lower = 5, geom = "line")
autoplot(xRle, type = "viewMaxs", lower = 5, geom = "segment")



autoplot(xRleList)
autoplot(xRleList, geom = "segment")
autoplot(xRleList, geom = "line")
autoplot(xRleList, type = "viewMaxs", lower = 5)
autoplot(xRleList, type = "viewMaxs", lower = 5, geom = "line")
autoplot(xRleList, type = "viewSums", lower = 5, geom = "segment",
      facetByRow = FALSE, color = "red", size = I(5))

autoplot(xRle, size = y)
autoplot(xRle, type = "viewSums", lower = 5)

autoplot(xRle, type = "viewSums", lower = 5, size = I(10), color = I("red"),
      alpha = y)
