\name{stat_bin}
\alias{stat_bin}
\alias{stat_bin,data.frame-method}
\alias{stat_bin,Rle-method}
\alias{stat_bin,RleList-method}
\title{Binning method}
\description{
  Binning method especially for \code{Rle} and \code{RleList}, for
  \code{data.frame} it's just calling \code{ggplot2::stat_bin}. 
}
\usage{
\S4method{stat_bin}{data.frame}(data, ...)

\S4method{stat_bin}{Rle}(data, ..., binwidth, nbin = 30,
         xlab, ylab, main, geom = c("bar", "heatmap"),
         type = c("viewSums","viewMins",
                                  "viewMaxs", "viewMeans"))
\S4method{stat_bin}{RleList}(data, ..., binwidth, nbin = 30,
          xlab, ylab, main,
          indName = "sample",
          geom = c("bar", "heatmap"),
          type = c("viewSums","viewMins",
          "viewMaxs", "viewMeans"))
}
\arguments{
\item{data}{
    a \code{data.frame} or \code{Rle} or \code{RleList} object.
}
\item{...}{
    arguments passed to aesthetics mapping.
}
\item{binwidth}{
    width of the bins.
}
\item{nbin}{
    number of bins.
}
\item{xlab}{
x label.
}
\item{ylab}{
y label.
}
\item{main}{
title.
}
\item{indName}{
when faceted by a \code{RleList}, name used for labeling faceted
factor. Default is 'sample'.
}
\item{geom}{
geometric types.
}
\item{type}{
statistical summary method used within bins, shown as bar height or
heatmap colors.
}
}
\value{
a ggplot object.
}
\examples{
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)

ggplot() + stat_bin(xRle)
ggplot() + stat_bin(xRle, nbin = 100)
ggplot() + stat_bin(xRle, binwidth = 200)

p1 <- ggplot() + stat_bin(xRle, type = "viewMeans")
p2 <- ggplot() + stat_bin(xRle, type = "viewSums")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)

ggplot() + stat_bin(xRle, geom = "heatmap")
ggplot() + stat_bin(xRle, nbin = 100, geom = "heatmap")
ggplot() + stat_bin(xRle, binwidth = 200, geom = "heatmap")

## for RleList
ggplot() + stat_bin(xRleList)
ggplot() + stat_bin(xRleList, nbin = 100)
ggplot() + stat_bin(xRleList, binwidth = 200)

p1 <- ggplot() + stat_bin(xRleList, type = "viewMeans")
p2 <- ggplot() + stat_bin(xRleList, type = "viewSums")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)

ggplot() + stat_bin(xRleList, geom = "heatmap")
ggplot() + stat_bin(xRleList, nbin = 100, geom = "heatmap")
ggplot() + stat_bin(xRleList, binwidth = 200, geom = "heatmap")
}
\author{Tengfei Yin}

