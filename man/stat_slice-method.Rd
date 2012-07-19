\name{stat_slice}
\alias{stat_slice}
\alias{stat_slice,Rle-method}
\alias{stat_slice,RleList-method}
\title{Slice Rle/RleList to view them as bar or heatmap.}
\description{
  Slice Rle/RleList to different view by set lower or other parameters,
  then view summary for all those viewed region.
}
\usage{
\S4method{stat_slice}{Rle}(data, ...,xlim,
          xlab, ylab, main,
          na.rm = FALSE,
          facets = NULL, 
          geom = NULL,
          lower=-Inf, upper=Inf,
          includeLower=TRUE, includeUpper=TRUE,
          rangesOnly = FALSE,                                       
          type = c("viewSums","viewMins",
          "viewMaxs", "viewMeans"))

\S4method{stat_slice}{RleList}(data, ...,xlim,
          xlab, ylab, main,
          indName = "sample",
          na.rm = FALSE,
          facets = NULL, 
          geom = NULL,
          lower=-Inf, upper=Inf,
          includeLower=TRUE, includeUpper=TRUE,
          rangesOnly = FALSE,                                       
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
\item{na.rm}{
  logical value, default \code{FALSE}, passed to function like
  \code{viewMaxs} for statistical summary computation.
}
\item{lower}{
  passed to \code{\link[IRanges]{slice}}.
}
\item{upper}{
  passed to \code{\link[IRanges]{slice}}.
}
\item{includeLower}{
  passed to \code{\link[IRanges]{slice}}.  
}
\item{includeUpper}{
    passed to \code{\link[IRanges]{slice}}.
}
\item{rangesOnly}{
    passed to \code{\link[IRanges]{slice}}.
}
}
\value{
  a ggplot object.
}
\seealso{
  \code{\link[IRanges]{slice}}
}
\examples{
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)

ggplot() + stat_slice(xRle, lower = 5)
ggplot() + stat_slice(xRle, lower = 5, geom = "bar")
ggplot() + stat_slice(xRle, lower = 5, geom = "heatmap")

p1 <- ggplot() + stat_slice(xRle, type = "viewMeans", lower = 5,
                            geom = "bar")
p2 <- ggplot() + stat_slice(xRle, type = "viewSums", lower = 5,
                            geom = "bar")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)

ggplot() + stat_slice(xRleList, lower = 5)
ggplot() + stat_slice(xRleList, lower = 5, geom = "bar")
ggplot() + stat_slice(xRleList, lower = 5, geom = "heatmap")

p1 <- ggplot() + stat_slice(xRleList, type = "viewMeans", lower = 5,
                            geom = "bar")
p2 <- ggplot() + stat_slice(xRleList, type = "viewSums", lower = 5,
                            geom = "bar")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)
}
\author{Tengfei Yin}
