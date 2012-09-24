\name{stat_slice}
\alias{stat_slice}
\alias{stat_slice,missing-method}
\alias{stat_slice,uneval-method}
\alias{stat_slice,Rle-method}
\alias{stat_slice,RleList-method}
\title{Slice Rle/RleList to view them as bar or heatmap.}
\description{
  Slice Rle/RleList to different view by set lower or other parameters,
  then view summary for all those viewed region.
}
\usage{
\S4method{stat_slice}{Rle}(data, ...,
          xlab, ylab, main,
          na.rm = FALSE,
          geom = NULL,
          lower=-Inf, upper=Inf,
          includeLower=TRUE, includeUpper=TRUE,
          rangesOnly = FALSE,                                       
          type = c("viewSums","viewMins",
          "viewMaxs", "viewMeans"))

\S4method{stat_slice}{RleList}(data, ...,
          xlab, ylab, main,
          indName = "sample",
          na.rm = FALSE,
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

ggplot(xRle) + stat_slice(lower = 5)
ggplot(xRle) + stat_slice(lower = 5, geom = "bar")
ggplot(xRle) + stat_slice(lower = 5, geom = "heatmap")

p1 <- ggplot(xRle) + stat_slice(type = "viewMeans", lower = 5,
                            geom = "bar")
p2 <- ggplot(xRle) + stat_slice(type = "viewSums", lower = 5,
                            geom = "bar")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)

ggplot(xRleList) + stat_slice(lower = 5)
ggplot(xRleList) + stat_slice(lower = 5, geom = "bar")
ggplot(xRleList) + stat_slice(lower = 5, geom = "heatmap")

p1 <- ggplot(xRleList) + stat_slice(type = "viewMeans", lower = 5,
                            geom = "bar")
p2 <- ggplot(xRleList) + stat_slice(type = "viewSums", lower = 5,
                            geom = "bar")
## y scale are different.
tracks(viewMeans = p1, viewSums = p2)
}
\author{Tengfei Yin}
