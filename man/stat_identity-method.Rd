\name{stat_identity}
\alias{stat_identity}
\alias{stat_identity,data.frame-method}
\alias{stat_identity,GRanges-method}
\alias{stat_identity,Rle-method}
\alias{stat_identity,RleList-method}
\title{Transform the data to a data.frame and for multiple geoms.}
\description{
  Transform the data to a suitable data.frame and then one could use
  multiple geom or even stat to re-plot the data.
}
\usage{
\S4method{stat_identity}{data.frame}(data, ...)

\S4method{stat_identity}{GRanges}(data, ..., geom = NULL)

\S4method{stat_identity}{Rle}(data, ..., xlab, ylab, main, geom = NULL)

\S4method{stat_identity}{RleList}(data, ..., xlab, ylab, main,
          geom = NULL, indName = "sample")
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
}
\value{
  A 'Layer'.
}
\examples{
##  load
set.seed(1)
N <- 50
require(ggbio)
require(GenomicRanges)
##  simul
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

##  geom_point_start
ggplot() + stat_identity(gr, aes(x = start, y = value), geom = "point")

##  geom_point_midpoint
ggplot() + stat_identity(gr, aes(x = midpoint, y = value), geom = "point")

##  geom_rect_all
ggplot() + stat_identity(gr, aes(xmin = start, xmax = end,
                                 ymin = value - 0.5, ymax = value + 0.5),
                           geom = "rect")

##  geom_rect_y
ggplot() + stat_identity(gr, aes(y = value), geom = "rect")

##  geom_line
ggplot() + stat_identity(gr, aes(x = start, y = value),  geom = "line")

##  geom_segment
ggplot() + stat_identity(gr, aes(y = value), geom = "segment")

## Rle/RleList
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)

ggplot() + stat_identity(xRle, geom = "point")
ggplot() + stat_identity(xRleList, geom = "point")
}
\author{Tengfei Yin}

