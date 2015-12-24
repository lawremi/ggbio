\name{stat_identity}
\alias{stat_identity}
\alias{stat_identity,ANY-method}
\alias{stat_identity,missing-method}
\alias{stat_identity,uneval-method}
\alias{stat_identity,GRanges-method}
\alias{stat_identity,Rle-method}
\alias{stat_identity,RleList-method}
\title{Transform the data to a data.frame and for multiple geoms.}
\description{
  Transform the data to a suitable data.frame and then one could use
  multiple geom or even stat to re-plot the data.
}
\usage{
\S4method{stat_identity}{ANY}(data, ...)

\S4method{stat_identity}{GRanges}(data, ..., geom = NULL)

\S4method{stat_identity}{Rle}(data, ..., xlab, ylab, main, geom = NULL)

\S4method{stat_identity}{RleList}(data, ..., xlab, ylab, main,
          geom = NULL, indName = "sample")
}
\arguments{
  \item{data}{
    Typically a \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
  \item{xlab}{
    x label.
  }
  \item{ylab}{
    y label.
  }
  \item{main}{
    title of graphic..
  }
  \item{indName}{
    sample name.
    }
}
\value{
  A 'Layer'.
}
\examples{
##  load
set.seed(1)
N <- 50

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
## or more formal
ggplot(gr) + stat_identity(aes(x = start, y = value), geom = "point")

##  geom_point_midpoint
ggplot(gr) + stat_identity(aes(x = midpoint, y = value), geom = "point")

##  geom_rect_all
ggplot(gr) + stat_identity(aes(xmin = start, xmax = end,
                                 ymin = value - 0.5, ymax = value + 0.5),
                           geom = "rect")

##  geom_rect_y
ggplot(gr) + stat_identity(aes(y = value), geom = "rect")

##  geom_line
ggplot(gr) + stat_identity(aes(x = start, y = value),  geom = "line")

##  geom_segment
ggplot(gr) + stat_identity(aes(y = value), geom = "segment")

## Rle/RleList
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)

ggplot(xRle) + stat_identity(geom = "point")
ggplot(xRleList) + stat_identity(geom = "point")
}
\author{Tengfei Yin}

