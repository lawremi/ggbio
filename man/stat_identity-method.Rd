\name{stat_identity}
\alias{stat_identity}
\alias{stat_identity,data.frame-method}
\alias{stat_identity,GRanges-method}
\title{Calculate coverage}
\description{
  Calculate coverage.
}
\usage{
% data.frame
\S4method{stat_identity}{data.frame}(data, ...)
% GRanges
\S4method{stat_identity}{GRanges}(data, ..., geom = NULL)
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
}
\author{Tengfei Yin}

