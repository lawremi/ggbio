\name{geom_arrow}
\alias{geom_arrow}
\alias{geom_arrow,GRanges-method}
\alias{geom_arrow,missing-method}
\alias{geom_arrow,uneval-method}
\title{Arrow geoms for GRanges object}
\description{
  Show interval data as arrows.
}
\usage{
\S4method{geom_arrow}{GRanges}(data, ..., xlab, ylab, main,
          angle = 30, length = unit(0.12, "cm"), type = "open",
          stat = c("stepping", "identity"), facets = NULL,
          arrow.rate = 0.03, group.selfish = TRUE)

}
\arguments{
  \item{data}{
    A \code{GRanges} object.
  }
  \item{...}{
    Extra parameters such as aes() passed.
  }
  \item{xlab}{
    Label for x
  }
  \item{ylab}{
    Label for y
  }
  \item{main}{
    Title for plot.
  }
  \item{angle}{
     The angle of the arrow head in degrees (smaller numbers produce
  narrower, pointier arrows).  Essentially describes the width of the
  arrow head.
  }
  \item{length}{
    A unit specifying the length of the arrow head (from tip to base).
  }
  \item{type}{
  One of "open" or "closed" indicating whether the arrow head should
  be a closed triangle.
  }
  \item{stat}{
    Character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{arrow.rate}{
    Arrow density of the arrow body.
  }
  \item{group.selfish}{
    Passed to \code{addStepping}, control whether to show each group as
  unique level or not. If set to \code{FALSE}, if two groups are not
  overlapped with each other, they will probably be layout in the same
  level to save space.
  }  
}
\value{
  A 'Layer'.
}
\examples{
set.seed(1)
N <- 100
require(GenomicRanges)
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


## ======================================================================
##  default
## ======================================================================
ggplot(gr) + geom_arrow()
# or
ggplot() + geom_arrow(gr)

## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot(gr) + geom_arrow(facets = sample ~ seqnames, aes(color = strand, fill = strand))

## ======================================================================
##  stat:identity
## ======================================================================
ggplot(gr) + geom_arrow(stat = "identity", aes(y = value))


## ======================================================================
##  stat:stepping
## ======================================================================
ggplot(gr) + geom_arrow(stat = "stepping", aes(y = value, group = pair))

## ======================================================================
##  group.selfish 
## ======================================================================
ggplot(gr) + geom_arrow(stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)



## ======================================================================
## other options to control arrow angle, density, ...
## ======================================================================
library(grid)
ggplot(gr) + geom_arrow(stat = "stepping", aes(y = value, group = pair),
                      arrow.rate = 0.01, length = unit(0.3, "cm"), angle = 45,
                      group.selfish = FALSE)

}
\author{Tengfei Yin}

