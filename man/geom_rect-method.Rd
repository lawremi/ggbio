\name{geom_rect}
\alias{geom_rect}
\alias{geom_rect,ANY-method}
\alias{geom_rect,GRanges-method}
\alias{geom_rect,missing-method}
\alias{geom_rect,uneval-method}
\title{Rect geoms for GRanges object}
\description{
  Show interval data as rectangle.
}
\usage{
\S4method{geom_rect}{ANY}(data, ...)
\S4method{geom_rect}{GRanges}(data,..., xlab, ylab, main,
          facets = NULL, stat = c("stepping", "identity"),
          rect.height = NULL,
          group.selfish = TRUE)
}
\arguments{
  \item{data}{
    Typically a \code{GRanges} or \code{data.frame} object. When it's
    \code{data.frame}, it's simply calling ggplot2::geom_rect.
  }
  \item{...}{
    Extra parameters such as aes() or \code{color, size} passed.
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
  \item{facets}{
    Faceting formula to use. 
  }
  \item{stat}{
    Character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.
  }
  \item{rect.height}{
    Half height of the arrow body. 
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
##  data.frame call ggplot2::geom_rect
## ======================================================================
ggplot() + geom_rect(data = mtcars, aes(xmin = mpg, ymin = wt, xmax = mpg + 10, ymax = wt + 0.2,
                       fill = cyl))



## ======================================================================
##  default
## ======================================================================
ggplot(gr) + geom_rect()
# or
ggplot() + geom_rect(gr)


## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot(gr) + geom_rect(facets = sample ~ seqnames, aes(color = strand, fill = strand))


## ======================================================================
##  stat:identity
## ======================================================================
ggplot(gr) + geom_rect(stat = "identity", aes(y = value))


## ======================================================================
##  stat:stepping
## ======================================================================
ggplot(gr) + geom_rect(stat = "stepping", aes(y = value, group = pair))


## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot(gr) + geom_rect(stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)
}
\author{Tengfei Yin}

