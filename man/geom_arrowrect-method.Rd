\name{geom_arrowrect}
\alias{geom_arrowrect}
\alias{geom_arrowrect,GRanges-method}
\alias{geom_arrowrect,missing-method}
\alias{geom_arrowrect,uneval-method}
\title{Arrowrect geoms for GRanges object}
\description{
  Show interval data as rectangle with a arrow head.
}
\usage{
\S4method{geom_arrowrect}{GRanges}(data, ..., xlab, ylab, main,
              facets = NULL, stat = c("stepping", "identity"),
              rect.height = NULL, arrow.head = 0.06,
              arrow.head.rate = arrow.head,  arrow.head.fix = NULL,
              group.selfish = TRUE)

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
  \item{arrow.head}{
    Arrow head to body ratio.
  }
  \item{arrow.head.rate}{
    Arrow head to body ratio. same with arrow.head.
  }
  \item{arrow.head.fix}{
    fixed length of arrow head.
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
ggplot(gr) + geom_arrowrect()
## or
ggplot() + geom_arrowrect(gr)

## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot(gr) + geom_arrowrect(facets = sample ~ seqnames, aes(color = strand, fill = strand))


## ======================================================================
##  stat:identity
## ======================================================================
ggplot(gr) + geom_arrowrect(stat = "identity", aes(y = value))


## ======================================================================
##  stat:stepping
## ======================================================================
ggplot(gr) + geom_arrowrect(stat = "stepping", aes(y = value, group = pair))


## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot(gr) + geom_arrowrect(gr, stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)
}
\author{Tengfei Yin}

