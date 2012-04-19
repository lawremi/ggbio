\name{stat_stepping}
\alias{stat_stepping}
\alias{stat_stepping,GRanges-method}
\title{Calculate stepping levels}
\description{
  Calculate stepping levels.
}
\usage{
\S4method{stat_stepping}{GRanges}(data, ..., xlab, ylab, main,
              facets = NULL,
              geom = c("rect", "alignment", "segment"))
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
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
  \item{geom}{
    The geometric object used to display the data. For 'stepping', could
    be one of 'rect', 'alignment', 'segment'.
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

## @knitr default 
ggplot() + stat_stepping(gr)

## @knitr facet_aes
ggplot() + stat_stepping(gr, aes(color = strand, fill = strand),
                                 facets = sample ~ seqnames)

## @knitr geom_segment
ggplot() + stat_stepping(gr, aes(color = strand),
                         geom = "segment", xlab = "Genomic coord", ylab = "y", main = "hello")


## @knitr geom_alignment
ggplot() + stat_stepping(gr, geom = "alignment")

## @knitr geom_alignment_group
ggplot() + stat_stepping(gr, aes(group = pair),geom = "alignment")
}
\author{Tengfei Yin}

