\name{stat_coverage}
\alias{stat_coverage}
\alias{stat_coverage,GRanges-method}
\alias{stat_coverage,GRangesList-method}
\alias{stat_coverage,BamFile-method}
\title{Calculate coverage}
\description{
  Calculate coverage.
}
\usage{
# for GRanges
\S4method{stat_coverage}{GRanges}(data, ..., xlim, xlab, ylab, main,
              facets = NULL, geom = NULL)
# for GRangesList
\S4method{stat_coverage}{GRangesList}(data, ..., xlim, xlab, ylab, main,
              facets = NULL, geom = NULL)

# for Bamfile
\S4method{stat_coverage}{BamFile}(data, ..., maxBinSize = 2^14, xlim,
                         which, xlab, ylab, main, facets = NULL, 
                        geom = NULL,  method = c("estimate", "raw"))

}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{xlim}{
    Limits for x.
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
    The geometric object to use display the data. 
  }
  \item{maxBinSize}{
    maxBinSize.
  }
  \item{method}{
    'estimate' for parsing estimated coverage(fast), 'raw' is slow and
    parse the accurate coverage.
  }
  \item{which}{
    \code{GRanges} which defines region to subset the results.
  }
}
\value{
  A 'Layer'.
}
\examples{
## @knitr load
library(ggbio)

## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
set.seed(1)
N <- 1000
library(GenomicRanges)
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


## @knitr geom
ggplot() + stat_coverage(gr)
ggplot() + stat_coverage(gr, geom = "point")
ggplot() + stat_coverage(gr, geom = "area")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "histogram")
}
\author{Tengfei Yin}

