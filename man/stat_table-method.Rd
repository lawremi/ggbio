\name{stat_table}
\alias{stat_table}
\alias{stat_table,GRanges-method}
\alias{stat_table,GRangesList-method}
\title{Tabulate a GRanges object}
\description{
  Tabulate a GRanges object  
}
\usage{
\S4method{stat_table}{GRanges}(data, ..., xlab, ylab, main,
            geom = NULL,stat = NULL)
\S4method{stat_table}{GRangesList}(data, ..., xlab, ylab, main,
            facets = NULL, geom = NULL)

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
    The geometric object to use display the data. 
  }
  \item{stat}{
    The geometric object to use display the data. 
  }
}
\value{
  A 'Layer'.
}
\examples{
## @knitr load
set.seed(1)
N <- 100
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

gr <- c(gr[seqnames(gr) == "chr1"][sample(1:10, size = 1e4, replace = TRUE)],gr)

## @knitr default
ggplot() + stat_table(gr)
ggplot() + stat_table(gr, geom = "segment", aes(y = ..score.., color = ..score..))
ggplot() + stat_table(gr, aes(color = score))
}
\author{Tengfei Yin}

