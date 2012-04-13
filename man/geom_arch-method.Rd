\name{geom_arch}
\alias{geom_arch}
\alias{geom_arch,data.frame-method}
\alias{geom_arch,GRanges-method}
\title{Arch geoms for GRanges object}
\description{
  Show interval data as arches.
}
\usage{
% for data.frame
\S4method{geom_arch}{data.frame}(data, ..., n = 25, max.height = 10)

% for GRanges
\S4method{geom_arch}{GRanges}(data, ..., xlab, ylab, main, facets = NULL,
                 rect.height = 0, n = 25, max.height = 10)

}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters passed to autoplot function, \code{aes} mapping
    support \code{height, x, xend}.
    \itemize{
      \item{x}{start of the arches}
      \item{xend}{end of the arches}
      \item{height}{height of arches}
    }
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
  \item{n}{
    Integer values at which interpolation takes place to create 'n'
    equally spaced points spanning the interval ['min(x)', 'max(x)'].
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{rect.height}{
    When data is \code{GRanges}, this padding the arches from original y
    value to allow users putting arches 'around' the interval rectangles.
  }
  \item{max.height}{
    Max height of all arches.
  }  
}
\value{
  A 'Layer'.
}
\details{
  To draw a interval data as arches, we need to provide a special geom for
this purpose. Arches is popular in gene viewer or genomoe browser,
when they try to show isoforms or gene model.\code{geom_arch},
just like any other \code{geom_*} function in ggplot2, you can pass
aes() to it to map variable to height of arches.
}

\examples{
## @knitr load
set.seed(1)
N <- 100
library(ggbio)
library(GenomicRanges)

## @knitr simul
## =======================================
##  simmulated GRanges
## =======================================
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
## =======================================
##  default
## =======================================
ggplot() + geom_arch(gr)

## @knitr facet_aes
## =======================================
##  facetting and aesthetics
## =======================================
ggplot() + geom_arch(gr, aes(color = value, height = value, size = value),
                     alpha = 0.2, facets = sample ~ seqnames)


}
\author{Tengfei Yin}

