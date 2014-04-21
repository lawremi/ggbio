\name{Ideogram}
\alias{Ideogram}
\alias{Ideogram-class}
\alias{plotIdeogram}
\alias{plotSingleChrom}
\alias{+,Ideogram,ANY-method}
\title{Plot single chromosome with cytoband}
\usage{

plotIdeogram(obj, subchr = NULL, zoom.region = NULL, which = NULL, xlab, ylab, main, xlabel =
                 FALSE, color = "red", fill = "red", alpha = 0.7,
                 zoom.offset = 0.2, size = 1,
                 cytoband = TRUE, aspect.ratio = 1/20, genome)

## constructor
Ideogram(obj, subchr = NULL, which = NULL, xlabel = FALSE,
                 cytoband = TRUE, color = "red", fill = "red", alpha =
                 0.7, zoom.region = NULL, zoom.offset = 0.2, size = 1,
                 aspect.ratio = 1/20, ..., genome)

## depricated
plotSingleChrom(obj, subchr = NULL, zoom.region, xlab, ylab, main, xlabel =
                 FALSE, color = "red", fill = "red", alpha = 0.7,
                 zoom.offset = 0.1, size = 1,
                 cytoband = TRUE, aspect.ratio = 1/20, genome)

}
\description{
  Plot single chromosome with cytoband. \code{plotIdeogram} is synonyms
  to \code{plotSingleChrom}.
}
\details{
  User could provide the whole ideogram and use subchr to point to
  particular chromosome.
}
\value{
  A \code{ggplot} object.
}
\author{Tengfei Yin}
\arguments{
  \item{obj}{
    A \code{GenomicRanges} object, which include extra
    information about cytoband, check biovizBase::isIdeogram.
  }
  \item{subchr}{
    A single character of chromosome names to show.
  }
  \item{which}{
    \code{GRanges} object to subset and highlight the ideogram.
  }
  \item{zoom.region}{
    A numeric vector of length 2 indicating zoomed region.
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
  \item{xlabel}{
    A logical value. Show the x label or not.
  }
  \item{color}{
    color for highlight region.
  }
  \item{fill}{
    fill color for highlight region.
  }
  \item{alpha}{
    alpha for highlight regio.
  }
  \item{zoom.offset}{
    zoomed highlights region offset around chromosome plotting region.
  }
  \item{size}{
    size for zoomed region rectangle boundary.
  }
  \item{cytoband}{
    If FALSE, plot just blank chromosome without cytoband. default is TRUE.
es  }
  \item{aspect.ratio}{
    aspect ratio for the chromosome ideogram plot, default is NULL.
  }
  \item{genome}{
    genome character passed to \code{\link{getIdeogram}}
  }
  \item{...}{
    passed to ggbio constructor.
  }
}
\examples{
\dontrun{
library(biovizBase)
p.ideo <- Ideogram(genome = "hg19")
p.ideo
library(GenomicRanges)
p.ideo + xlim(GRanges("chr2", IRanges(1e8, 1e8+10000)))
Ideogram(genome = "hg19", xlabel = TRUE)
}
}

