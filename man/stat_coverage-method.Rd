\name{stat_coverage}
\alias{stat_coverage}
\alias{stat_coverage,GRanges-method}
\alias{stat_coverage,GRangesList-method}
\title{Calculate coverage}
\description{
  Calculate coverage.
}
\usage{
% for GRanges
\S4method{stat_coverage}{GRanges}(data, ..., xlim, xlab, ylab, main,
              facets = NULL,
              geom = NULL)
% for GRangesList
\S4method{stat_coverage}{GRangesList}(data, ..., xlim, xlab, ylab, main,
              facets = NULL,
              geom = NULL)

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
}
\value{
  A 'Layer'.
}
\examples{
}
\author{Tengfei Yin}

