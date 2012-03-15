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
}
\author{Tengfei Yin}

