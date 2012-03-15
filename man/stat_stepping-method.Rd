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
    The geometric object to use display the data. For 'stepping', could
    be one of 'rect', 'alignment', 'segment'.
  }
}
\value{
  A 'Layer'.
}
\examples{
}
\author{Tengfei Yin}

