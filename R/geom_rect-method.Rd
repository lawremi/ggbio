\name{geom_rect}
\alias{geom_rect}
\alias{geom_rect,data.frame-method}
\alias{geom_rect,GRanges-method}
\title{Rect geoms for GRanges object}
\description{
  Show interval data as rectangle.
}
\usage{
## For data.frame
\S4method{geom_rect}{data.frame}(data, ...)
## For GRanges
\S4method{geom_rect}{GRanges}(data,..., xlab, ylab, main,
          facets = NULL, stat = c("stepping", "identity"),
          rect.height = 0.4,
          group.selfish = TRUE)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
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
}
\author{Tengfei Yin}

