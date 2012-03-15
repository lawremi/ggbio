\name{geom_arrowrect}
\alias{geom_arrowrect}
\alias{geom_arrowrect,GRanges-method}
\title{Arrowrect geoms for GRanges object}
\description{
  Show interval data as rectangle with a arrow head.
}
\usage{
\S4method{geom_arrowrect}{GRanges}(data, ..., facets = NULL,
                          stat = c("stepping", "identity"),
                          rect.height = 0.4,
                          arrow.head = 0.06,
                          group.selfish = TRUE)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed.
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

