\name{geom_arrow}
\alias{geom_arrow}
\alias{geom_arrow,GRanges-method}
\title{Arrow geoms for GRanges object}
\description{
  Show interval data as arrows.
}
\usage{
\S4method{geom_arrow}{GRanges}(data, ...,
              angle = 30, length = unit(0.15, "cm"),
              type = "open", stat = c("stepping", "identity"),
              facets = NULL, arrow.rate = 0.05,
              group.selfish = TRUE)
}
\arguments{
  \item{data}{
    A \code{GRanges} object.
  }
  \item{...}{
    Extra parameters such as aes() passed.
  }
  \item{angle}{
     The angle of the arrow head in degrees (smaller numbers produce
  narrower, pointier arrows).  Essentially describes the width of the
  arrow head.
  }
  \item{length}{
    A unit specifying the length of the arrow head (from tip to base).
  }
  \item{type}{
  One of "open" or "closed" indicating whether the arrow head should
  be a closed triangle.
  }
  \item{stat}{
    Character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{arrow.rate}{
    Arrow density of the arrow body.
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

