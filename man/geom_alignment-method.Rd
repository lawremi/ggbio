\name{geom_alignment}
\alias{geom_alignment}
\alias{geom_alignment,GRanges-method}
\title{Alignment geoms for GRanges object}
\description{
  Show interval data as alignment.
}
\usage{
% for GRanges
\S4method{geom_alignment}{GRanges}(data,..., xlab, ylab, main,
               facets = NULL, stat = c("stepping", "identity"),
               main.geom = c("rect", "arrowrect"),
               gap.geom = c("chevron", "arrow", "segment"),
               rect.height = 0.4,  group.selfish = TRUE)
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
  \item{main.geom}{
    Geom for 'main' intervals which is the data you passed.
  }
  \item{gap.geom}{
    Geom for 'gap' computed from the data you passed based on the group information.
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

