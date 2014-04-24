\name{Plot}
\alias{Plot}
\alias{Plot-class}
\alias{ggplotPlot-class}
\alias{latticePlot-class}
\alias{ggbioPlot-class}
\alias{Plot,gg-method}
\alias{Plot,trellis-method}
\alias{Plot,GGbio-method}
\alias{Plot,Ideogram-method}
\title{Plot class}
\description{
  genealize a graphic object to a Plot object.
}
\usage{
  \S4method{Plot}{gg}(x)
  \S4method{Plot}{trellis}(x, mutable = FALSE)
  \S4method{Plot}{GGbio}(x)
  \S4method{Plot}{Ideogram}(x)
}
\arguments{
  \item{x}{
    object of gg, GGbio, trellis, Ideogram.
  }
  \item{mutable}{
    whether a plot repsonse to \code{+} method or not.
  }
}
\value{
  A Plot object.
}
\author{Tengfei Yin}

