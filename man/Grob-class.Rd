\name{Grob-class}
\alias{Grob-class}
\alias{Grob}
\alias{Grob-method}
\alias{Grob,gg-method}
\alias{Grob,gtable-method}
\alias{Grob,trellis-method}
\alias{Grob,lattice-method}
\alias{Grob,GGbio-method}
\alias{GrobList-class}
\alias{ggplotGrob-class}
\alias{latticeGrob-class}
\alias{GrobList}
\title{Grob getter}
\description{
  'Grob' class is a container for 'grob' based object defined with grid
  system. Generic function \code{Grob} gets grob object supported by grid system, and make an instance of
  subclass of class 'Grob'.

  'GrobList' is a container of list of 'Grob' object.
}
\usage{
\S4method{Grob}{gg}(x)
\S4method{Grob}{gtable}(x)
\S4method{Grob}{trellis}(x)
\S4method{Grob}{lattice}(x)
\S4method{Grob}{GGbio}(x)
}
\arguments{
  \item{x}{
    object of class: gg, gtable, trellis, lattice, GGbio.
  }
}
\value{
  A \code{Grob} object.
}
\author{Tengfei Yin}

