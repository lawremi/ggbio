\name{zoom}
\alias{zoom}
\alias{zoom_in}
\alias{zoom_out}
\alias{nextView}
\alias{prevView}
\title{Simple navigation for ggbio object.}
\description{
  A set of simple navigation API apply to ggbio object, let you move
  along the genome and zoom in/out.
}
\usage{
zoom(fac = 1/2)
zoom_in(fac = 1/2)
zoom_out(fac = 2)
nextView(unit = c("view", "gene", "exon", "utr"))
prevView(unit = c("view", "gene", "exon", "utr"))
}
\arguments{
  \item{fac}{
    numeric value to indicate zoom factor, multiple of current view
    width. If it's smaller than 1, then it's zoom-in operation; if it's bigger
    than 1, then it's zoom-out operation.
  }
  \item{unit}{
    only support 'view' unit now.
  }
}
\value{
  A special class of navigation.
}
\details{
  \code{zoom_in} and \code{zoom_out} are just simple wrapper around
  \code{zoom} function.

  For more convenient, gene features based jumpting we will support it
  in the future.
}
\author{Tengfei Yin}

