\name{scale_x_sequnit}
\alias{scale_x_sequnit}
\title{scale x by unit}
\description{
  scale x by unit 'Mb','kb', 'bp'.
}
\usage{
scale_x_sequnit(unit = c("Mb", "kb", "bp"), append = NULL, ...)
}
\arguments{
  \item{unit}{
    unit to scale x. Default is Mb.
  }
  \item{append}{
    default \code{NULL}. If pass a character, it disable unit and arbitrarily append a
    text behind the original x scale numbers.
  }
  \item{...}{
    arguments to pass to \code{scale_x_continuous}.
    Note that \code{scale_x_sequnit} sets breaks and labels
    automatically, so arguments \code{breaks} and \code{labels} cannot
    be passed (\code{breaks} can if \code{append} is not set).
  }
}
\value{
  'position_c'
}
\examples{
library(ggplot2)
p <- qplot(x = seq(1, to = 10000, length.out = 40), y = rnorm(40), geom
= "point")
## default mb
p + scale_x_sequnit()
p + scale_x_sequnit("kb")
p + scale_x_sequnit("bp")
}
\author{Tengfei Yin}
\author{samuel barreto}
