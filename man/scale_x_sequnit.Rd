\name{scale_x_sequnit}
\alias{scale_x_sequnit}
\title{scale x by unit}
\description{
  scale x by unit 'Mb','kb', 'bp'.
}
\usage{
scale_x_sequnit(unit = c("Mb", "kb", "bp"), append = NULL)
}
\arguments{
  \item{unit}{
    unit to scale x. Default is Mb. 
  }
  \item{append}{
    default \code{NULL}. If pass a character, it disalbe unit and arbitrarily append a
    text behind the original x scale numbers. 
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

