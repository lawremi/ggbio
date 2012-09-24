\name{scale_x_sequnit}
\alias{scale_x_sequnit}
\title{scale x by unit}
\description{
  scale x by unit 'Mb','kb', 'bp'.
}
\usage{
scale_x_sequnit(unit = c("Mb", "kb", "bp"))
}
\arguments{
  \item{unit}{
    unit to scale x. 
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

