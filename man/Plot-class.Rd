\name{Plot-class}
\alias{Plot-class}
\alias{Plot}
\title{Plot Class}
\description{
  Plot class is a S4 class which describe a containter for special Plot
  object which only allow to be embed into tracks. This abstract allow
  not only ggplot2 object but also some other objects could be
  implemented into support for tracks building. Plot extends class 'Tracked'.
}
\usage{
Plot(x)
}
\arguments{
  \item{x}{
    Any of the supported graphics objects, which could be turned into a
    'Plot' sub-class instance. 
  }
}
\value{
  A 'plot' object.
}
\seealso{
  \link{Tracked-class}
}
\examples{
require(lattice)
require(testthat)
require(ggplot2)
require(gridExtra)
x = 1:3
p1 <- qplot(x = mpg, y = wt, data = mtcars)
p2 <- xyplot(1:10 ~ 1:10)
require(ggbio)
p3 <- new("GGbio", p1)
Plot(p1)
Plot(p2)
Plot(p3)
PlotList(p1, p2, p3)
}
\author{Tengfei Yin}

