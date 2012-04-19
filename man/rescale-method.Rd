\name{rescale}
\alias{rescale}
\alias{rescale,numeric-method}
\alias{rescale,ggplot-method}
\title{rescale ggplot object}
\description{
  Rescale a numeric vector or ggplot object, could be used for static
  zoom-in in ggbio.
}
\usage{
## For signature numeric
\S4method{rescale}{numeric}(x, to = c(0, 1),
      from = range(x, na.rm = TRUE))

## For signature ggplot
\S4method{rescale}{ggplot}(x, xlim, ylim, sx = 1, sy = 1, wise = TRUE)
}
\arguments{
\item{x}{
  A numeric object or ggplot object to be rescaled. 
}
\item{to}{
  For numeric object. it's a vector of two numeric values, specifying
the range to be rescale.
}
\item{from}{
  Range of x. 
}
\item{xlim}{
  For ggplot object. This specify the new limits on x-scale.
}
\item{ylim}{
  For ggplot object. This specify the new limits on y-scale.
}
\item{sx}{
  Scale fold for x-scale. Default is 1, no change.
}
\item{sy}{
  Scale fold for y-scale. Default is 1, no change.
}
\item{wise}{
  wise: If 'TRUE' will wisely expand the actual range of the plot a
       little, in the way that setting the limits on the scales does
}
}
\value{
  Return the object of the same class as \code{x} after rescaling.
}
\details{
  When \code{x} is numeric value, it's just call scales::rescale, please
refer to the manual page to check more details. If \code{x} is ggplot
object, it first try to estimate current x limits and y limits of the ggplot
object, then rescale based on those information.
}
\examples{
library(ggbio)
head(mtcars)
range(mtcars$mpg)
p <- qplot(data = mtcars, x = mpg, y = disp, geom = "point")
p.new <- rescale(p, xlim = c(20, 25))
p.new
}
\author{Tengfei Yin}

