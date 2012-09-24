\name{ggbio}
\alias{ggbio}
\alias{ggbio-class}
\alias{+,ggbio,ANY-method}
\title{class ggbio}
\description{
  a sub class of ggplot and gg class defined in ggplot2 package, used for ggbio specific methods.
}
\usage{
  ggbio(x)
}
\arguments{
  \item{x}{
    a ggplot or gg object.
  }
}
\value{
  a ggbio object.
}
\details{
  This class is defined to facilitate the ggbio-specific visualization
  method, especially when using \code{\link{ggplot}} to construct ggbio
  supported object, that will return a ggbio class. And internals tricks
  will help a lazy evaluation for following \code{+} method.
}
\seealso{
  \code{\link{ggplot}}
}
\examples{
p1 <- qplot()
g1 <- ggbio(p1)
class(g1)
}
\author{Tengfei Yin}

