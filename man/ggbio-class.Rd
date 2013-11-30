\name{GGbio}
\alias{ggbio}
\alias{GGbio}
\alias{ggbio-class}
\alias{+,GGbio,ANY-method}
\alias{$,GGbio-method}
\alias{$<-,GGbio-method}
\title{class ggbio}
\description{
  a sub class of ggplot and gg class defined in ggplot2 package, used for ggbio specific methods.
}
\usage{
  GGbio(ggplot = NULL, data = NULL, fetchable = FALSE, blank =
                 FALSE, ...)
}
\arguments{
  \item{ggplot}{
    a ggplot or gg object.
  }
  \item{data}{
    raw data
  }
  \item{fetchable}{
    logical value, default \code{FALSE}, is there any fetch method available. 
  }
  \item{blank}{
    logical value, default \code{FALSE}, is this plot a blank plot.
  }
  \item{...}{
    More properties passed to class like \code{Cache}.
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

