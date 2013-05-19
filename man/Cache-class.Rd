\name{Cache-class}
\alias{Cache-class}
\alias{cached-method}
\alias{cached<-,Cache-method}
\alias{cached_xlim,-method}
\alias{cached_ylim-method}
\alias{Cache}
\title{Cache class}
\description{
  Cache class is a S4 class to describe a cached object used for ggbio,
  which allow cached features implemented.
}
\usage{
Cache(cached = FALSE, cached_xlim = NULL, cached_ylim = NULL)
}
\section{Slots}{
  \describe{
    \item{cached:}{
      logical value, cached or not.
    }
    \item{cached_xlim:}{
      numeric value of length 2, or \code{NULL}(default).
    }
    \item{cached_ylim:}{
      numeric value of length 2, or \code{NULL}(default).
    }
  }
}
\value{
  A 'Cache' object.
}
\examples{
showClass("Cache")

obj <- new("Cache")
obj
cached(obj)
cached(obj) <- FALSE
obj
cached_xlim(obj)
cached_xlim(obj) <- 1
cached_ylim(obj) <- c(0, 5)

obj <- Cache()

}
\author{Tengfei Yin}

