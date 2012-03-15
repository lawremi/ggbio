\name{geom_arch}
\alias{geom_arch}
\alias{geom_arch,data.frame-method}
\alias{geom_arch,GRanges-method}
\title{Arch geoms for GRanges object}
\description{
  Show interval data as arches.
}
\usage{
% for data.frame
\S4method{geom_arch}{data.frame}(data, ..., n = 25, max.height = 10)

% for GRanges
\S4method{geom_arch}{GRanges}(data, ..., facets = NULL, rect.height = 0.4,
          n = 25, max.height = 10)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters passed to autoplot function, \code{aes} mapping
    support \code{height, x, xend}.
    \itemize{
      \item{x}{start of the arches}
      \item{xend}{end of the arches}
      \item{height}{height of arches}
    }
  }
  \item{n}{
    Integer values at which interpolation takes place to create 'n'
    equally spaced points spanning the interval ['min(x)', 'max(x)'].
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{rect.height}{
    When data is \code{GRanges}, this padding the arches from original y
    value to allow users putting arches 'around' the interval rectangles.
  }
  \item{max.height}{
    Max height of all arches.
  }  
}
\value{
  A 'Layer'.
}
\details{
  To draw a interval data as arches, we need to provide a special geom for
this purpose. Arches is popular in gene viewer or genomoe browser,
when they try to show isoforms or gene model.\code{geom_arch},
just like any other \code{geom_*} function in ggplot2, you can pass
aes() to it to map variable to height of arches.
}

\examples{
}
\author{Tengfei Yin}

