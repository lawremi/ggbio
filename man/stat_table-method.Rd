\name{stat_table}
\alias{stat_table}
\alias{stat_table,GRanges-method}
\alias{stat_table,GRangesList-method}
\title{Tabulate a GRanges object}
\description{
  Tabulate a GRanges object  
}
\usage{
\S4method{stat_table}{GRanges}(data, ..., xlab, ylab, main,
            geom = NULL,stat = NULL)
\S4method{stat_table}{GRangesList}(data, ..., xlab, ylab, main,
            facets = NULL, geom = NULL)

}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{xlab}{
    Label for x
  }
  \item{ylab}{
    Label for y
  }
  \item{main}{
    Title for plot.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
  \item{stat}{
    The geometric object to use display the data. 
  }
}
\value{
  A 'Layer'.
}
\examples{
}
\author{Tengfei Yin}

