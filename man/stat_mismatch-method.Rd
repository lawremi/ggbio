\name{stat_mismatch}
\alias{stat_mismatch}
\alias{stat_mismatch,missing-method}
\alias{stat_mismatch,uneval-method}
\alias{stat_mismatch,GRanges-method}
\alias{stat_mismatch,BamFile-method}
\title{Calculate mismatch summary}
\description{
  Calculate mismatch summary  
}
\usage{
## for GRanges
\S4method{stat_mismatch}{GRanges}(data, ..., bsgenome, 
              xlab, ylab, main,
              geom = c("segment", "bar"),
              show.coverage = TRUE)
## for BamFile
\S4method{stat_mismatch}{BamFile}(data, ..., bsgenome, which,
              xlab, ylab, main,
              geom = c("segment", "bar"),
              show.coverage = TRUE)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{BamFile} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{bsgenome}{
    \code{BSgenome} object.
  }
  \item{which}{
    \code{GRanges} object to subset the data.
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
  \item{show.coverage}{
    whether to show coverage as background or not.
  }
}
\value{
  A 'Layer'.
}
\author{Tengfei Yin}

