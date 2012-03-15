\name{layout_karyogram}
\alias{layout_karyogram}
\alias{layout_karyogram,GRanges-method}
\title{Create a karyogram layout}
\description{
  Create a karyogram layout.
}
\usage{
\S4method{layout_karyogram}{GRanges}(data,..., xlab, ylab, main,
                 facets = seqnames ~ ., cytoband = FALSE,
                 geom = NULL, stat = NULL, ylim = NULL,
                 rect.height = 10)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes().
  }
  \item{xlab}{
    character vector or expression for x axis label
  }
  \item{ylab}{
    character vector or expression for y axis label  
  }
  \item{main}{
    character vector or expression for plot title
  }
  \item{facets}{
    faceting formula to use
  }
  \item{cytoband}{
    Logical value indicate to show the cytobands or not.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
  \item{stat}{
    character vector specifying statistics to use
  }
  \item{ylim}{
    limits for y axis
  }
  \item{rect.heigh}{
    numreic value indicate half of the rectangle ploting region, used
    for alignment of multiple layers.
  }
}
\value{
  A 'Layer'.
}
\examples{

}
\author{Tengfei Yin}

