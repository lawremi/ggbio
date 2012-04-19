\name{stat_aggregate}
\alias{stat_aggregate}
\alias{stat_aggregate,GRanges-method}
\title{Generates summaries on the specified windows}
\description{
Generates summaries on the specified windows  
}
\usage{
% for GRanges
\S4method{stat_aggregate}{GRanges}(data, ..., xlab, ylab, main, by, FUN, start = NULL,
                 end = NULL, width = NULL, y = NULL, frequency = NULL,
                 delta = NULL, simplify = TRUE, window = NULL, facets =
                 NULL, type = c("mean", "median", "max", "min", "sum",
                 "count", "identity"), geom = NULL)

}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
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
  \item{by}{
    An object with 'start', 'end', and 'width' methods. Passed to \code{aggreagate}.
  }
  \item{FUN}{
    The function, found via 'match.fun', to be applied to each window of
    'x'. Passed to \code{aggreagate}.
  }
  \item{start}{
    Start of the window. If 'by' is missing, then must supply two of the
    'start', 'end', 'width'. If 'window' is provided then you don't have
    to specify it.
  }
  \item{end}{
    End of the window. If 'by' is missing, then must supply two of the
    'start', 'end', 'width'. If 'window' is provided then you don't have
    to specify it.
  }
  \item{width}{
    Width of the window. If 'by' is missing, then must supply two of the
    'start', 'end', 'width'. If 'window' is provided then you don't have
    to specify it.
  }
  \item{y}{
    A character indicate the varialbe column for which aggregation is
    taken on. Notice for geom like 'boxplot', we don't compute or
    aggregate the variable, we simply want to use the idenitcal y as y
    axis, in that case please put y in the \code{aes} mapping function.
  }
  \item{frequency}{
   Optional arguments that specify the sampling frequency within the window.    
  }
  
  \item{delta}{
   Optional arguments that specify the sampling increment within the window.    
  }
  \item{...}{
    Arguments passed to plot function. such as aes() and color.
  }
  \item{simplify}{
    A logical value specifying whether or not the
    result should be simplified to a vector or matrix if
    possible.    
  }
  \item{window}{
    Integer value indicate window size. 
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{type}{
    
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
}
\value{
  A 'Layer'.
}
\examples{
## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr simul
set.seed(1)
N <- 1000
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

## 36,37 doesn't work
ggplot() + stat_aggregate(gr, y = "value",fill = "gray40")
ggplot() + stat_aggregate(gr, window = 30,  y = "value",fill = "gray40", geom = "histogram")
ggplot() + stat_aggregate(gr, window = 100, fill = "gray40",y = "value",
                          type = "max", geom = "histogram")

}
\author{Tengfei Yin}


