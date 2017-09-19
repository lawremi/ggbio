\name{stat_aggregate}
\alias{stat_aggregate}
\alias{stat_aggregate,GRanges-method}
\alias{stat_aggregate,missing-method}
\alias{stat_aggregate,uneval-method}
\title{Generates summaries on the specified windows}
\description{
Generates summaries on the specified windows  
}
\usage{
% for GRanges
\S4method{stat_aggregate}{GRanges}(data, ..., xlab, ylab, main, by, FUN,
                          maxgap=-1L, minoverlap=0L,
                          type=c("any", "start", "end", "within", "equal"),
                          select=c("all", "first", "last", "arbitrary"),
                          y = NULL, window = NULL, facets = NULL, 
                          method = c("mean", "median","max",
                                   "min", "sum", "count", "identity"),
                          geom = NULL)


}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Arguments passed to plot function. such as aes() and color.
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
  \item{maxgap, minoverlap, type}{
    Used in the internal call to \code{findOverlaps()} to detect overlaps.
    See \code{?\link[IRanges]{findOverlaps}} in the \pkg{IRanges} package
    for a description of these arguments.
  }
  \item{select}{
    It passed to \code{findOverlaps}.
    
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link{Hits} object. When \code{select} is \code{"first"},
    \code{"last"}, or \code{"arbitrary"} the results are returned as an
    integer vector of length \code{query} containing the first, last,
    or arbitrary overlapping interval in \code{subject}, with \code{NA}
    indicating intervals that did not overlap any intervals in \code{subject}.
 
    If \code{select} is \code{"all"}, a \link{Hits} object is returned.
    For all other \code{select} the return value depends on the \code{drop}
    argument. When \code{select != "all" && !drop}, an \link{IntegerList}
    is returned, where each element of the result corresponds to a space in
    \code{query}. When\code{select != "all" && drop}, an integer vector is
    returned containing indices that are offset to align with the unlisted
    \code{query}.
  }
  \item{y}{
    A character indicate the varialbe column for which aggregation is
    taken on, same as aes(y = ).
  }
  \item{window}{
    Integer value indicate window size. 
  }
  \item{facets}{
    Faceting formula to use.
  }
  
  \item{method}{
    customized method for aggregating, if FUN is not provided. 
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
}
\value{
  A 'Layer'.
}
\examples{
library(GenomicRanges)
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


ggplot(gr) + stat_aggregate(aes(y = value))
## or
## ggplot(gr) + stat_aggregate(y = "value")
ggplot(gr) + stat_aggregate(aes(y = value), window = 36)
ggplot(gr) + stat_aggregate(aes(y = value), select = "first")
\dontrun{
## no hits 
ggplot(gr) + stat_aggregate(aes(y = value), select = "first", type = "within")
}
ggplot(gr) + stat_aggregate(window = 30,  aes(y = value),fill = "gray40", geom = "bar")
ggplot(gr) + stat_aggregate(window = 100, fill = "gray40", aes(y = value),
                           method = "max", geom = "bar")

ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot")
ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot", window = 60)
## now facets need to take place inside stat_* geom_* for an accurate computation
ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot", window = 30,
              facets = sample ~ seqnames)
## FIXME:
## autoplot(gr, stat = "aggregate", aes(y = value), window = 36)
## autoplot(gr, stat = "aggregate", geom = "boxplot", aes(y = value), window = 36)
}
\author{Tengfei Yin}


