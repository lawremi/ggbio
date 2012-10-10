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
                          maxgap=0L, minoverlap=1L,
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
  \item{maxgap, minoverlap}{
    It passed to \code{findOverlaps}.
    
    Intervals with a separation of \code{maxgap} or less and a minimum
    of \code{minoverlap} overlapping positions, allowing for
    \code{maxgap}, are considered to be overlapping.  \code{maxgap}
    should be a scalar, non-negative, integer. \code{minoverlap}
    should be a scalar, positive integer.
  }
  \item{type}{
    It passed to \code{findOverlaps}.
    
    By default, any overlap is accepted. By specifying the \code{type}
    parameter, one can select for specific types of overlap. The types
    correspond to operations in Allen's Interval Algebra (see
    references). If \code{type} is \code{start} or \code{end}, the
    intervals are required to have matching starts or ends,
    respectively. While this operation seems trivial, the naive
    implementation using \code{outer} would be much less
    efficient. Specifying \code{equal} as the type returns the
    intersection of the \code{start} and \code{end} matches. If
    \code{type} is \code{within}, the query interval must be wholly
    contained within the subject interval. Note that all matches must
    additionally satisfy the \code{minoverlap} constraint described above.

    The \code{maxgap} parameter has special meaning with the special
    overlap types. For \code{start}, \code{end}, and \code{equal},
    it specifies the maximum difference in the starts, ends or both,
    respectively. For \code{within}, it is the maximum amount by which
    the query may be wider than the subject.
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
ggplot(gr) + stat_aggregate(window = 30,  aes(y = value),fill = "gray40", geom = "histogram")
ggplot(gr) + stat_aggregate(window = 100, fill = "gray40", aes(y = value),
                           method = "max", geom = "histogram")

ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot")
ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot", window = 60)
## now facets need to take place inside stat_* geom_* for an accurate computation
ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot", window = 30,
              facets = sample ~ seqnames)
autoplot(gr, stat = "aggregate", aes(y = value))
autoplot(gr, stat = "aggregate", geom = "boxplot", aes(y = value), window = 36)
}
\author{Tengfei Yin}


