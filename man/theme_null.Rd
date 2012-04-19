\name{theme_null}
\alias{theme_null}
\title{Blank theme}
\description{
  Theme with no background, axis, legend. Useful for circular plot.
}
\usage{
theme_null()
}
\value{
  Return a options list.
}
\examples{
## @knitr load
set.seed(1)
N <- 50
require(ggbio)
require(GenomicRanges)
## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = "chr1",
              IRanges(start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE))

## @knitr default
autoplot(gr)

## @knitr theme_null
autoplot(gr) + theme_null()
}
\author{Tengfei Yin}


