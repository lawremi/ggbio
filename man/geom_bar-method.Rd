\name{geom_bar}
\alias{geom_bar}
\alias{geom_bar,ANY-method}
\alias{geom_bar,GRanges-method}
\alias{geom_bar,missing-method}
\alias{geom_bar,chevron-method}
\title{Segment geoms for GRanges object}
\description{
  Show interval data as vertical bar, width equals to interval width and
  use 'score' or specified 'y' as y scale. 
}
\usage{
\S4method{geom_bar}{ANY}(data, ...)
\S4method{geom_bar}{GRanges}(data,..., xlab, ylab, main)
}
\arguments{
  \item{data}{
    Typically a \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() or \code{color, size} passed.
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
}
\details{
  Useful for showing bed like files, when imported as GRanges, have a
  extra 'score' column, use it as default y, you could also specify y by
  using aes(y = ). 
}
\value{
  A 'Layer'.
}
\examples{
## load
library(GenomicRanges)

## simul
set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b2 <- GRanges(seqnames = "chr2", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b <- c(gr.b, gr.b2)
## default use score as y

## bar
ggplot(gr.b) + geom_bar(aes(fill = value))
## or
ggplot() + geom_bar(gr.b, aes(fill = value))
ggplot(gr.b) + geom_bar(aes(y = value))
## equal to
autoplot(gr.b, geom = "bar")
}

