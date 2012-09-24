\name{stat_stepping}
\alias{stat_stepping}
\alias{stat_stepping,missing-method}
\alias{stat_stepping,uneval-method}
\alias{stat_stepping,GRanges-method}
\title{Calculate stepping levels}
\description{
  Calculate stepping levels.
}
\usage{
\S4method{stat_stepping}{GRanges}(data, ..., xlab, ylab, main,
              facets = NULL,
              geom = c("rect", "alignment", "segment"))
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
  \item{facets}{
    Faceting formula to use.
  }
  \item{geom}{
    The geometric object used to display the data. For 'stepping', could
    be one of 'rect', 'alignment', 'segment'.
  }
}
\value{
  A 'Layer'.
}
\examples{

set.seed(1)
N <- 50

require(GenomicRanges)
##  simul
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

##  default 
ggplot(gr) + stat_stepping()
## or
ggplot() + stat_stepping(gr)

##  facet_aes
ggplot(gr) + stat_stepping(aes(color = strand, fill = strand),
                                 facets = sample ~ seqnames)

##  geom_segment
ggplot(gr) + stat_stepping(aes(color = strand),
                         geom = "segment", xlab = "Genomic coord", ylab = "y", main = "hello")


##  geom_alignment
## ggplot(gr) + stat_stepping(geom = "alignment")

##  geom_alignment_group
##  ggplot(gr) + stat_stepping(aes(group = pair),geom = "alignment")
}
\author{Tengfei Yin}

