\name{stat_reduce}
\alias{stat_reduce}
\alias{stat_reduce,missing-method}
\alias{stat_reduce,uneval-method}
\alias{stat_reduce,GRanges-method}
\alias{stat_reduce,IRanges-method}
\alias{stat_reduce,TxDbOREnsDb-method}
\title{Reduce an object.}
\description{
  Reduce \code{GRanges}, \code{IRanges} or \code{TxDb} object.
}
\usage{
\S4method{stat_reduce}{GRanges}(data, ...,
           xlab, ylab, main,
           drop.empty.ranges = FALSE,
           min.gapwidth = 1L,
           facets = NULL, geom = NULL)

\S4method{stat_reduce}{IRanges}(data, ...,
           xlab, ylab, main,
           drop.empty.ranges = FALSE,
           min.gapwidth = 1L,
           with.inframe.attrib=FALSE,
           facets = NULL, geom = NULL)

\S4method{stat_reduce}{TxDbOREnsDb}(data, ...)
}
\arguments{
  \item{data}{
    \code{GRanges}, \code{IRanges} or \code{TxDb} object.
  }
  \item{...}{
    passed to aesthetics mapping.
  }
  \item{xlab}{
    x label.
  }
  \item{ylab}{
    y label.
  }
  \item{main}{
    title.
  }
  \item{drop.empty.ranges}{
    pass to \code{\link[IRanges]{reduce}} function.
  }
  \item{min.gapwidth}{
    pass to \code{\link[IRanges]{reduce}} function.
  }
  \item{with.inframe.attrib}{
    pass to \code{\link[IRanges]{reduce}} function.
  }
  \item{facets}{
    pass to \code{\link[IRanges]{reduce}} function.
  }
  \item{geom}{
    geometric type.
  }
}
\value{
  a ggplot object.
}
\seealso{
  \code{\link[IRanges]{reduce}}.
}
\examples{
set.seed(1)
N <- 1000
library(GenomicRanges)

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

ggplot(gr) + stat_reduce()
autoplot(gr, stat = "reduce")
strand(gr) <- "*"
ggplot(gr) + stat_reduce()

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
## made a track comparing full/reduce stat.
ggplot(txdb) + stat_reduce(which = genesymbol["RBM17"])

}
\author{Tengfei Yin}

