\name{plotSpliceSum}
\alias{plotSpliceSum}
\alias{plotSpliceSum,character,GRangesList-method}
\alias{plotSpliceSum,character,TxDb-method}
\alias{plotSpliceSum,character,EnsDb-method}
\title{Plot Splice Summary from RNA-seq data}
\description{
  Plot splice summary by simply counting overlaped junction read in
  weighted way or not.
}
\usage{
## For character,GRangesList
\S4method{plotSpliceSum}{character,GRangesList}(data, model, ..., weighted = TRUE)
## For character,TxDb
\S4method{plotSpliceSum}{character,TxDb}(data, model, which,
      ..., weighted = TRUE)
## For character,EnsDb
\S4method{plotSpliceSum}{character,EnsDb}(data, model, which,
      ..., weighted = TRUE)
}
\arguments{
  \item{data}{
    A character specifying the bam file path of RNA-seq data.
  }
  \item{model}{
    A GRangesList which represting different isoforms, a TxDb or an
    \code{\link[ensembldb]{EnsDb}} object. For the latter cases, users
    need to pass "which" argument which, for TxDb, is a GRanges object
    to specify the region and for \code{EnsDb} can be a GRanges object,
    an object extending
    \code{\link[AnnotationFilter]{AnnotationFilter}}, an
    \code{\link[AnnotationFilter]{AnnotationFilterList}} combining such
    filter objects or a filter expression in form of a \code{formula}.
  }
  \item{which}{
    A GRanges object specifying the region you want to get model from
    the TxDb object.
    For \code{\link[ensembldb]{EnsDb}}: can be a GRanges object, an
    object extending \code{\link[AnnotationFilter]{AnnotationFilter}}, an
    \code{\link[AnnotationFilter]{AnnotationFilterList}} combining such
    filter objects or a filter expression in form of a \code{formula}.
  }
  \item{weighted}{
    If \code{TRUE}, weighted by simply add 1/cases matched to each model
    and if \code{FALSE}, simply add 1 to every case.
  }
  \item{...}{
    Extra arugments passed to \code{qplot} function. such as,
    \code{offset} which control the height of chevron.
  }
}
\value{
  A ggplot object.
}
\details{
  Internally we use biovizBase:::spliceSummary for simple counting, but
  we encourage users to use their own robust way to make slicing summary
  and store it as GRangesList, then plot the summary by \code{qplot}
  function.
}
\seealso{
  \code{\link{qplot}}
}
\examples{
\dontrun{
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
data(genesymbol)
exons <- exonsBy(txdb, by = "tx")
exons.rbm17 <- subsetByOverlaps(exons, genesymbol["RBM17"])
plotSpliceSum(bamfile, exons.rbm17)
plotSpliceSum(bamfile, exons.rbm17, weighted = FALSE, offset = 0.01)
plotSpliceSum(bamfile, txdb, which = genesymbol["RBM17"])
plotSpliceSum(bamfile, txdb, which = genesymbol["RBM17"], offset = 0.01)
plotSpliceSum(bamfile, txdb, which = genesymbol["RBM17"],
              show.label = TRUE,
              label.type = "count")
}
}
\author{Tengfei Yin}
