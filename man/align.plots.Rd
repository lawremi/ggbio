\name{align.plots}
\alias{align.plots}
\title{align plots on x axis}
\description{
  align plots on x axis
}
\usage{
align.plots(..., vertical = TRUE,
            heights = unit(rep(1, nrow), "null"))
}
\arguments{
  \item{...}{
    ggplot2 graphic object.
  }
  \item{vertical}{
    logical value indicate how to align the plots, currently only
    support vertical alignments.
  }
  \item{heights}{
    numeric values or units, indicate the heights of each track.
  }
}
\value{
  return a frame grob; side-effect (plotting) if plot=T.
}
\seealso{
  \code{\link{tracks}}
}
\examples{
## @knitr load
## ==========================================================
## Load packages
## ==========================================================
## Load gene features for human
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
## ==========================================================
## Create tracks
## ==========================================================
## create two tracks
## full gene model
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "gene")
## reduced gene model
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
## @knitr align.plots
## ==========================================================
## align.plots
## ==========================================================
}
\author{Tengfei Yin, Baptiste Auguie}

