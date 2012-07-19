\name{stat_gene}
\alias{stat_gene}
\alias{stat_gene,TranscriptDb-method}
\title{Calculate gene structure}
\description{
  Calculate gene structure.
}
\usage{
\S4method{stat_gene}{TranscriptDb}(data, ..., which, xlim, truncate.gaps = FALSE,
           truncate.fun = NULL, ratio = 0.0025, xlab, ylab, main,
           facets = NULL, geom = "gene", stat = c("identity", "reduce"),
           gap.geom = c("arrow", "chevron", "segment"),
           names.expr = "tx_name(gene_id)")
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
  }
  \item{which}{
    \code{GRanges} object to subset the \code{TranscriptDb} object.
  }
  \item{xlim}{
    Limits for x, to subset the \code{TranscriptDb} object.
  }
  \item{truncate.gaps}{
    logical value indicate to truncate gaps or not.
  }
  \item{truncate.fun}{
    shrinkage function. Please see \code{shrinkagefun} in package biovizBase.
  }
  \item{ratio}{
    used in \code{maxGap}.
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
    geometric object. only support "gene" now.
  }
  \item{stat}{
    defualt "identity" give full gene model and "reduce" for reduced model.
  }
  \item{gap.geom}{
    geom to show as introns. default is arrow.
  }
  \item{names.expr}{
    Expression for showing y label.
  }
}
\value{
  A 'Layer'.
}
\examples{
## loading package
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## made a track comparing full/reduce stat.
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"])
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], stat = "reduce")
tracks(full = p1, reduce = p2, heights = c(3, 1))

## change y labels
ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], names.expr = "tx_id:::gene_id")
}
\author{Tengfei Yin}

