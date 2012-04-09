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
                 facets = NULL, geom = c("gene", "reduced_gene"),
                 names.expr = expression(paste(tx_name, "(", gene_id,
                 ")", sep = "")))


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
    The geometric object to use display the data. 'gene' shows full gene
    model with 5'-utr, 3'-utr and cds. "reduced_gene" shos reduced
    single gene structure.
  }
  \item{names.expr}{
    Expression for showing y label.
  }
}
\value{
  A 'Layer'.
}
\examples{
## @knitr load
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], fill = "gray40", geom = "gene")
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
tracks(p1, p2, heights = c(3, 1))
}
\author{Tengfei Yin}

