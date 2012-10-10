\name{stat_gene}
\alias{stat_gene}
\alias{stat_gene,TranscriptDb-method}
\title{Calculate gene structure}
\description{
  Calculate gene structure.
}
\usage{
\S4method{stat_gene}{TranscriptDb}(data, ...)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_alignment}.
  }
}
\value{
  A 'Layer'.
}
\seealso{\code{\link{geom_alignment}}}
\examples{
\dontrun{
## loading package
## Deprecated
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## made a track comparing full/reduce stat.
p1 <- ggplot() + geom_alignment(txdb, which = genesymbol["RBM17"])
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"])
## or 
p1 <- ggplot(txdb) + stat_gene(which = genesymbol["RBM17"])

p1 <- ggplot(txdb) + stat_gene(which = genesymbol["RBM17"])
p2 <- ggplot(txdb) + stat_gene(which = genesymbol["RBM17"], stat =
"reduce")
p2 <- ggplot(txdb) + stat_gene(which = genesymbol["RBM17"], stat = "reduce")
## ggplot(txdb) + geom_alignment(which = genesymbol["RBM17"]) + stat_reduce()
## ggplot(txdb) + geom_alignment(which = genesymbol["RBM17"])
tracks(full = p1, reduce = p2, heights = c(3, 1))

## change y labels
ggplot(txdb) + stat_gene(which = genesymbol["RBM17"], names.expr =
"tx_id:::gene_id")
}
}
\author{Tengfei Yin}

