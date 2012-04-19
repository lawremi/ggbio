\name{theme_alignment}
\alias{theme_alignment}
\title{Theme for alignment}
\description{
  Theme for alignment
}
\usage{
theme_alignment(label = FALSE, base_size = 12,
                base_family = "", axis = TRUE,
                border = TRUE, grid = TRUE)
}
\arguments{
  \item{label}{
    logical value. Show labels or not.
  }
  \item{base_size}{
    size for font
  }
  \item{base_family}{
    family for font
  }
  \item{axis}{
    logical value, show axis or not.
  }
  \item{border}{
    logical value, show border or not.
  }
  \item{grid}{
    logical value, show background grid or not.
  }
}
\value{
  Return a options list.
}
\examples{
## @knitr load
library(ggbio)
data(genesymbol, package = "biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr theme:default
p <- autoplot(txdb, which = genesymbol["RBM17"])
p

## @knitr theme:tweak
p + theme_alignment(border = TRUE, grid = FALSE, label = TRUE)
}
\author{Tengfei Yin}

