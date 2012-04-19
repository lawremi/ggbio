\name{plotFragLength}
\alias{plotFragLength}
\alias{plotFragLength,character,GRanges-method}
\title{Plot estimated fragment length for paired-end RNA-seq data}
\description{
  Plot estimated fragment length for paired-end RNA-seq data against
  single reduced data model. 
}
\usage{
\S4method{plotFragLength}{character,GRanges}(data, model,
               gap.ratio = 0.0025,
               geom = c("segment", "point", "line"),
               type = c("normal", "cut"),
               heights = c(400, 100),
               annotation = TRUE)
}
\arguments{
  \item{data}{
    A character indicate the bam file.
  }
  \item{model}{
    A reduced model to compute estimated fragment length. please see
    details.
  }
  \item{gap.ratio}{
    When type is set to "cut", it will provide a compact view, which cut
    the common gaps in a certain ratio.
  }
  \item{geom}{
    One or all three geoms could be drawn at the same time. y value of
    "point" and "line" indicate the estimated fragment length. and if
    geom is set to "segment", the segment is from the left most position
    to paired right most position, should be equal to "isize".
  }
  \item{type}{
    "normal" return a uncut view, loose but the coordinate is true
    genomic coordinates. "cut" cut the view in a compact way.
  }
  \item{heights}{
    Numeric vector indicate the heights of tracks.
  }
  \item{annotation}{
    A logical value. TRUE shows model, and FALSE shows only fragment
    length with labels.
  }
}
\value{
  A ggplot object when \code{annotation = FALSE} and a frame grob if
  \code{annotation = TRUE}
}
\details{
  We use a easy way to define this estimated fragment length, we
  collect all paired reads and model, reduce model first, then find
  common gaps, remove common gaps between paired-end reads, and compute
  the new estimated fragment length. 
}
\examples{
\dontrun{
data(genesymbol)
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
model <- exonsBy(txdb, by = "tx")
model.new <- subsetByOverlaps(model, genesymbol["RBM17"])
exons.rbm17 <- subsetByOverlaps(exons(txdb), genesymbol["RBM17"])
exons.new <- reduce(exons.rbm17)
plotFragLength(bamfile, exons.new, geom = "line")
plotFragLength(bamfile, exons.new, geom = c("point","segment"))
plotFragLength(bamfile, exons.new, geom = c("point","segment"), annotation = FALSE)
plotFragLength(bamfile, exons.new, geom = c("point","segment"), type = "cut",
               gap.ratio = 0.001)
}
}
\author{Tengfei Yin}

