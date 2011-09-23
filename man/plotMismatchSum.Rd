\name{plotMismatchSum}
\alias{plotMismatchSum}
\title{Plot mismatch summary for short read}
\description{
  Showing only mismatch read counts as color coded stacked barchart or
  with coverage for background.
}
\usage{
  plotMismatchSum(obj, show.coverage = TRUE)
}
\arguments{
  \item{obj}{
    obj should be a \code{GRanges} object, usually returned by
    \code{pileupGRAngesAsVariantTable} function. Or is a \code{GRanges}
    object with arbitrary column: read, ref, count, depth, match.
  }
  \item{show.coverage}{
    A logical value indicate whether to show the coverage or
    not. Default is \code{TRUE}.
  }
}
\value{
  A \code{ggplot} object.
}
\details{
  Color for DNA bases are specified in the biovizBase package and
  retrieved in the options. Please see \code{getBioColor} for
  more information.
}
\examples{
\dontrun{
data(genesymbol)
library(BSgenome.Hsapiens.UCSC.hg19)    
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
gr <- GRanges("chr10", IRanges(6134000, 6135000))
test <- pileupAsGRanges(bamfile, region = gr)
test.match <- pileupGRangesAsVariantTable(test, Hsapiens)
## use plotMismatchSum directly
p <- plotMismatchSum(test.match, FALSE)
p <- plotMismatchSum(test.match, TRUE)
## use qplot generic function
p <- qplot(test.match, geom = "mismatch.summary")
p <- qplot(test.match, geom = "mismatch.summary", show.coverage = FALSE)

library(Rsamtools)

## for character
p <- qplot(bamfile, which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = TRUE)
p <- qplot(bamfile, which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = FALSE)

## for BamFile
p <- qplot(BamFile(bamfile), which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = TRUE)
p <- qplot(BamFile(bamfile), which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = FALSE)
}
}
\author{Tengfei Yin}

