\name{plotRangesLinkedToData}
\alias{plotRangesLinkedToData}
\title{Plot Ranges Linked with Data}
\description{
  Plot GRanges object structure and linked to a even spaced paralell
  coordinates plot which represting the data in elementeMetadata.
}
\usage{
plotRangesLinkedToData(data, ..., stat.col, stat.label,
                                   stat.ylab,
                                   sig, sig.col = c("black", "red"),
                                   stat.coord.trans = coord_trans(),
                                   annotation = list(),
                                   width.ratio = 0.8,
                                   track.skip = -1,
                                   theme.stat = theme_grey(),
                                   theme.align = theme_gray(),
                                   linetype = 3, 
                                   heights)

}
\arguments{
  \item{data}{
    GRanges object with a DataFrame as elementMetadata.
  }
  \item{...}{
    Parameters passed to control lines in top plot.
  }
  \item{stat.col}{
    integer (variable position starting in DataFrame of data, start from
    1) or strings (variable names) which indicate the column names.
  }
  \item{stat.label}{
    Labels of the columns, if missing, use stat.col.
  }
  \item{stat.ylab}{
    y label for stat track(the top track).
  }
  \item{sig}{
    a character of element meta data column of logical value, indicates
    which row is signficant. and will be shown in link lines and rectangle.
  }
  \item{sig.col}{
    colors for significant, valid when you specify "sig" argument, the
    first color indicates \code{FALSE}, non-significant, the second
    color indicates \code{TRUE}.
  }
  \item{stat.coord.trans}{
    transformation used for top plot.
  }
  \item{annotation}{
    A list of ggplot object.
  }
  \item{width.ratio}{
    Control the segment length of statistic layer.
  }
  \item{track.skip}{
    skip between tracks.
  }
  \item{theme.stat}{
    top plot theme.
  }
  \item{theme.align}{
    alignment themes.
  }
  \item{linetype}{
    linetype
  }
  \item{heights}{
    Heights of each track.
  }
}
\value{
  return a frame grob; side-effect (plotting) if plot=T.
}
\details{
Inspired by some graphics produced in some other packages, for example
in package DEXseq, the author provides graphics with gene
models and linked to an even spaced statistics summary. This is useful
because we always plot everything along the genomic coordinates, but
genomic features like exons are not evenly distributed, so we could
actually treat the statistics associated with exons like categorical
data, and show them as "Paralell Coordinates Plots". This is one
special layout which represent the data in a nice manner and also keep
the genomic structure information. With abliity of \code{tracks},
it's possible to generate such type of a graphic along with other
annotations.

The data we want is a normal \code{GRanges} object, and make sure
the intervals are not overlaped with each other(currently), and you
may have multiple columns which store the statistics for multiple
samples, then we produce the graphic we introduced above and users
could pass other annotation track in the function which will be shown
below the main linked track. 

The reason you need to pass annotation into the function instead of
binding them by \code{tracks} later is because binding manually
with annotation tracks is tricky and this function doesn't return a
ggplot object. 
}
\examples{
\dontrun{
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(ggbio)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
model <- exonsBy(txdb, by = "tx")
model17 <- subsetByOverlaps(model, genesymbol["RBM17"])
exons <- exons(txdb)
exon17 <- subsetByOverlaps(exons, genesymbol["RBM17"])
## reduce to make sure there is no overlap
## just for example
exon.new <- reduce(exon17)
## suppose
values(exon.new)$sample1 <- rnorm(length(exon.new), 10, 3)
values(exon.new)$sample2 <- rnorm(length(exon.new), 10, 10)
values(exon.new)$score <- rnorm(length(exon.new))
values(exon.new)$significant <- sample(c(T,F), size =
length(exon.new),replace = TRUE)
plotRangesLinkedToData(exon.new, stat.col = c("sample1", "sample2"))
plotRangesLinkedToData(exon.new, stat.col = 1:2)
plotRangesLinkedToData(exon.new, stat.col = 1:2, size = 3, linetype = 4)
plotRangesLinkedToData(exon.new, stat.col = 1:2, size = 3, linetype = 4,
                       sig = "significant")
plotRangesLinkedToData(exon.new, stat.col = 1:2, size = 3, linetype = 4,
                       sig = "significant", sig.col = c("red",
"gray90"))
}
}
\author{Tengfei Yin}

