\name{plotStackedOverview}
\alias{plotStackedOverview}
\title{Plot stacked overview for genome}
\usage{
plotStackedOverview(obj, xlab, ylab, main, facets = seqnames ~ ., cytoband
                 = FALSE)

}
\description{Plot stacked overview for genome with or without cytoband.}
\details{This function requires two column of the \code{gieStain} and
\code{name}. Which you could get from \code{getIdeogram} function
in pacakge \code{biovizBase}.}
\value{A \code{ggplot} object.}
\author{Tengfei Yin}
\arguments{
  \item{obj}{A \code{GenomicRanges} object, which include extra
    information about cytoband.}
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
  \item{cytoband}{Logical value. Default is FALSE. If TRUE, plotting
cytoband.}
}
\examples{
\dontrun{
library(GenomicRanges)
data(hg19IdeogramCyto, package = "biovizBase")
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
## with cytoband
p <- plotStackedOverview(new.ideo, cytoband = TRUE)
print(p)
## with nocytoband
p <- plotStackedOverview(new.ideo, cytoband = FALSE)
print(p)
}
}

