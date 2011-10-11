\name{plotOverview}
\alias{plotOverview}
\title{Plot stacked overview for genome}
\usage{plotOverview(obj, cytoband=FALSE)
}
\description{Plot stacked overview for genome with or without cytoband.}
\details{This function requires two column of the \code{gieStain} and
\code{name}. Which you could get from \code{getIdeogram} function
in pacakge \code{biovizBase}.}
\value{A \code{ggplot} object.}
\author{Tengfei Yin}
\arguments{\item{obj}{A \code{GenomicRanges} object, which include extra
information about cytoband.}
\item{cytoband}{Logical value. Default is FALSE. If TRUE, plotting
cytoband.}
}
\examples{
library(GenomicRanges)
data(hg19IdeogramCyto)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
## with cytoband
p <- plotOverview(new.ideo, cytoband = TRUE)
print(p)
## with nocytoband
p <- plotOverview(new.ideo, cytoband = FALSE)
print(p)
}

