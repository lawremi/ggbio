\name{geom_hotregion}
\alias{geom_hotregion}
\title{Adding hotregion for stacked overview (genome-wide)}
\usage{geom_hotregion(data, ...)
}
\description{Adding hotregion which is a \code{GRanges} object for stacked
overview (genome-wide)}
\details{The overplayed region may contain single position which is not
interval, this will be plotted as segments instead of rectangle.}
\value{A 'Layer'}
\author{Tengfei Yin}
\arguments{\item{data}{A \code{\link{GRanges}} object, which you want to
overlay on the stacked overview.}
\item{...}{Extra parameters passed to geom in
ggplot2. e.g. aes(color = score)}
}
\examples{
data(hg19IdeogramCyto)
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
p <- plotOverview(new.ideo, cytoband = FALSE)
data(darned_hg19_subset500)
## rename 
new.darned <- renameSeqlevels(darned_hg19_subset500, new.chrs)
p <- p + geom_hotregion(new.darned)
print(p)
}

