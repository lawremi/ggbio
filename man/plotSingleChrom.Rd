\name{plotSingleChrom}
\alias{plotSingleChrom}
\title{Plot single chromosome with cytoband}
\usage{plotSingleChrom(obj, subchr, zoom.region, xlabel = FALSE)
}
\description{Plot single chromosome with cytoband}
\details{User could provide the whole ideogram and use subchr to point to
particular chromosome.}
\value{A \code{ggplot} object.}
\author{Tengfei Yin}
\arguments{\item{obj}{A \code{GenomicRanges} object, which include extra
information about cytoband.}
\item{subchr}{A single character of chromosome names to show.}
\item{zoom.region}{A numeric vector of length 2 indicating zoomed
  region.}
\item{xlabel}{
  A logical value. Show the x label or not.
}
}
\examples{data(hg19IdeogramCyto)
vp1 <- viewport(width = 1, height = 0.14)
p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1")
print(p, vp = vp1)}

