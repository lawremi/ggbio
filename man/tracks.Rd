\name{tracks}
\alias{tracks}
\title{Tracks for genomic graphics}
\usage{tracks(..., check.xlim=TRUE, legend=FALSE, xlim, ylim)
}
\description{In most genome browsers, they all have such a view that including
many tracks, could be any anntation data along genomic
coordinate. So we try to provide a convenient constructor for
building tracks, which here in this package is simply vertically
binding of several plots. It's essentially a
\code{grid.arrange}. So if users want to have more delicate
control over their tracks, they need manipulate the graphics in
ggplot2 level or grid levels.}
\details{\code{tracks} function has some extra features and limitations
compare to \code{grid.arrange}.
\itemize{
\item{}{
Always sitting on genomic or protein space.
}
\item{}{
Always using ncol = 1 as default arguments.
}
\item{}{
For now, since the unbalanced legend and labels in ggplot2 has
been solved (maybe just I haven't found such features). We simply
remove legend and y axis labels to make sure all tracks are aligned
exactly in the same way.
}
\item{}{
Remove the x-axis for most track except the last one.
} 
\item{}{
Does the ajustment of margins for you automatically.
}
\item{}{
Doesn't like \code{qplot}, tracks doesn't return \code{ggplot}
object. so processing your plot before you pass them to
\code{tracks}.
}
}}
\seealso{\code{\link{grid.arrange}}}
\value{return a frame grob; side-effect (plotting) if plot=T.}
\author{Tengfei Yin}
\arguments{\item{...}{Plots of class ggplot2, trellis, or grobs, and valid
arguments to grid.layout.}
\item{check.xlim}{Default is TRUE, make sure all tracks are on the
same scale. if FALSE, just like common grid.arrange function.}
\item{legend}{Default is FALSE, remove legend, this make sure all
tracks are aligned exactly based on the same position.}
\item{xlim}{Limits on x.}
\item{ylim}{Limits on y.}
}
\examples{
\dontrun{
library(BSgenome.Hsapiens.UCSC.hg19)
gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
p1 <- qplot(Hsapiens, name = gr, geom = "text")
p2 <- qplot(Hsapiens, name = gr, geom = "point")
p3 <- qplot(Hsapiens, name = gr, geom = "segment")
p4 <- qplot(Hsapiens, name = gr, geom = "rectangle")
tracks(p1, p2, p3, p4)
}
}

