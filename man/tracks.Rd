\name{tracks}
\alias{tracks}
\title{Tracks for genomic graphics}
\usage{tracks(..., show.axis.text.y = FALSE,
              show.ticks = FALSE,
              legend = FALSE, xlim, ylim)
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
\item{}{
  Tracks cannot guarantee all trakcs are aligned well in all the
  cases sinmly because control of grobs sometimes are tricky and based
  on what users passed. Like theme change, or uneven labels or legend
  could affact the alignemnts. Eventhough I tried hard to align them
  in most cases.
}
\item{}{
When y axis text length are not equal across tracks, it's hard
  to align. So you will see small wiggles. Even missing y label would
  affect the accurate alignmetns too.}
\item{}{So by default, we remove y axis text and legend to make sure
  they are aligned exactly at the same position. Clearly this is not a
  good practice.}
\item{}{
We will try our best to fix this restriction in the future release. So
advanced users could use gridExtra package to modify the
graphics before passing to \code{grid.arrange}.
}}}
\seealso{\code{\link{grid.arrange}}}
\value{return a frame grob; side-effect (plotting) if plot=T.}
\author{Tengfei Yin}
\arguments{\item{...}{Plots of class ggplot2, trellis, or grobs, and valid
    arguments to grid.layout.}
  \item{show.axis.text.y}{
    logical value indicate to show y axis text or not.
  }
  \item{show.ticks}{
    logical value indicate to show ticks or not.
  }
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

