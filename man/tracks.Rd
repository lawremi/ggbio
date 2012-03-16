\name{tracks}
\alias{tracks}
\title{Tracks for genomic graphics}
\usage{
tracks(..., heights, xlim, ylim, xlab = NULL,                
            theme = NULL, legend = FALSE,     
            track.skip = -1,
            xlim.change = rep(TRUE, length(list(...))))
}
\arguments{
  \item{...}{plots of class ggplot2, trellis, or grobs, and valid
    arguments to grid.layout.}
  \item{legend}{default is FALSE, remove legend, this make sure all
    tracks are aligned exactly based on the same position.}
  \item{xlim}{limits on x.}
  \item{ylim}{limits on y.}
  \item{xlab}{label for x axis.}
  \item{heights}{numeric vector of the same length of passed graphic
    object to indicate the ratio of each track.}
  \item{theme}{Option list or theme applied to each track.}
  \item{track.skip}{Numeric value, skip between tracks, unit is 'lines'.}
  \item{xlim.change}{Vector of logical value of the same length as
    passed graphic objects, to control whether we adjust that track with
  each other or just leave it as it is. This could be useful when you
  pass a single chromosome view on top of the tracks.}
}
\description{
  In most genome browsers, they all have such a view that including
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


