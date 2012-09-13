\name{geom_chevron}
\alias{geom_chevron}
\alias{geom_chevron,GRanges-method}
\title{Chevron geoms for GRanges object}
\description{
  Break normal intervals stroed in \code{GRanges} object and show them
  as chevron, useful for showing model or splice summary.
}
\usage{
\S4method{geom_chevron}{GRanges}(data, ..., xlab, ylab, main,
             offset = 0.1,
             facets = NULL,
             stat = c("stepping", "identity"),
             chevron.height.rescale = c(0.1, 0.8),
             group.selfish = TRUE)
}
\arguments{
  \item{data}{
    A GRanges object.
  }
  \item{...}{
    Extra parameters passed to autoplot function.
  }
  \item{xlab}{
    Label for x
  }
  \item{ylab}{
    Label for y
  }
  \item{main}{
    Title for plot.
  }
  \item{offset}{
    A nunmeric value or characters. If it's numeric value, indicate how
    much you want the chevron to wiggle, usually the rectangle for
    drawing \code{GRanges} is of height unit 1, so it's better between
    -0.5 and 0.5 to make it nice looking. Unless you specify offset as
    one of those columns, this will use height of the chevron to
    indicate the columns. Of course you could use size of the chevron to
    indicate the column variable easily, please see the examples.
  }
  \item{facets}{
    faceting formula to use.
  }
  \item{stat}{
    character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.
  }
  \item{chevron.height.rescale}{
    A numeric vector of length 2. When the offset parameters is a
    character which is one of the data columns, this parameter rescale
    the offset.
  }
  \item{group.selfish}{
    Passed to \code{addStepping}, control whether to show each group as
  unique level or not. If set to \code{FALSE}, if two groups are not
  overlapped with each other, they will probably be layout in the same
  level to save space.
  }  
}
\value{
  A 'Layer'.
}
\details{
  To draw a normal GRanges as Chevron, we need to provide a special geom for
this purpose. Chevron is popular in gene viewer or genomoe browser,
when they try to show isoforms or gene model.\code{geom_chevron},
just like any other \code{geom_*} function in ggplot2, you can pass
aes() to it to use height of chevron or width
of chevron to show statistics summary.
}

\examples{
## @knitr load
set.seed(1)
N <- 100
require(ggbio)
require(GenomicRanges)
## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))


## @knitr default
## ======================================================================
##  default
## ======================================================================
ggplot() + geom_chevron(gr)

## @knitr facet_aes
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_chevron(gr, facets = sample ~ seqnames, aes(color = strand))

## @knitr stat:identity
## ======================================================================
##  stat:identity
## ======================================================================
ggplot() + geom_chevron(gr, stat = "identity", aes(y = value))

## @knitr stat:stepping
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_chevron(gr, stat = "stepping", aes(group = pair))

## @knitr group.selfish
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_chevron(gr, stat = "stepping", aes(group = pair), group.selfish = FALSE,
                        xlab = "xlab", ylab = "ylab", main = "main")

p = qplot(x = mpg, y = cyl, data = mtcars)
p + labs(title = "asdf")
## @knitr offset
## ======================================================================
##  offset
## ======================================================================
gr2 <- GRanges("chr1", IRanges(c(1, 10, 20), width = 5))
gr2.p <- gaps(gr2)
## resize to connect them
gr2.p <- resize(gr2.p, fix = "center", width = width(gr2.p)+2)
## @knitr offset:default
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p)

## @knitr offset:0
## notice the rectangle height is 0.8
## offset = 0 just like a line
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = 0)

## @knitr offset:0.4
## equal height
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = 0.4)

## @knitr chevron.height:default
## ======================================================================
##  chevron.height
## ======================================================================
values(gr2.p)$score <- c(100, 200)
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = "score")
## chevron.height
ggplot() + geom_rect(gr2) + geom_chevron(gr2.p, offset = "score",
                                         chevron.height.rescale = c(0.4, 10))
 
}
\author{Tengfei Yin}

