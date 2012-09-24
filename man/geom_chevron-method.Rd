\name{geom_chevron}
\alias{geom_chevron}
\alias{geom_chevron,GRanges-method}
\alias{geom_chevron,missing-method}
\alias{geom_chevron,uneval-method}
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
set.seed(1)
N <- 100
require(GenomicRanges)

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



## ======================================================================
##  default
##
##  ======================================================================
ggplot(gr) + geom_chevron()
## or
ggplot() + geom_chevron(gr)


## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot(gr) + geom_chevron(facets = sample ~ seqnames, aes(color = strand))


## ======================================================================
##  stat:identity
## ======================================================================
ggplot(gr) + geom_chevron(stat = "identity", aes(y = value))


## ======================================================================
##  stat:stepping
## ======================================================================
ggplot(gr) + geom_chevron(stat = "stepping", aes(group = pair))


## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot(gr) + geom_chevron(stat = "stepping", aes(group = pair), group.selfish = FALSE,
                        xlab = "xlab", ylab = "ylab", main = "main")

p <- qplot(x = mpg, y = cyl, data = mtcars)

## ======================================================================
##  offset
## ======================================================================
gr2 <- GRanges("chr1", IRanges(c(1, 10, 20), width = 5))
gr2.p <- gaps(gr2)
## resize to connect them
gr2.p <- resize(gr2.p, fix = "center", width = width(gr2.p)+2)

ggplot(gr2) + geom_rect() + geom_chevron(gr2.p)


## notice the rectangle height is 0.8
## offset = 0 just like a line
ggplot(gr2) + geom_rect() + geom_chevron(gr2.p, offset = 0)


## equal height
ggplot(gr2) + geom_rect() + geom_chevron(gr2.p, offset = 0.4)


## ======================================================================
##  chevron.height
## ======================================================================
values(gr2.p)$score <- c(100, 200)
ggplot(gr2) + geom_rect() + geom_chevron(gr2.p, offset = "score")
## chevron.height
ggplot(gr2) + geom_rect() + geom_chevron(gr2.p, offset = "score",
                                         chevron.height.rescale = c(0.4, 10))
 
}
\author{Tengfei Yin}

