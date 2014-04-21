\name{layout_circle}
\alias{circle}
\alias{layout_circle}
\alias{layout_circle,GRanges-method}
\alias{layout_circle,missing-method}
\alias{layout_circle,uneval-method}
\title{Create a circle layout}
\description{
  Create a circle layout.
}
\usage{
\S4method{layout_circle}{GRanges}(data, ..., geom = c("point", "line", "link", "ribbon",
                 "rect", "bar", "segment", "hist", "scale", "heatmap", "ideogram",
                 "text"), linked.to, radius = 10, trackWidth = 5,
                 space.skip = 0.015, direction = c("clockwise",
                 "anticlockwise"), link.fun = function(x, y, n = 30)
                 bezier(x, y, evaluation = n), rect.inter.n = 60, rank,
                 ylim = NULL,
                 scale.n = 60, scale.unit = NULL, scale.type = c("M",
                 "B", "sci"), grid.n = 5, grid.background = "gray70",
                 grid.line = "white", grid = FALSE, chr.weight = NULL)

\S4method{layout_circle}{missing}(data, ...)
circle(...)
}
\arguments{
  \item{data}{
    A \code{GRanges} object.
  }
  \item{...}{
    Extra parameters such as aesthetics mapping in aes(), or
    \code{color, size}, etc. For circle function, it passed to \code{layout_circle}.
  }
  \item{geom}{
    The geometric object to use display the data.
  }
  \item{linked.to}{
    Character indicates column that specifying end of the linking lines,
    that column should be a \code{GRanges} object.
  }
  \item{radius}{
    Numeric value indicates radius. Default is 10.
  }
  \item{trackWidth}{
    Numeric value indicates the track width.
  }
  \item{space.skip}{
    Numeric value indicates the ratio of skipped region between chunks(chromosomes
    in \code{GRanges}) to the whole track space.
  }
  \item{direction}{
    Space layout orders.
  }
  \item{link.fun}{
    Function used for interpolate the linking lines. Default is Hmisc::bezier.
  }
  \item{rect.inter.n}{
    n passed to interpolate function in rectangle transformation(from a
    rectangle) to a section in circular view.
  }
  \item{rank}{
    For default equal trackWidth, use rank to specify the circle orders.
  }
  \item{ylim}{
    Numeric range to control y limits.
  }
  \item{scale.n}{
    Approximate number of ticks you want to show on the whole
    space. used when scale.unit is \code{NULL}.
  }
  \item{scale.unit}{
    Unit used for computing scale. Default is \code{NULL},
  }
  \item{scale.type}{
    Scale type used for
  }
  \item{grid}{
    logical value indicate showing grid background for track or not.
  }
  \item{grid.n}{
    integer value indicate horizontal grid line number.
  }
  \item{grid.background}{
    grid background color.
  }
  \item{grid.line}{
    grid line color.
  }
  \item{chr.weight}{
    numeric vectors which sum to <1, the names of vectors has to be
  matched with seqnames in seqinfo, and you can only specify part of the
  seqnames, other lengths of chromosomes will be assined proportionally
  to their seqlengths, for example, you could specify chr1 to be 0.5, so
  the chr1 will take half of the space and other chromosomes squeezed to
  take left of the space.
  }

}
\value{
  A 'Layer'.
}
\examples{
N <- 100
library(GenomicRanges)
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


seqlengths(gr) <- c(400, 500, 700)
values(gr)$to.gr <- gr[sample(1:length(gr), size = length(gr))]

## doesn't pass gr to the ggplot
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
    layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6,
trackWidth = 1)

## more formal API
ggplot(gr) + layout_circle(geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(geom = "bar", radius = 10, trackWidth = 4, aes(fill = score, y = score)) +
  layout_circle(geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
    layout_circle(geom = "link", linked.to = "to.gr", radius = 6, trackWidth = 1)

}
\author{Tengfei Yin}

