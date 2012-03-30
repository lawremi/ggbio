\name{layout_circle}
\alias{layout_circle}
\alias{layout_circle,GRanges-method}
\title{Create a circle layout}
\description{
  Create a circle layout.
}
\usage{
\S4method{layout_circle}{GRanges}(data, ..., geom = c("point", "line", "link", "ribbon",
                 "rect", "bar", "segment", "hist", "scale", "ideogram",
                 "text"), linked.to, radius = 10, trackWidth = 5,
                 space.skip = 0.015, direction = c("clockwise",
                 "anticlockwise"),
                 link.fun = function(x, y, n = 30) bezier(x, y, evaluation = n),
                 rect.inter.n = 5, rank, scale.n = 60, scale.unit = NULL,
                 scale.type = c("M", "B", "sci"), grid.n = 5, grid.background = "gray70",
                 grid.line = "white", grid = FALSE)
}
\arguments{
  \item{data}{
    A \code{GRanges} object.
  }
  \item{...}{
    Extra parameters such as aesthetics mapping in aes(), or
    \code{color, size}, etc.
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
}
\value{
  A 'Layer'.
}
\examples{
\dontrun{
## make a sample data?
library(GenomicRanges)
data(hg19Ideogram, package = "biovizBase")
obj <- hg19Ideogram
obj <- keepSeqlevels(obj, paste("chr", c(1:22, "X", "Y"), sep = ""))

## define a set of data points
set.seed(1)
seqs <- seqlengths(obj)
N <- 20
lst <- lapply(1:length(seqs), function(i){
  seq <- seqs[i]
  pos <- runif(N, max = seq)
  score <- rnorm(N)
  GRanges(names(seq), IRanges(pos, width = 100))
})
res <- do.call(c, lst)
width(res) <- round(rnorm(N, mean = 1e7, sd = 10))
## seqlengths *MUST* be consistency
seqlengths(res) <- seqlengths(obj)
## adding some scores
values(res)$score <- rnorm(N)

## let's adding a link!
## simulate some data
lst <- lapply(1:length(seqs), function(i){
  seq <- seqs[i]
  pos <- runif(N, max = seq)
  score <- rnorm(N)
  GRanges(names(seq), IRanges(pos, width = 1))
})
to.gr <- do.call(c, lst)
## don't forget to add seqlengths for GRanges object
seqlengths(to.gr) <- seqlengths(obj)
width(to.gr) <- round(rnorm(N, mean = 1e3, sd = 10))
to.gr <- to.gr[sample(1:length(to.gr), size = length(to.gr))]
values(res)$to <- to.gr

## edit text label
values(obj)$label <- sub("chr", "", as.character(seqnames(obj)))
library(Hmisc)
library(ggbio)
ggplot() +
  layout_circle(res, geom = "link", rank = 0, linked.to = "to",
                         alpha = 0.4, aes(color = score) ) +
  layout_circle(res, geom = "ideogram", fill = "gray80", rank = 1) +
  layout_circle(obj, geom = "text", aes(label = label), rank = 2) +
  layout_circle(res, aes(y = score), geom = "point", rank = 3, size = 0.5) +
    layout_circle(res, aes(y = score), geom = "line", rank = 4) +
       layout_circle(res, geom = "rect", rank = 5) +
          layout_circle(res, geom = "segment", rank = 6) +
         layout_circle(res, geom = "scale", rank = 7)
}
}
\author{Tengfei Yin}

