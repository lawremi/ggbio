\name{geom_chevron}
\alias{geom_chevron}
\title{Chevron Geoms for GRanges object}
\description{
  Break normal intervals stroed in \code{GRanges} object and show them
  as chevron geom, useful for showing model or splice summary.
}
\usage{
geom_chevron(data, ..., group.name, offset = 0.1,
             chevron.height = c(0.1, 0.8))
}
\arguments{
  \item{data}{
    A GRanges object.
  }
  \item{...}{
    Extra parameters passed to qplot function.
  }
  \item{group.name}{
    group.name passed to \code{addSteppings}, in case your
    \code{GRanges} object doesn't contain ".levels" column. You can
    do it here.
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
  \item{chevron.height}{
    A numeric vector of length 2. When the offset parameters is a
    character which is one of the data columns, this parameter could
    specify the wiggled range of the chevron(or the height range).
  }
}
\value{
  A 'Layer'.
}
\details{
  To draw a normal GRanges as Chevron instead of segments or rectangle
as shown in previous sections, we need to provide a special geom for
this purpose. Chevron is popular in gene viewer or genomoe browser,
when they try to show isoforms or gene model.\code{geom_chevron},
just like any other \code{geom_*} function in ggplot2, you can pass
aes() to it to use height of chevron or width
of chevron to show statistics summary.
}

\examples{
\dontrun{
library(GenomicRanges)
gr <- GRanges("chr1", IRanges(c(100, 200, 300), width = 50))
p <- qplot(gr)
gr.gaps <- gaps(gr)[-1]
values(gr.gaps)$score <- c(1, 100)
p + geom_chevron(gr.gaps)
p + geom_chevron(gr.gaps, aes(size = score), offset = "score")
p + geom_chevron(gr.gaps, aes(size = score), offset = "score",
                 chevron.height = c(0.1, 0.2))
p + geom_chevron(gr.gaps, offset = -0.1)
}
}
\author{Tengfei Yin}

