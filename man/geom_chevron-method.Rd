\name{geom_chevron}
\alias{geom_chevron}
\alias{geom_chevron,GRanges-method}
\title{Chevron geoms for GRanges object}
\description{
  Break normal intervals stroed in \code{GRanges} object and show them
  as chevron, useful for showing model or splice summary.
}
\usage{
\S4method{geom_chevron}{GRanges}(data, ..., offset = 0.1,
             facets = NULL,
             stat = c("stepping", "identity"),
             chevron.height = c(0.1, 0.8))
}
\arguments{
  \item{data}{
    A GRanges object.
  }
  \item{...}{
    Extra parameters passed to autoplot function.
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
  To draw a normal GRanges as Chevron, we need to provide a special geom for
this purpose. Chevron is popular in gene viewer or genomoe browser,
when they try to show isoforms or gene model.\code{geom_chevron},
just like any other \code{geom_*} function in ggplot2, you can pass
aes() to it to use height of chevron or width
of chevron to show statistics summary.
}

\examples{
}
\author{Tengfei Yin}

