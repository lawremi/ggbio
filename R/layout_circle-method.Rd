\name{layout_circle}
\alias{layout_circle}
\alias{layout_circle,GRanges-method}
\title{Create a circle layout}
\description{
  Create a circle layout.
}
\usage{
\S4method{layout_circle}{GRanges}(data, ..., geom = c("point", "line", "link", "ribbon","rect", "bar",
                                       "segment", "rectangle", "hist", "scale", 
                                "ideogram", "text"), linked.to,
                          radius = 10, trackWidth = 5, 
                          space.skip = 0.015, direction = c("clockwise", "anticlockwise"),
                          link.fun = function(x, y, n = 30) bezier(x, y, evaluation = n),
                   rect.inter.n = 5, rank, 
                   scale.n = 60, scale.unit = NULL, scale.type = c("M", "B", "sci"))
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
  }
  \item{...}{
    Extra parameters such as aes() passed to \code{geom_rect},
    \code{geom_alignment}, or \code{geom_segment}.
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
  \items{space.skip}{
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
}
\value{
  A 'Layer'.
}
\examples{
}
\author{Tengfei Yin}

