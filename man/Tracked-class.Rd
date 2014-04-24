\name{Tracked}
\alias{Tracked}
\alias{Tracked-class}
\title{Tracked class}
\description{
  Create a tracked object, designed for tracks function.
}
\usage{
Tracked(mutable = TRUE, fixed = FALSE, labeled = TRUE,
        hasAxis = FALSE, bgColor = "white", height = unit(1, "null"))
}
\arguments{
  \item{mutable}{
    logical value, default \code{TRUE}. To control whether a track is
    updatable by applying \code{+} on it.
  }
  \item{fixed}{
    logical value, default \code{FALSE}. To control whether the scale
    response to a xlim change or not.
  }
  \item{labeled}{
    logical value, default \code{TRUE}. To control whether to label it
    all not.
  }
  \item{hasAxis}{
    logical value, default \code{FALSE}. To control whether to show axis
    for that track or not.
  }
  \item{bgColor}{
    character to control background color of a track.
  }
  \item{height}{
    unit, to control track height.
  }
}
\value{
  a \code{Tracked} object.
}
\author{Tengfei Yin}

