\name{fortify}
\alias{fortify}
\alias{fortify,eSet,ANY-method}
\alias{fortify,GRanges,ANY-method}
\title{Fortify a object to a data frame}
\description{
  Fortify a object to a data frame.
}
\usage{
\S4method{fortify}{eSet,ANY}(model = NULL, data)
\S4method{fortify}{GRanges,ANY}(model = NULL, data)
}
\arguments{
  \item{model}{
    model or other R object to convert to data frame 
  }
  \item{data}{
    Origainal data if needed.
  }
}
\value{
  a data.frame object.
}
\author{Tengfei Yin}

