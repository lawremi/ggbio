\name{fortify}
\alias{fortify}
\alias{fortify,eSet,missing-method}
\alias{fortify,GRanges,missing-method}
\title{Fortify a object to a data frame}
\description{
  Fortify a object to a data frame.
}
\usage{
\S4method{fortify}{eSet,missing}(model, data)
\S4method{fortify}{GRanges,missing}(model, data)
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

