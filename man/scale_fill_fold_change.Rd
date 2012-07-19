\name{scale_fill_fold_change}
\alias{scale_fill_fold_change}
\title{scale color for fold change values}
\description{
  In biology, lots of data are scaled to value around 0, and people like
  to show them as blue-white-red scale color, where negative value are
  blue, 0 is white and positive value is red, and they are scaled for
  continuous variables.
}
\usage{
scale_fill_fold_change()
}
\value{
  a list.
}
\examples{
p1 <- autoplot(volcano - 150)
p1
p1 + scale_fill_fold_change()
}
\author{Tengfei Yin}

