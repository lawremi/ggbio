\name{arrangeGrobByParsingLegend}
\alias{arrangeGrobByParsingLegend}
\title{Arrange grobs by parse their legend.}
\description{
Arrange grobs and parse their legend, then put it together on the
right.
}
\usage{
arrangeGrobByParsingLegend(..., nrow = NULL, ncol = NULL,
                           widths = c(4, 1), legend.idx = NULL)
}
\arguments{
  \item{...}{ggplot graphics.}
  \item{nrow}{number of row for layout.}
  \item{ncol}{number of columns for layout}
  \item{widths}{width ratio for plot group and legend group.}
  \item{legend.idx}{legend index you want to keep.}  
}
\value{
  a 
}
\examples{
library(ggplot2)
p1 <- qplot(x = mpg, y= cyl, data = mtcars, color = carb)
p2 <- qplot(x = mpg, y= cyl, data = mtcars, color = wt)
p3 <- qplot(x = mpg, y= cyl, data = mtcars, color = qsec)
p4 <- qplot(x = mpg, y= cyl, data = mtcars, color = gear)
arrangeGrobByParsingLegend(p1, p2, p3, p4)
arrangeGrobByParsingLegend(p1, p2, p3, p4, ncol = 1)
arrangeGrobByParsingLegend(p1, p2, p3, p4, legend.idx = 2)
}
\author{Tengfei Yin}

