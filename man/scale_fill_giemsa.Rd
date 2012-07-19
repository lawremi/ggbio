\name{scale_fill_giemsa}
\alias{scale_fill_giemsa}
\title{scale filled color to customized giemsa color.}
\description{
  scale filled color to customized giemsa color.
}
\usage{
scale_fill_giemsa(fill = getOption("biovizBase")$cytobandColor)
}
\arguments{
  \item{fill}{
    a character vector to indicate colors, and names of vector mapped to
    gieStain name.
  }
}
\value{
  a list.
}
\examples{
getOption("biovizBase")$cytobandColor
library(biovizBase)
data(hg19IdeogramCyto)
p1 <- autoplot(hg19IdeogramCyto, layout = "karyogram", aes(fill  =
gieStain))
p1
p1 + scale_fill_giemsa()
}
\author{Tengfei Yin}

