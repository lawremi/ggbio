\name{plotGrandLinear}
\alias{plotGrandLinear}
\title{Manhattan for GWAS}
\description{
A Manhattan plot is special scatter plot used to visualize data with a
large number of data points, with a distribute of some higher-magnitude
values. For example, in the GWAS(genome-wide association studies). Here
we mainly focus on GWAS Manhattan plots. X-axis is genomic coordinates
and Y-axis is negative logarithm of the associated P-value for each
single nucleotide polymorphism. So higher the value, more stronger the
association they are.
}
\usage{
plotGrandLinear(obj, y, ..., facets, space.skip = 0.01, geom = NULL,
                            color.type = c("twocolor", "identity", "seqnames"),
                            two.color = c("#0080FF", "#4CC4FF"),
                            cutoff = NULL, cutoff.color = "red",
                            cutoff.size = 1, legend = FALSE, xlim, ylim, 
                            xlab = "Genomic Coordinates",
                            ylab = substitute(y), main, theme)

}
\arguments{
  \item{obj}{
    \code{GRanges} object which contains extra p value, before users
    pass this object, they need to make sure the pvalue has been changed
    to -log10(p).
  }
  \item{...}{
    extra arguments passed. such as color, size, alpha.
  }
  \item{facets}{
    facets formula, such as group ~ .
  }
  \item{space.skip}{
    numeric value for skip ratio, between chromosome spaces.default is 0.01.
  }
  \item{geom}{
    geometric object, defualt is "point".
  }
  \item{cutoff}{
    A numeric vector which used as cutoff for Manhattan plot.
  }
  \item{cutoff.color}{
    A character specifying the color used for cutoff. Default is "red".
  }
  \item{cutoff.size}{
    A numeric value which used as cutoff line size.
  }
  \item{legend}{
    A logical value indicate whether to show legend or not. Default is
    FALSE which disabled the legend.
  }
  \item{xlim}{
    limits for x scale.
  }
  \item{ylim}{
    limits for y scale.
  }
  \item{xlab}{
    Label for xscale.
  }
  \item{ylab}{
    Label for yscale.
  }
  \item{main}{
    title.
  }
  \item{theme}{
    theme options.
  }
}
\value{
  Return a ggplot object. 
}
\details{
  Please use seqlengths of the object and space.skip arguments to
  control the layout of the coordiant genome transformation.

  aes(y = ...) is requried.

  aes(color = ) is used to mapping to data variables, if just pass
  "color" without aes(), then will recycle the color to represent each
  chromosomes.please see the example below.
  
}
\examples{
\dontrun{

}
}
\author{Tengfei Yin}
