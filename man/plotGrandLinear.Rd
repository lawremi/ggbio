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
plotGrandLinear(obj, y, facets, size, shape, color, alpha, ..., geom =
                 c("point", "line"), color.type = c("twocolor",
                 "identity", "seqnames"), two.color = c("#0080FF",
                 "#4CC4FF"), cutoff = NULL, cutoff.color = "red",
                 cutoff.size = 1, legend = FALSE, xlim, ylim, xlab =
                 "Genomic Coordinates", ylab = substitute(y), main,
                 theme)

}
\arguments{
  \item{obj}{
    \code{GRanges} object which contains extra p value, before users
    pass this object, they need to make sure the pvalue has been changed
    to -log10(p).
  }
  \item{y}{
    Unevaluated name which should be one the of the column names
    indicating the p-value Or other score used as y value in the plot.
    This is required field.
  }
  \item{size}{
    Size for point or lines. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{shape}{
    Shape for point or lines. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{color}{
    Color for point for lines. Could equal a column name or a fixed
    character. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{alpha}{
    Alpha blending. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{...}{
    Extra arguments passed. Will be automatically dispatched to the
    right place, such as scales, space. When you try to use a faceted
    seqnames, you need to specify \code{scales = "free"} and \code{space
      = "free"}, if you want a free scaled x-scale.
  }
  \item{geom}{
    "point"(default) or "line". When it's line, it will make sure the
    line is not connect cross different chromosomes.
  }
  
  \item{color.type}{
    "identity" use single color for all points, when it's enabled, you
    can specify color in the arguments to equal a character or an
    unevaluated name, when use specific color, try use \code{I}, for
    instance, color = I("red"); "seqnames" use default
    discrete color scheme for all chromosomes; "twocolor" use two colors
    to represent all the chromosomes, could specify color in the
    two.color argument.
  }
  \item{two.color}{
    A character vector of two. Default is chosen from dichromat palette
    "BluetoOrange.8", make sure it's color-blind safe.
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
  \item{xlab}{
    Label for xscale.
  }
  \item{ylab}{
    Label for yscale.
  }
  \item{facets}{
  }
  \item{xlim}{
  }
  \item{ylim}{
  }
  \item{main}{
  }
  \item{theme}{
  }
}
\value{
  Return a ggplot object. 
}
\details{
If scales and space are free, then the mapping between position 
and values in the data will be the same across all panels   
}
\examples{
\dontrun{
library(GenomicRanges)
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
seqlths <- seqlengths(hg19Ideogram)[chrs]
set.seed(1)
nchr <- length(chrs)
nsnps <- 1000
gr.snp <- GRanges(rep(chrs,each=nsnps),
                  IRanges(start = do.call(c, lapply(chrs, function(chr){
                    N <- seqlths[chr]
                    runif(nsnps,1,N)
                  })), width = 1),
                  SNP=sapply(1:(nchr*nsnps), function(x) paste("rs",x,sep='')),
                  pvalue =  -log10(runif(nchr*nsnps)),
                  group = sample(c("Normal", "Tumor"), size = nchr*nsnps,
                    replace = TRUE)
                  )
## processing the name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)


## compact view
## no facet by samples, but make sure you want it that way
## default is two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point")
## facet by samples, comparison across groups
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facet = group ~ .,  color.type = "twocolor")
## change two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facet = group ~ .,  color.type = "twocolor",
                two.color = c("red", "blue"))
## geom line
plotGrandLinear(gr.snp, y = pvalue,
                geom = "line", facet = group ~ .)  


## add size and change color
plotGrandLinear(gr.snp, y = pvalue, size = pvalue,geom = "point",
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, size = I(0.05),
                geom = "point", facet = group ~ .)

plotGrandLinear(gr.snp, y = pvalue, color = group, geom = "point",
                facet = group ~ .,
                color.type = "identity")

plotGrandLinear(gr.snp, y = pvalue, color = I("blue"), geom = "point", facet = group ~ .,
                color.type = "identity")

## facet by seqnames, slower
plotGrandLinear(gr.snp, y = pvalue,geom = "point", 
                facet = group ~ seqnames, scales = "free", space = "free")

}
}
\author{Tengfei Yin}
