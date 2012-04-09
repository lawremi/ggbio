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
  \item{y}{
    unevaluated name which should be one the of the column names
    indicating the p-value Or other score used as y value in the plot.
    This is required field.
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
seqlths <- round(seqlths/1000)
set.seed(1)
nchr <- length(chrs)
nsnps <- 100
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
plotGrandLinear(gr.snp, y = pvalue)
plotGrandLinear(gr.snp, y = pvalue, space.skip = 0.1)

## sort chromosomes
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
plotGrandLinear(gr.snp, y = pvalue)

## tweak with seqlengths
nms <- names(seqlengths(gr.snp))
seqs <- rep(3e5, length(nms))
names(seqs) <- nms
seqlengths(gr.snp) <- seqs
plotGrandLinear(gr.snp, y = pvalue)

## facet by samples, comparison across groups
plotGrandLinear(gr.snp, y = pvalue, 
                facets = group ~ .,  color.type = "twocolor")
## change two color
plotGrandLinear(gr.snp, y = pvalue, 
                facet = group ~ .,  color.type = "twocolor",
                two.color = c("red", "blue"))



## geom line
plotGrandLinear(gr.snp, y = pvalue,
                geom = "line", facet = group ~ .)  


## add size and change color
plotGrandLinear(gr.snp, y = pvalue, size = 5,
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, aes(size = pvalue),
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, color = group,
                facet = group ~ .,
                color.type = "identity")

plotGrandLinear(gr.snp, y = pvalue, color = "blue", facet = group ~ .,
                color.type = "identity")

}
}
\author{Tengfei Yin}
