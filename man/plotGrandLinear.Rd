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
plotGrandLinear(obj, ..., facets, space.skip = 0.01, geom = NULL,
                 cutoff = NULL, cutoff.color = "red", cutoff.size = 1,
                 legend = FALSE, xlim, ylim,
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
## @knitr load
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
library(GenomicRanges)

## @knitr simul_gr
library(biovizBase)
gr <- GRanges(rep(c("chr1", "chr2"), each = 5),
              IRanges(start = rep(seq(1, 100, length = 5), times = 2),
                      width = 50))
autoplot(gr)

## @knitr coord:genome
autoplot(gr, coord = "genome")
gr.t <- transformToGenome(gr)
head(gr.t)

## @knitr is
is_coord_genome(gr.t)
metadata(gr.t)$coord


## @knitr simul_snp
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
seqlths <- seqlengths(hg19Ideogram)[chrs]
set.seed(1)
nchr <- length(chrs)
nsnps <- 100
gr.snp <- GRanges(rep(chrs,each=nsnps),
                  IRanges(start =
                          do.call(c, lapply(chrs, function(chr){
                            N <- seqlths[chr]
                            runif(nsnps,1,N)
                          })), width = 1),
                  SNP=sapply(1:(nchr*nsnps), function(x) paste("rs",x,sep='')),
                  pvalue =  -log10(runif(nchr*nsnps)),
                  group = sample(c("Normal", "Tumor"), size = nchr*nsnps,
                    replace = TRUE)
                  )

## @knitr shorter
seqlengths(gr.snp)
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)



## @knitr unorder
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr sort
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr with_seql
names(seqlths) <- gsub("chr", "", names(seqlths))
seqlengths(gr.snp) <- seqlths[names(seqlengths(gr.snp))]
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr line
autoplot(gr.snp, coord = "genome", geom = "line", aes(y = pvalue, group = seqnames,
                                     color = seqnames))



## @knitr plotGrandLinear
plotGrandLinear(gr.snp, aes(y = pvalue))

## @knitr morecolor
plotGrandLinear(gr.snp, aes(y = pvalue, color = seqnames))
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue"))
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue", "red"))
plotGrandLinear(gr.snp, aes(y = pvalue), color = "red")

## @knitr cutoff
plotGrandLinear(gr.snp, aes(y = pvalue), cutoff = 3, cutoff.color = "blue", cutoff.size = 4)

## @knitr cutoff-low
plotGrandLinear(gr.snp, aes(y = pvalue)) + geom_hline(yintercept = 3, color = "blue", size = 4)

## @knitr longer
## let's make a long name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- paste("chr00000", nms, sep = "")
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)

## @knitr rotate
plotGrandLinear(gr.snp, aes(y = pvalue)) + opts(axis.text.x=theme_text(angle=-90, hjust=0))

## @knitr sessionInfo
sessionInfo()
}
\author{Tengfei Yin}
