\name{plotManhattan}
\alias{plotManhattan}
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
plotManhattan(obj, y, title,
              scales = c("free", "fixed"),
              space = c("free", "fixed"),
              color.type = c("identity", "seqnames", "twocolor"),
              two.color = c("#0080FF", "#4CC4FF"),
              cutoff = NULL,
              cutoff.color = "red",
              cutoff.size = 1,
              legend = FALSE,
              xlab = "Chromosome",
              ylab = expression(-log[10](italic(p))),
              theme_bw = TRUE)
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
  \item{title}{
    Title for the plot, default plot without title.
  }
  \item{scales}{
    How to control scale globally. Default is "free", other option
    including "fixed", Please see details.
  }
  \item{space}{
    Control space. Default is "free", other option including "fixed",
    Please see details.
  }
  \item{color.type}{
    "identity" use single color for all points; "seqnames" use default
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
  \item{theme_bw}{
    A logical value indicate whether to show gray background or
    not. Default is TRUE, use theme_bw() in ggplot2.
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
data(hg19IdeogramCyto)
data(hg19Ideogram)
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
             pvalue =  -log10(runif(nchr*nsnps))
             )

## processing the name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)


plotManhattan(gr.snp, y = pvalue)
plotManhattan(gr.snp, y = pvalue, scale = "fixed")
plotManhattan(gr.snp, y = pvalue, scale = "fixed", space = "fixed")
plotManhattan(gr.snp, y = pvalue, space = "fixed")

plotManhattan(gr.snp, y = pvalue, color.type = "seqnames")
plotManhattan(gr.snp, y = pvalue, color.type = "twocolor")
plotManhattan(gr.snp, y = pvalue, color.type = "twocolor", title = "SNP")

plotManhattan(gr.snp, y = pvalue, color.type = "twocolor", title = "SNP",
              cutoff = c(4, 5, 5.2), cutoff.color = "blue", cutoff.size = 1,
              legend = TRUE)

plotManhattan(gr.snp, y = pvalue, color.type = "twocolor", 
              two.color = c("black", "gray50"))
}
}
\author{Tengfei Yin}
