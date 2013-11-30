\name{ggplot}
\alias{ggplot}
\alias{ggplot,ANY-method}
\alias{ggplot,GRanges-method}
\alias{ggplot,GRangesList-method}
\alias{ggplot,IRanges-method}
\alias{ggplot,Seqinfo-method}
\alias{ggplot,matrix-method}
\alias{ggplot,Views-method}
\alias{ggplot,ExpressionSet-method}
\alias{ggplot,SummarizedExperiment-method}
\alias{ggplot,VCF-method}
\alias{ggplot,GAlignments-method}
\alias{ggplot,BamFile-method}
\alias{ggplot,character-method}
\alias{ggplot,TranscriptDb-method}
\alias{ggplot,BSgenome-method}
\alias{ggplot,Rle-method}
\alias{ggplot,RleList-method}
\title{ggplot generic method}
\description{
  Usage is similar to \code{ggplot} function in ggplot2 package, but support
  more objects defined in Bioconductor, or even for object like matrix,
  which intitialize a ggbio object. It can be used to declare
  the input data frame for a graphic and to specify the set of plot
  aesthetics intended to be common throughout all subsequent layers
  unless specifically overridden. And more than that, it store the
  original data object before coercion into the returned object.
  Please check the behavior for  \code{\link{mold}} method to see how
  each object coerced into data.frame.
}
\usage{
\S4method{ggplot}{GRanges}(data, ...)
\S4method{ggplot}{GRangesList}(data, ...)
\S4method{ggplot}{IRanges}(data, ...)
\S4method{ggplot}{Seqinfo}(data, ...)
\S4method{ggplot}{matrix}(data, ...)
\S4method{ggplot}{Views}(data, ...)
\S4method{ggplot}{ExpressionSet}(data, ...)
\S4method{ggplot}{SummarizedExperiment}(data, assay.id = 1, ...)
\S4method{ggplot}{VCF}(data, ...)
\S4method{ggplot}{GAlignments}(data, ...)
\S4method{ggplot}{BamFile}(data, ...)
\S4method{ggplot}{character}(data, ...)
\S4method{ggplot}{TranscriptDb}(data, ...)
\S4method{ggplot}{BSgenome}(data, ...)
\S4method{ggplot}{Rle}(data, ...)
\S4method{ggplot}{RleList}(data, ...)
}
\arguments{
  \item{data}{
    original data object.
  }
  \item{...}{
    other arguments passed to specific methods.
  }
  \item{assay.id}{
    index of assay you are using when multiple assays exist.
  }
}
\value{
  a return \code{ggbio} object, which is a subclass of \code{ggplot}
  defined in ggplot2 package, but that's more, a '.data' list entry is
  stored with the returned object. 
}
\details{
    The biggest difference for objects returned by \code{ggplot} in
    ggbio from ggplot2, is we always keep the original data copy, this
    is useful because in ggbio, our starting point is not always
    data.frame, many special statistical transformation is computed upon
    original data objects instead of coerced data.frame. This is a hack
    to follow ggplot2's API while allow our own defined components to
    trace back to original data copy and do the transformation. For
    objects supported by \code{mold} we transform them to
    data.frame stored along the original data set, for objects which not
    supported by \code{mold} method, we only store the original copy
    for ggbio specific graphics.
    
    \code{ggplot()} is typically used to construct a plot incrementally,
    using the \code{+} operator to add layers to the existing ggplot object.
    This is advantageous in that the code is explicit about which
    layers are added and the order in which they are added. For
    complex graphics with multiple layers, initialization with
    \code{ggplot} is recommended. You can always call \code{qplot} in
    package ggplot2 or \code{autoplot} in ggbio for convenient usage. 

    There are three common ways to invoke \code{ggplot}:
    \itemize{    
      \item{}{\code{ggplot(df, aes(x, y, <other aesthetics>))}}
      \item{}{\code{ggplot(df)}}
      \item{}{\code{ggplot()}}
    }
    
    The first method is recommended if all layers use the same data and
    the same set of aesthetics, although this method can also be used to
    add a layer using data from another data frame.  The second method
    specifies the default data frame to use for the plot, but no
    aesthetics are defined up front.  This is useful when one data frame
    is used predominantly as layers are added, but the aesthetics may
    vary from one layer to another.  The third method initializes a
    skeleton \code{ggplot} object which is fleshed out as layers are
    added. This method is useful when multiple data frames are used to
    produce different layers, as is often the case in complex graphics.

    The examples below illustrate how these methods of invoking
    \code{ggplot} can be used in constructing a graphic.
}
\seealso{\code{\link{mold}}, \code{\link{ggbio}}}
\examples{
set.seed(1)
N <- 100
library(GenomicRanges)
## GRanges
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

## automatically facetting and assign y
## this must mean geom_rect support GRanges object
ggplot(gr) + geom_rect()
ggplot(gr) + geom_alignment()
ggplot() + geom_alignment(gr)


## use pure ggplot2's geom_rect, no auto facet
ggplot(gr) + ggplot2::geom_rect(aes(xmin = start, ymin = score,
                               xmax = end, ymax = score + 1))

## GRangesList
grl <- split(gr, values(gr)$pair)
ggplot(grl) + geom_alignment()
ggplot(grl) + geom_rect()
ggplot(grl) + ggplot2::geom_rect(aes(xmin = start, ymin = score,
                               xmax = end, ymax = score + 1))


## IRanges
ir <- ranges(gr)
ggplot(ir) + geom_rect()
ggplot(ir) + layout_circle(geom = "rect")

## Seqinfo
seqlengths(gr) <- c(400, 500, 420)
ggplot(seqinfo(gr)) + geom_point(aes(x = midpoint, y = seqlengths))

## matrix
mx <- matrix(1:12, nrow = 3)
ggplot(mx, aes(x = x, y = y)) + geom_raster(aes(fill = value))
## row is the factor
ggplot(mx, aes(x = x, y = row)) + geom_raster(aes(fill = value))
colnames(mx)
colnames(mx) <- letters[1:ncol(mx)]
mx
## has extra 'colnames'
ggplot(mx, aes(x = x, y = row)) + geom_raster(aes(fill = colnames)) 
rownames(mx)
rownames(mx) <- LETTERS[1:nrow(mx)]
ggplot(mx, aes(x = x, y = row)) + geom_raster(aes(fill = rownames))
## please check autoplot, matrix for more control

##  Views
% subject <- XInteger(10, 3:-6)
% ## equal width
% v1 <- Views(subject, start=4:1, width = 3)
% ggplot(v1) + geom_tile(aes(x = x, y = row, fill = value))
% ggplot(v1) + geom_tile(aes(x = x, y = factor(1), fill = value)) +
% facet_grid(group ~. )
% ## unequal width
% v2 <- Views(subject, start=4:1, width = 3:6)  
% ggplot(v2) + geom_tile(aes(x = x, y = row, fill = value))

## ExpressionSet
library(Biobase)
data(sample.ExpressionSet)
sample.ExpressionSet
set.seed(1)
## select 50 features
idx <- sample(seq_len(dim(sample.ExpressionSet)[1]), size = 50)
eset <- sample.ExpressionSet[idx,]

ggplot(eset) + geom_tile(aes(x = x, y = y, fill = value))
## please check autoplot,matrix method which gives you more control
ggplot(eset) + geom_tile(aes(x = x, y = y, fill = sex))
ggplot(eset) + geom_tile(aes(x = x, y = y, fill = type))

## Rle
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
ggplot(xRle) + geom_tile(aes(x = x, y = y, fill = value))

## RleList
xRleList <- RleList(xRle, 2L * xRle)
xRleList
ggplot(xRleList) + geom_tile(aes(x = x, y = y, fill = value)) +
facet_grid(group~.)
names(xRleList) <- c("a" ,"b")
ggplot(xRleList) + geom_tile(aes(x = x, y = y, fill = value)) +
facet_grid(group~.)


## SummarizedExperiments
library(GenomicRanges)
nrows <- 200; ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
counts2 <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowData <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                   IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                   strand=sample(c("+", "-"), 200, TRUE))
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                     row.names=LETTERS[1:6])
sset <- SummarizedExperiment(assays=SimpleList(counts=counts,
                                               counts2 = counts2),
                             rowData=rowData, colData=colData)
ggplot(sset) + geom_raster(aes(x = x, y = y , fill = value))

}

\author{Tengfei Yin}

