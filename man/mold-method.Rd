\name{mold}
\alias{mold}
\alias{mold,eSet-method}
\alias{mold,GRanges-method}
\alias{mold,IRanges-method}
\alias{mold,GRangesList-method}
\alias{mold,Seqinfo-method}
\alias{mold,matrix-method}
\alias{mold,Views-method}
\alias{mold,ExpressionSet-method}
\alias{mold,SummarizedExperiment-method}
\alias{mold,Rle-method}
\alias{mold,RleList-method}
\title{Mold an object to a data frame}
\description{
  S4 method to transform a object to a data.frame.
}
\usage{
\S4method{mold}{eSet}(data)
\S4method{mold}{GRanges}(data)
\S4method{mold}{IRanges}(data)
\S4method{mold}{GRangesList}(data,
          indName = "grl_name")
\S4method{mold}{IRanges}(data)
\S4method{mold}{Seqinfo}(data)
\S4method{mold}{matrix}(data)
\S4method{mold}{ExpressionSet}(data)
\S4method{mold}{SummarizedExperiment}(data, assay.id = 1)
\S4method{mold}{Views}(data)
\S4method{mold}{Rle}(data)
\S4method{mold}{RleList}(data)
}
\arguments{
  \item{data}{
    oringinal data object.
  }
  \item{indName}{
    character. When mold a \code{\linkS4class{GRangesList}} to a
    \code{\linkS4class{GRanges}} the collapsed list names will be added
    as a column named by \code{indName}, default is "grl_name".
  }
  \item{assay.id}{
    an integer indicates which assay to be used to mold into the data.frame.
  }
}
\value{
    a data.frame object.
}
\details{
    \describe{
      For different object, we try to maximize the information kept during
      molding to a data.frame. Most cases, this is different from simply
      use method as.data.frame. 

      \item{\code{\linkS4class{GRanges}}}{return a data.frame with
      extra 'midpoint' column, which is (start+end)/2.}

      \item{ \code{\linkS4class{GRangesList}}}{return a data.frame with
      extra 'midpoint' column, which is (start+end)/2, and with indName
      indicates which group they are originally from the list.}

      \item{\code{\linkS4class{IRanges}}}{return a data.frame with
      extra 'midpoint' column, which is (start+end)/2.}

      \item{\code{\linkS4class{Seqinfo}}}{return a data.frame 
      column: seqnames, start, end, width, strand, midpoint, seqlengths,
      isCircular, genome.}

      \item{\code{matrix}}{return a data.frame with 'x', 'y', and
      'value', 'row', 'col' column. If either colnames or rownames exists, a new
      'colnames' or 'rownames' column will be created and added to the
      data.frame. Notice, 'x' and 'y' are numeric coordinates in the
      matrix while 'col' and 'row' are the same value but are all factors.}
      
      \item{\code{\linkS4class{Views}}}{return a data.frame with
      'x', 'y', and 'value', 'start', 'end', 'width', 'midpoint', 'group'
      column. If either colnames or rownames exists, a new 'colnames' or
      'rownames' column will be created and added to the
      data.frame. This is achieved by coerce it to a matrix
      first. Additional variable 'row' will be added to indicate the
      group, but it actually equals to 'y'.}
      
      \item{\code{\linkS4class{ExpressionSet}}}{parse the matrix by using
      \code{exprs} on it and then \code{mold} the matrix to a
      data.frame with 'x', 'y', and 'value', 'col', 'row' column, 'colnames' for
      sample data and 'rownames' for features. 'x' and 'y' are numeric coordinates in the
      matrix while 'col' and 'row' are the same value but are all
      factors. The pheno data is also integrated with it.}

      \item{\code{\linkS4class{Rle}}}{coerce to a data.frame with
      column 'x', 'y', 'col', 'row', 'value'. 'x' and 'y' are numeric coordinates in the
      matrix while 'col' and 'row' are the same value but are all
      factors.}
    
      \item{\code{\linkS4class{RleList}}}{coerce to a data.frame with
      column 'x', 'y','col', 'row', 'value' and 'group', and 'group' variable
      indicates the original list entry number. 'x' and 'y' are numeric coordinates in the
      matrix while 'col' and 'row' are the same value but are all factors.}

      
      \item{\code{\linkS4class{SummarizedExperiment}}}{parse the matrix by using
      \code{exprs} on it and then \code{mold} the matrix to a
      data.frame with 'x', 'y', and 'value', 'col', 'row' column, 'colnames' for
      sample data and 'rownames' for features. 'x' and 'y' are numeric coordinates in the
      matrix while 'col' and 'row' are the same value but are all
      factors. The \code{colData} and \code{rowData} are also integrated with it.}
    }
}

\examples{
set.seed(1)
N <- 1000
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

## GRangesList
grl <- split(gr, values(gr)$pair)
head(mold(grl))
head(mold(grl, indName = "group_sample"))

## IRanges
ir <- ranges(gr)
head(mold(ir))

## Seqinfo
seqlengths(gr) <- c(400, 500, 420)
head(mold(seqinfo(gr)))


## matrix
mx <- matrix(1:12, nrow = 3)
head(mold(mx))
colnames(mx)
colnames(mx) <- letters[1:ncol(mx)]
mx
head(mold(mx))
rownames(mx)
rownames(mx) <- LETTERS[1:nrow(mx)]
head(mold(mx))

% ## Views
% subject <- c(10, 3:-6)
% ## equal width
% v1 <- Views(subject, start=4:1, width = 3)
% mold(v1)
% names(v1) <- letters[1:4]
% head(mold(v1))

## ExpressionSet
library(Biobase)
data(sample.ExpressionSet)
sample.ExpressionSet
set.seed(1)
## select 50 features
idx <- sample(seq_len(dim(sample.ExpressionSet)[1]), size = 50)
eset <- sample.ExpressionSet[idx,]
head(mold(eset))



## Rle
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
head(mold(xRle))


## RleList
xRleList <- RleList(xRle, 2L * xRle)
xRleList
head(mold(xRleList))
names(xRleList) <- c("a" ,"b")
xRleList
head(mold(xRleList))

## SummerizedExperiments
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
head(mold(sset))

## VCF
library(VariantAnnotation)
vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(vcffile, "hg19")


}
\author{Tengfei Yin}

