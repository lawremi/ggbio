\name{geom_alignment}
\alias{geom_alignment}
\alias{geom_alignment,GRanges-method}
\alias{geom_alignment,missing-method}
\alias{geom_alignment,uneval-method}
\alias{geom_alignment,TranscriptDb-method}
\alias{geom_alignment,BamFile-method}
\title{Alignment geoms for GRanges object}
\description{
  Show interval data as alignment.
}
\usage{
% for GRanges
\S4method{geom_alignment}{GRanges}(data, ..., xlab, ylab, main, facets = NULL, stat =
                 c("stepping", "identity"), range.geom = c("rect",
                 "arrowrect"), gap.geom = c("chevron", "arrow",
                 "segment"), rect.height = NULL, group.selfish = TRUE,
                  label = TRUE)

\S4method{geom_alignment}{TranscriptDb}(data, ..., which, xlim, truncate.gaps = FALSE,
                 truncate.fun = NULL, ratio = 0.0025, xlab, ylab, main,
                 facets = NULL, geom = "alignment",
                 stat = c("identity", "reduce"),
                 range.geom = "rect", gap.geom = "arrow",
                 utr.geom = "rect", names.expr = "tx_name",
                 label = TRUE)

}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} or \code{TranscriptDb} object.
  }
  \item{...}{
    Extra parameters such as aes() passed.
  }
  \item{xlab}{
    Label for x
  }
  \item{ylab}{
    Label for y
  }
  \item{main}{
    Title for plot.
  }
  \item{facets}{
    Faceting formula to use.
  }
  \item{stat}{
    For \code{\linkS4class{GRanges}}:
    Character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.

    For  \code{\linkS4class{TranscriptDb}}:
    defualt "identity" give full gene model and "reduce" for reduced model.    
  }
  \item{gap.geom}{
    Geom for 'gap' computed from the data you passed based on the group information.
  }
  \item{rect.height}{
    Half height of the arrow body. 
  }
  \item{group.selfish}{
    Passed to \code{addStepping}, control whether to show each group as
  unique level or not. If set to \code{FALSE}, if two groups are not
  overlapped with each other, they will probably be layout in the same
  level to save space.
}
  \item{which}{
    \code{GRanges} object to subset the \code{TranscriptDb} object.
  }
  \item{xlim}{
    Limits for x, to subset the \code{TranscriptDb} object.
  }
  \item{truncate.gaps}{
    logical value indicate to truncate gaps or not.
  }
  \item{truncate.fun}{
    shrinkage function. Please see \code{shrinkagefun} in package biovizBase.
  }
  \item{ratio}{
    used in \code{maxGap}.
  }
  \item{geom}{
    geometric object. only support "gene" now.
  }
  \item{range.geom}{
    geom for main intevals or exons.
  }
  \item{utr.geom}{
    geom for utr region.
  }
  \item{names.expr}{
    expression for showing y label.
  }
  \item{label}{
    logical value. Whether to label the intervals with names specified
    by argument \code{names.expr}. 
  }
}
\value{
  A 'Layer'.
}
\examples{
set.seed(1)
N <- 100
require(GenomicRanges)
## ======================================================================
##  simmulated GRanges
## ======================================================================
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


## ======================================================================
##  default
## ======================================================================
ggplot(gr) + geom_alignment()
## or
ggplot() + geom_alignment(gr)

## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot(gr) + geom_alignment(facets = sample ~ seqnames, aes(color = strand, fill = strand))

## ======================================================================
##  stat:stepping
## ======================================================================
ggplot(gr) + geom_alignment(stat = "stepping", aes(group = pair))

## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot(gr) + geom_alignment(stat = "stepping", aes(group = pair), group.selfish = FALSE)

## =======================================
##  main/gap geom
## =======================================
ggplot(gr) + geom_alignment(range.geom = "arrowrect", gap.geom = "chevron")

## =======================================
##  For TranscriptDb
## =======================================
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
## made a track comparing full/reduce stat.
p1 <- ggplot(txdb) + geom_alignment(which = genesymbol["RBM17"])
p2 <- ggplot(txdb) + geom_alignment(which = genesymbol["RBM17"], stat = "reduce")
tracks(full = p1, reduce = p2, heights = c(3, 1))
tracks(full = p1, reduce = p2, heights = c(3, 1)) + theme_tracks_sunset()
tracks(full = p1, reduce = p2, heights = c(3, 1)) +
     theme_tracks_sunset(axis.line.color = NA)
## change y labels
ggplot(txdb) + geom_alignment(which = genesymbol["RBM17"], names.expr = "tx_id:::gene_id")
}
\author{Tengfei Yin}

