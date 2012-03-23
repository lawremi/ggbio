\name{layout_karyogram}
\alias{layout_karyogram}
\alias{layout_karyogram,GRanges-method}
\title{Create a karyogram layout}
\description{
  Create a karyogram layout.
}
\usage{
\S4method{layout_karyogram}{GRanges}(data,..., xlab, ylab, main,
                 facets = seqnames ~ ., cytoband = FALSE,
                 geom = NULL, stat = NULL, ylim = NULL,
                 rect.height = 10)
}
\arguments{
  \item{data}{
    a \code{GRanges} object, which could contain extra
    information about cytoband. If you want an accurate genome mapping, please provide
    \code{seqlengths} with this \code{GRanges} object,otherwise it will
    emit a warning and use data space to estimate the chromosome space
    which is very rough.
  }
  \item{...}{
    Extra parameters such as aes() or arbitrary \code{color} and \code{size}.
  }
  \item{xlab}{
    character vector or expression for x axis label.
  }
  \item{ylab}{
    character vector or expression for y axis label.
  }
  \item{main}{
    character vector or expression for plot title.
  }
  \item{facets}{
    faceting formula to use.
  }
  \item{cytoband}{
    logical value indicate to show the cytobands or not.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
  \item{stat}{
    character vector specifying statistics to use.
  }
  \item{ylim}{
    limits for y axis.
  }
  \item{rect.height}{
    numreic value indicate half of the rectangle ploting region, used
    for alignment of multiple layers.
  }}
\value{
  A 'Layer'.
}
\examples{
\dontrun{
library(biovizBase)
data(hg19IdeogramCyto, package = "biovizBase")
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
new.ideo <- keepSeqlevels(new.ideo, c(as.character(1:22) , "X", "Y"))
new.ideo
ggplot() + layout_karyogram(new.ideo, cytoband = TRUE)
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE)
data(darned_hg19_subset500, package = "biovizBase")
idx <- is.na(values(darned_hg19_subset500)$exReg)
values(darned_hg19_subset500)$exReg[idx] <- "unknown"
## blank background
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE)
## no background
ggplot() + layout_karyogram(darned_hg19_subset500, geom = "rect",
                            aes(color = exReg, fill = exReg))

## need to be consistency!
old.chrs <- seqnames(seqinfo(darned_hg19_subset500))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
darned_hg19_subset500 <- renameSeqlevels(darned_hg19_subset500, new.chrs)
dn <- darned_hg19_subset500
## overlayed
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "rect",
                   aes(color = exReg, fill = exReg))

values(dn)$score <- rnorm(length(dn))
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "line",
                   aes(x = midpoint, y = score))
## To make it more perseivable
## method one, rescale your data, since default is 0, 10
values(dn)$score2 <- rescale(values(dn)$score, c(0, 10))
ggplot() + layout_karyogram(new.ideo, cytoband = FALSE) +
  layout_karyogram(dn, geom = "area",
                   aes(x = midpoint, y = score2), fill = "steelblue")
}}
\author{Tengfei Yin}

