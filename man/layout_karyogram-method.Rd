\name{layout_karyogram}
\alias{layout_karyogram}
\alias{layout_karyogram,GRanges-method}
\title{Create a karyogram layout}
\description{
  Create a karyogram layout.
}
\usage{
\S4method{layout_karyogram}{GRanges}(data, ..., xlab, ylab, main,
          facets = seqnames ~ ., cytobands = FALSE, geom = "rect",
          stat = NULL, ylim = NULL, rect.height = 10)

}
\arguments{
  \item{data}{
    a \code{GRanges} object, which could contain extra
    information about cytobands. If you want an accurate genome mapping, please provide
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
  \item{cytobands}{
    logical value indicate to show the cytobands or not.
  }
  \item{geom}{
    The geometric object to use display the data. 
  }
  \item{stat}{
    character vector specifying statistics to use.
  }
  \item{ylim}{
    limits for y axis, usually the chromosome spaces y limits are from 0
    to rect.height, which 10, so if you wan to stack some data on top of
    it, you can set limits to like c(10, 20).
  }
  \item{rect.height}{
    numreic value indicate half of the rectangle ploting region, used
    for alignment of multiple layers.
  }}
\value{
  A 'Layer'.
}
\examples{
### R code from vignette source 'karyogram.Rnw'

###################################################
### code chunk number 1: loading
###################################################
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
head(hg19IdeogramCyto)
## default pre-set color stored in 
getOption("biovizBase")$cytobandColor


###################################################
### code chunk number 2: default
###################################################
autoplot(hg19IdeogramCyto, layout = "karyogram", cytobands = TRUE)



###################################################
### code chunk number 3: change-order
###################################################
library(GenomicRanges)
hg19 <- keepSeqlevels(hg19IdeogramCyto, paste0("chr", c(1:22, "X", "Y")))
head(hg19)
autoplot(hg19, layout = "karyogram", cytobands = TRUE)


###################################################
### code chunk number 4: cyto-normal
###################################################
library(GenomicRanges)
## it's a 'ideogram'
biovizBase::isIdeogram(hg19)
## set to FALSE
autoplot(hg19, layout = "karyogram", cytobands = FALSE, aes(fill = gieStain)) +
  scale_fill_giemsa()


###################################################
### code chunk number 5: load-RNAediting
###################################################
data(darned_hg19_subset500, package = "biovizBase")
dn <- darned_hg19_subset500
head(dn)
## add seqlengths
## we have seqlegnths information in another data set
data(hg19Ideogram, package = "biovizBase")
seqlengths(dn) <- seqlengths(hg19Ideogram)[names(seqlengths(dn))]
## now we have seqlengths
head(dn)
## then we change order
dn <- keepSeqlevels(dn, paste0("chr", c(1:22, "X")))
autoplot(dn, layout = "karyogram")
## this equivalent to 
## autoplot(seqinfo(dn))


###################################################
### code chunk number 6: load-RNAediting-color
###################################################
## since default is geom rectangle, even though it's looks like segment
## we still use both fill/color to map colors
autoplot(dn, layout = "karyogram", aes(color = exReg, fill = exReg))


###################################################
### code chunk number 7: load-RNAediting-color-NA
###################################################
## since default is geom rectangle, even though it's looks like segment
## we still use both fill/color to map colors
autoplot(dn, layout = "karyogram", aes(color = exReg, fill = exReg)) +
  scale_color_discrete(na.value = "brown")


###################################################
### code chunk number 8: load-RNAediting-color-fake
###################################################
dn2 <- dn
seqlengths(dn2) <- rep(max(seqlengths(dn2)), length(seqlengths(dn2)) )
autoplot(dn2, layout = "karyogram", aes(color = exReg, fill = exReg)) 


###################################################
### code chunk number 9: plotKaryogram (eval = FALSE)
###################################################
## plotKaryogram(dn)
## plotKaryogram(dn, aes(color = exReg, fill = exReg))


###################################################
### code chunk number 10: low-default
###################################################
## plot ideogram
p <- ggplot(hg19) + layout_karyogram(cytobands = TRUE)
p
## eqevelant autoplot(hg19, layout = "karyogram", cytobands = TRUE)


###################################################
### code chunk number 11: low-default-addon
###################################################
p <- p + layout_karyogram(dn, geom = "rect", ylim = c(11, 21), color = "red")
## commented line below won't work
## the cytoband fill color has been used already.
## p <- p + layout_karyogram(dn, aes(fill = exReg, color = exReg), geom = "rect")
p


###################################################
### code chunk number 12: edit-space
###################################################
## plot chromosome space
p <- autoplot(seqinfo(dn))
## make sure you pass rect as geom
## otherwise you just get background
p <- p + layout_karyogram(dn, aes(fill = exReg, color = exReg), geom = "rect")
values(dn)$pvalue <- rnorm(length(dn))
p + layout_karyogram(dn, aes(x = start, y = pvalue), ylim = c(10, 30), geom = "line", color = "red")
p


###################################################
### code chunk number 13: sessionInfo
###################################################
sessionInfo()
}
\author{Tengfei Yin}

