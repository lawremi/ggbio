\name{plotStackedOverview}
\alias{plotStackedOverview}
\alias{plotKaryogram}
\title{Plot stacked overview}
\usage{
plotStackedOverview(obj, ..., xlab, ylab, main, geom = "rect",
                         cytobands = FALSE, rescale = TRUE,
                         rescale.range = c(0, 10))
plotKaryogram(obj, ..., xlab, ylab, main, geom = "rect",
                         cytobands = FALSE, rescale = TRUE,
                         rescale.range = c(0, 10))
}
\description{
  Plot stacked overview for genome with or without cytobands. It's a
  wrapper around \code{layout_karyogram}.
}
\arguments{
  \item{obj}{
    a \code{GRanges} object, which could contain extra
    information about cytobands. If it's missing, will ask user to
    provide species information and download proper data set from UCSC.
    If you want an accurate genome mapping, please provide
    \code{seqlengths} with this \code{GRanges} object,otherwise it will
    emit a warning and use data space to estimate the chromosome space
    which is very rough.
  }
  \item{...}{
    arguments passed to graphic functions to control aesthetics. For
    example, if you use geom "point", you need to provide "y" in
    \code{aes()}, and if can also pass \code{color, fill, size} etc. to
    control graphics.
  }
  \item{xlab}{
    label for x
  }
  \item{ylab}{
    label for y
  }
  \item{main}{
    title for plot.
  }
  \item{geom}{
    geom plotted on the stacked layout. Default is "rect", which showing
    interval data as rectangles. It automatically figures out boundary
    so you don't have to provide information in \code{aes}, users could specify other supported
    geom works for \code{data.frame}.
  }
  \item{cytobands}{
    logical value. Default is \code{FALSE}. If \code{TRUE}, plotting cytobands, this
    require your data have arbitrary column as \code{name} and
    \code{gieStain}. the easiest way is to use \code{getIdeogram} to get
    your data. Notice for this function, when cytobands is \code{TRUE},
    it will only plot cytobands without overlaying your data. If you
    really need to overlay extra data on cytobands, please plus
    \code{layout_karyogram} for that purpose.
  }
  \item{rescale}{
    logical value. Default is \code{TRUE}, which rescale your data into
    the \code{rescale.range}, this make sure your data will not be
    plotted outside the stacked overview box.
  }
  \item{rescale.range}{
    Numeric range of length 2. Default is (0, 10), because stacked
    layout draws a white background as chromosome space and this space
    is of height 10. We hide the y-axis since we don't need it for
    stacked overview. Sometime users may want to leave some margin for
    their data, they can use this arguments to control the rescale.
  }
}
\details{
  Stacked overview is just a arbitrary layout for karyogram
  layout, it use facets seqnaems ~ . as default to stack the genome. For
  accurate mapping, you need to provide \code{seqlengths} information in
  your \code{GRanges} object. Otherwise, data space will be computed for
  stacked overview chromosome background, this is _NOT_ the actual
  chromosome space!.
}
\value{
  A \code{ggplot} object.
}
\author{Tengfei Yin}
\examples{
\dontrun{
library(biovizBase)
data(hg19IdeogramCyto, package = "biovizBase")
library(GenomicRanges)

## you can also get ideogram by biovizBase::getIdeogram

## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
new.ideo <- keepSeqlevels(new.ideo, c(as.character(1:22) , "X", "Y"))
new.ideo


## sample data
data(darned_hg19_subset500, package = "biovizBase")
idx <- is.na(values(darned_hg19_subset500)$exReg)
values(darned_hg19_subset500)$exReg[idx] <- "unknown"

## you need to add seqlengths for accruate mapping
chrnames <- unique(as.character(seqnames(darned_hg19_subset500)))
data(hg19Ideogram, package = "biovizBase")
seqlengths(darned_hg19_subset500) <- seqlengths(hg19Ideogram)[sort(chrnames)]


dn <- darned_hg19_subset500
values(dn)$score <- rnorm(length(dn))

## plotStackedOverview is a simple wrapper around this functions to
   create a stacked layout
plotStackedOverview(new.ideo, cytobands = TRUE)

plotStackedOverview(dn)
plotStackedOverview(dn, aes(color = exReg, fill = exReg))
## this will did the trick for you to rescale the space
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line")
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line", rescale.range = c(4, 6))
## no rescale
plotStackedOverview(dn, aes(x = midpoint, y = score), geom = "line", rescale = FALSE,
                    xlab = "xlab", ylab = "ylab", main  = "main") + ylab("ylab")

## no object? will ask you for species and query the data on the fly
plotStackedOverview()
plotStackedOverview(cytobands = TRUE)
}
}

