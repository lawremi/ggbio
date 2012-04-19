\name{tracks}
\alias{tracks}
\alias{Tracks-class}
\alias{xlim}
\alias{xlim,Tracks-method}
\alias{xlim,numeric-method}
\alias{xlim<-}
\alias{xlim<-,Tracks,numeric-method}
\alias{xlim<-,Tracks,IRanges-method}
\alias{xlim<-,Tracks,GRanges-method}
\alias{summary}
\alias{summary,Tracks-method}
\alias{show}
\alias{show,Tracks-method}
\alias{print}
\alias{print,Tracks-method}
\alias{Arith}
\alias{Arith,Tracks,ANY-method}
\alias{update}
\alias{update,Tracks-method}
\alias{reset}
\alias{reset,Tracks-method}
\alias{backup}
\alias{backup,Tracks-method}
\title{Tracks for genomic graphics}
\usage{
tracks(..., heights, xlim, xlab = NULL,                
           opts = NULL, track.skip = -1,
           xlim.change = rep(TRUE, length(list(...))),
           track.plot.col = rep("white", nrow))

\S4method{summary}{Tracks}(object)
\S4method{print}{Tracks}(x)
\S4method{show}{Tracks}(object)
\S4method{Arith}{Tracks,ANY}(e1, e2)
\S4method{xlim}{numeric}(obj, ...)
\S4method{xlim}{Tracks}(obj, ...)
xlim(x) <- value
\S4method{update}{Tracks}(object, xlim)
\S4method{reset}{Tracks}(obj)
\S4method{backup}{Tracks}(obj)

}
\arguments{
  \item{...}{plots of class ggplot2, trellis, or grobs, and valid
    arguments to grid.layout.}
  \item{heights}{numeric vector of the same length of passed graphic
    object to indicate the ratio of each track.}
  \item{xlim}{limits on x. could be \code{IRanges}, \code{GRanges}, \code{numeric} value}
  \item{xlab}{label for x axis.}
  \item{opts}{Option list or theme applied to each track.}
  \item{track.skip}{Numeric value, skip between tracks, unit is 'lines'.}
  \item{xlim.change}{Vector of logical value of the same length as
    passed graphic objects, to control whether we adjust that track with
    each other or just leave it as it is. This could be useful when you
    pass a single chromosome view on top of the tracks.}
  \item{track.plot.col}{plot background color for each track, default is
    white}
  \item{obj}{
    \code{Tracks} object.
  }
  \item{object}{
    \code{Tracks} object.
  }
  \item{x}{
    \code{Tracks} object.
  }
  \item{value}{
    Replaced xlim value.
  }
  \item{e1}{
    \code{Tracks} object on the left of '+'.
  }
  \item{e2}{
    \code{option} object like in 'ggplot2', on the right of '+'.
  }
}
\description{
  \code{tracks} is a conventient constructor for bindind graphics as trakcs. You dont' have
  to worry about adjusting different graphics, \code{tracks} did that for you. It's NOT
  just limited to bind genomic tracks, you can use this function to bind any
  tracks with the same defination of x axis, for example, sets of time series
  plots you made.

  Tracks view is most common way to viewing genome features and annotation data
  and widely used by most genome browsers. Our assumption is that, most graphics
  you made with ggbio or by yourself using ggplot2, are almost always sitting
  on the genomic coordinates or the same x axis. And to compare annotation
  information along with genome features, we need to align those plots on exactly
  the same x axis in order to form your hypothesis. This function leaves you the
  flexibility to construct each tracks separately with worrying your alignments
  later.

  ggbio provide a set of utilities to reset, backup, and apply options to
  tracks, please see examples below.
}
\details{\code{tracks} function has some extra special features.
  \itemize{
    \item{}{
      Only keep the bottom x axis based on assumption that all tracks are on the
      same space, but still keep x ticks. For simply wrapping, please use `align.plots`.
    }
    \item{}{
      `ncol` which defines columns is always 1, because binding tracks in the
      context of genomic data is almost always one single column. Multiple column
      alignments are not supported yet.
    }
  }
  also has some utilities.
  \itemize{
    \item{xlim}{
    }
    \item{reset,backup}{
      \code{reset} and {backup} help you play with options and appearance of the tracks,
      you could save certain status by calling {backup}, and get backup version back
      by calling {reset}.
    }
    \item{summary}{
      \code{summary} give you meta information about tracks.
    }
    \item{update}{
      \code{update} allow you to
      update a plot xlim on the fly, you can simply keep the plot window and run
      \code{update} to tweak with the view. Other wise you need to revise the tracks object
      and print it again.
    }
    \item{show,print}{
      show plots on your screen.
    }
  }
  }
\seealso{\code{\link{align.plots}}}
\value{
 A \code{Tracks} object.
}
\examples{
## @knitr load
## ==========================================================
## Load packages
## ==========================================================
## Load gene features for human
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
## ==========================================================
## Create tracks
## ==========================================================
## create two tracks
## full gene model
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "gene")
## reduced gene model
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
## building tracks
obj <- tracks(p1, p2, heights = c(3, 1))
## showing 
obj


## @knitr align.plots
## ==========================================================
## align.plots
## ==========================================================
align.plots(p1, p2)

## @knitr reset
## ==========================================================
## test reset/backup
## ==========================================================
## create tracks
obj <- tracks(p1, p2, heights = c(3, 1))
## show it
obj
## three ways to change x limits, IRanges/GRanges/numeric
xlim(obj) <- IRanges(start = 6145000, end = 6150000)
xlim(obj) <- GRanges("chr1", c(start = 6145000, end = 6150000))
xlim(obj) <- c(6145000, 6150000)
## show it
obj
## reset to original setting
obj <- reset(obj)
## get back
obj
## we could save a statue of the tracks to backup and then
## reset will get that copy back
xlim(obj) <- c(6145000, 6150000)
obj <- backup(obj)
obj@xlim <- c(6135000, 6150000)
obj
obj <- reset(obj)
obj

## @knitr utils
## ==========================================================
## utils
## ==========================================================
## summary information about a track
summary(obj)
## update a x limits on the fly, this is useful when you try to
## keep the view open and tweak with limits on the fly.
update(obj, xlim  = c(6130000, 6150000))

## @knitr opts
## ==========================================================
## options
## ==========================================================
## To make it easy, you could just apply any *options* by using "+"
## and this will apply it to every plot in the track.
obj + theme_bw()
}
\author{Tengfei Yin}


