\name{geom_alignment}
\alias{geom_alignment}
\alias{geom_alignment,GRanges-method}
\title{Alignment geoms for GRanges object}
\description{
  Show interval data as alignment.
}
\usage{
% for GRanges
\S4method{geom_alignment}{GRanges}(data,..., xlab, ylab, main,
               facets = NULL, stat = c("stepping", "identity"),
               main.geom = c("rect", "arrowrect"),
               gap.geom = c("chevron", "arrow", "segment"),
               rect.height = NULL,  group.selfish = TRUE)
}
\arguments{
  \item{data}{
    A \code{GRanges} or \code{data.frame} object.
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
    Character vector specifying statistics to use. "stepping" with
    randomly assigned stepping levels as y varialbe. "identity" allow
    users to specify \code{y} value in \code{aes}.
  }
  \item{main.geom}{
    Geom for 'main' intervals which is the data you passed.
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
}
\value{
  A 'Layer'.
}
\examples{
## @knitr load
set.seed(1)
N <- 100
require(ggbio)
require(GenomicRanges)
## @knitr simul
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


## @knitr default
## ======================================================================
##  default
## ======================================================================
ggplot() + geom_alignment(gr)

## @knitr facet_aes
## ======================================================================
##  facetting and aesthetics
## ======================================================================
ggplot() + geom_alignment(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))

## @knitr stat:stepping
## ======================================================================
##  stat:stepping
## ======================================================================
ggplot() + geom_alignment(gr, stat = "stepping", aes(group = pair))

## @knitr group.selfish
## ======================================================================
##  group.selfish controls when 
## ======================================================================
ggplot() + geom_alignment(gr, stat = "stepping", aes(group = pair), group.selfish = FALSE)

## @knitr main_gap
## =======================================
##  main/gap geom
## =======================================
ggplot() + geom_alignment(gr, main.geom = "arrowrect", gap.geom = "chevron")

}
\author{Tengfei Yin}

