\name{autoplot}

\alias{autoplot}
\alias{autoplot,GRanges-method}
\alias{autoplot,GRangesList-method}
\alias{autoplot,IRanges-method}
\alias{autoplot,BSgenome-method}
\alias{autoplot,GappedAlignments-method}
\alias{autoplot,BamFile-method}
\alias{autoplot,TranscriptDb-method}
\alias{autoplot,character-method}
\alias{autoplot,Rle-method}
\alias{autoplot,RleList-method}
\alias{autoplot,ExpressionSet-method}
\alias{autoplot,GenomicRangesList-method}
\alias{autoplot,VCF-method}


\title{Generic autoplot function}

\description{
  To visualize different objects describing biological data,
  we develop this generic function, and developed new types of geoms to
  each one. Try to make simple API and following the grammar of graphics,
  use higher level graphic package like ggplot2 to produce high quality
  graphics.
}

\usage{
## For object GRanges
\S4method{autoplot}{GRanges}(object, ..., xlab, ylab, main, truncate.gaps = FALSE,
                 truncate.fun = NULL, ratio = 0.0025, space.skip = 0.1,
                 legend = TRUE, geom = NULL, stat = NULL, coord =
                 c("default", "genome", "truncate_gaps"), layout =
                 c("linear", "karyogram", "circle"))


## For object GRangesList
\S4method{autoplot}{GRangesList}(object, ..., xlab, ylab, main, indName = "grl_name",
                 geom = NULL, stat = NULL, type = c("none", "sashimi"),
                 coverage.col = "gray50", coverage.fill = coverage.col,
                 group.selfish = FALSE, arch.offset = 1.3)


## For object IRanges
\S4method{autoplot}{IRanges}(object, ..., xlab, ylab, main)


## For object GappedAlignments
\S4method{autoplot}{GappedAlignments}(object, ..., xlab, ylab, main, which,
      geom = "gapped.pair", show.junction = FALSE)

## For object BamFile
\S4method{autoplot}{BamFile}(object, ..., which,
      xlab, ylab, main, bsgenome, geom = "line", stat = "coverage",
     method = c("estimate", "raw"), resize.extra = 10, show.coverage = TRUE)

## For object character
\S4method{autoplot}{character}(object, ..., xlab, ylab, main, asRangedData = FALSE)

## For object TranscriptDb
\S4method{autoplot}{TranscriptDb}(object, which, ..., xlab, ylab, main, truncate.gaps =
                 FALSE, truncate.fun = NULL, ratio = 0.0025, geom =
                 c("gene", "reduced_gene"), names.expr =
                 expression(paste(tx_name, "(", gene_id, ")", sep =
                 "")))



## For object BSgenome
\S4method{autoplot}{BSgenome}(object, which, ...,
                    xlab, ylab, main, geom = c("text",
                   "segment", "point", "rect"))



## For object Rle
\S4method{autoplot}{Rle}(object, lower, ..., xlab = "x", ylab = "y", main,
                 color, size, shape, alpha, geom = c("point", "line",
                 "segment"), type = c("raw", "viewMaxs", "viewMins",
                 "viewSums", "viewMeans"))


## For object RleList
\S4method{autoplot}{RleList}(object, lower, ...,
                 xlab = "x", ylab = "y", main,
                 size, shape, color, alpha, facetByRow = TRUE, geom =
                 c("point", "line", "segment"), type = c("raw",
                 "viewMaxs", "viewMins", "viewSums", "viewMeans"))


## For object ExpressionSet
\S4method{autoplot}{ExpressionSet}(object, ..., type = c("none", "heatmap",
                                    "matrix", "parallel", "MA",
                                    "mean-sd", "volcano",
                                    "NUSE", "RLE"),
                     test.method = "t")

## For object GenomicRangesList
\S4method{autoplot}{GenomicRangesList}(object, args = list(), trackWidth,
                    radius = 10, grid = FALSE, trackSkip = 1, layout = c("circle"))
## For object VCF
\S4method{autoplot}{VCF}(object, ..., xlab, ylab, main,
                    type = c("geno", "info", "fixed"),
                    ylabel = TRUE)

}

\arguments{
  \item{object}{
    object to be plot.
  }
  \item{x}{
    x value, start/end/midpoint without quotes. e.g \code{x = start},
    default use the \code{midpoint}.
  }
  \item{y}{
    y value, only be used in geom: point/line. Should be a single name
    of the column names in the elementMetatata without quotes.
    e.g \code{y = score}
  }
  \item{geom}{
    Geom to use (Single character for now). Please see section Geometry
    for details.
  }
  \item{size}{
    Size for point or lines. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{shape}{
    Shape for point or lines. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{color}{
    Color for point for lines. Could equal a column name or a fixed
    character. When it's fixed, please use \code{I()} to wrap the value.
  }
  \item{alpha}{
    Alpha blending. Could equal a column name or a fixed
    number. When it's fixed, please use \code{I()} to wrap the value.
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
  \item{space.skip}{
    space ratio between chromosome spaces in coordate genome.
  }
  \item{coord}{
    Coodinate system.
  }
  \item{lower}{
    When object is Rle/RleList, and use other types of methods which is
  not "raw", at least a lower parameters which passed to \code{slice}
  function is required.
    }
  \item{legend}{
    A logical value indicates whether to show legend or not. Default is
    \code{TRUE}
  }

  \item{which}{
    A \code{\link{GRanges}} object to subset the result, usually passed
    to the \code{\link{ScanBamParam}} function.
  }
  \item{show.junction}{
    A logical value indicates whether to show the line between junction reads or
    not.
  }
  \item{show.coverage}{
    A logical value indicates whether to show coverage or not. This is
  used for geom "mismatch.summary".
}
  \item{resize.extra}{
    A numeric value used to add buffer to intervals to compute stepping
    levels on.
  }
  \item{bsgenome}{
    A BSgenome object. Only need for geom "mismatch.summary".
  }
 \item{xlab}{
    x label.
  }
\item{ylab}{
    y label.
  }
\item{facetByRow}{
    A logical value, default is TRUE ,facet RleList by row. If FALSE,
   facet by column.
}
\item{type}{
   For Rle/RleList, "raw" plot everything, so be careful, that would be
  pretty slow if you have too much data. For "viewMins", "viewMaxs",
  "viewMeans", "viewSums", require extra arguments to slice the
  object. so users need to at least provide \code{lower}, more details
  and control please refer the the manual of \code{slice} function in
  IRanges. For "viewMins", "viewMaxs", we use \code{viewWhichMin} and
  \code{viewWhichMax} to get x scale, for "viewMeans", "viewSums", we
  use window midpoint as x.

  For ExpreesionSet, ploting types.
}
\item{args}{
  a list of arguments list which applied to each track.
}
\item{trackWidth}{
  Numeric values indicate width for each track, if it's only one value, then applied the same
value to each track, otherwise, the track must be of the same length of
track numbers.
}
\item{radius}{
  numeric value indicate inner radius of the innermost circle.
}
\item{grid}{
  logical value of length 1 or the same lengths of tracks numbers,
indicate whether you want to adding grid background or not.
}
\item{trackSkip}{
  numeric values indicate skipped region between tracks.
}
\item{layout}{
  Layout including linear, circular and karyogram. for \code{GenomicRangesList}, it only supports circular layout.
}
\item{method}{
  method used for parsing coverage from bam files. 'estimate' use fast
  esitmated method  and 'raw' use relatively slow parsing method.
}
\item{asRangedData}{
  when object is character, it may be imported by \code{import} function
  in package rtracklayer, then asRangedData passed to \code{import}
  function.  If FALSE, coerce object to \code{GRanges}.
}
\item{ylabel}{
 logical value indicates to show y labels or not.
}
\item{test.method}{
  test method
}
  \item{...}{
    Extra parameters. Usually are those parameters used in autoplot to
    control aesthetics or geometries.
  }
  \item{main}{
    title.
  }
  \item{stat}{
    statistical transformation.
  }
  \item{indName}{
    When coerce \code{GRangesList} to \code{GRanges}, names created for
    each group.
  }
  \item{coverage.col}{
    coverage stroke color.
  }
  \item{coverage.fill}{
    coverage fill color.
  }
  \item{group.selfish}{
        Passed to \code{addStepping}, control whether to show each group as
    unique level or not. If set to \code{FALSE}, if two groups are not
    overlapped with each other, they will probably be layout in the same
    level to save space.
  }
  \item{arch.offset}{
    arch.offset.
  }
  \item{names.expr}{
    names expression used for creating labels.
  }
}
\value{
  A \code{ggplot} object, so you can use common features from ggplot2
  package to manipulate the plot.
}

\section{Introduction}{
  \code{autoplot} is redefined as generic s4 method inside this package,
  user could use \code{autoplot} in the way they are familiar with, and
  we are also setting limitation inside this package, like

  \itemize{
    \item{scales}
    {X scales is always genomic coordinates in most cases, x could be
      specified as start/end/midpoint when it's special geoms for
      interval data like point/line}

    \item{colors}{
      Try to use default color scheme defined in biovizBase package
      as possible as it can
    }
  }
}

\section{Geometry}{
  \describe{
    We have developed new \code{geom} for different objects, some of
  them may require extra parameters you need to provide. Some of the
  geom are more like geom + stat in ggplot2
  package. e.g. "coverage.line" and "coverage.polygon".We simply combine
  them together, but in the future, we plan to make it more general.

  This package is designed for only biological data, especially genomic
  data if users want to explore the data in a more flexible way, you
  could simply coerce the \code{\link{GRanges}} to a data.frame, then
  just use formal \code{autoplot} function in ggplot2, or \code{autoplot}
  generic for \code{data.frame}.

  Some objects share the same geom so we introduce all the geom together
  in this section
  \item{full}{
    Showing all the intervals as stepped rectangle, colored by strand
    automatically.

    For \code{TranscripDb} object, showing full model.
  }
  \item{segment}{
    Showing all the intervals as stepped segments, colored by strand
    automatically.

    For object \code{BSgenome}, show nucleotides as colored segment.

    For Rle/RleList, show histogram-like segments.
  }
  \item{line}{
    Showing interval as line, the interval data could also be just
  single position when start = end, x is one of start/end/midpoint, y
  value is unquoted name in elementMetadata column names. y value is
  required.
  }
  \item{point}{
    Showing interval as point, the interval data could also be just
  single position when start = end, x is one of start/end/midpoint, y
  value is unquoted name in elementMetadata column names. y value is
  required.

  For object \code{BSgenome}, show nucleotides as colored point.
  }
  \item{coverage.line}{
    Coverage showing as lines for interval data.
  }
  \item{coverage.polygon}{
    Coverage showing as polygon for interval data.
  }
  \item{splice}{
    Splicing summary. The size and width of the line and rectangle
    should represent the counts in each model. Need to provide model.
  }
  \item{single}{
    For \code{TranscripDb} object, showing single(reduced) model only.
  }
  \item{tx}{
    For \code{TranscripDb} object, showing transcirpts isoforms.
  }
  \item{gapped.pair}{
    Show \code{GappedAlignments} as special stepping plots, it make sure
  all the reads of the same pair or qname shown in the same level and
  nothing falls in between. Then you can use \code{show.junction}
  arguments show the junction as lines between junction reads if any.
}
\item{mismatch.summary}{
  Showing color coded mismatched stacked bar to indicate the proportion
  of mismatching at each position, the reference is set to gray.
}
  \item{text}{
    For object \code{BSgenome}, show nucleotides as colored text.
  }
  \item{rectangle}{
    For object \code{BSgenome}, show nucleotides as colored rectangle.
  }
}
}

\section{Faceting}{
Faceting in ggbio package is a little differnt from ggplot2 in several ways
\itemize{
  \item {}{
    The faceted column could only be seqnames or regions
  on the genome. So we limited the formula passing to facet argument,
  e.g something \~ seqnames, is accepted formula, you can change
  "something" to variable name in the elementMetadata. But you
  can not change the second part.
   }
  \item{}{
    Sometime, we need to view different regions, so we also have a
    facet_gr argument which accept a \code{GRanges}. If
    this is provided, it will override the default seqnames and use
    provided region to facet the graphics, this might be useful for
    different gene centric views.
}
}
}


\author{Tengfei Yin}

\examples{
\dontrun{
library(ggbio)

## overide autoplot
autoplot(data = mtcars, mpg, cyl)
autoplot(1:3)
autoplot(volcano)
autoplot(c(1, 2.2, 3.3))
ggplot2::autoplot(1:3)
ggplot2::autoplot(c(1, 2.2, 3.3))
ggplot2::autoplot(volcano)


##  GRanges
set.seed(1)
N <- 1000
library(GenomicRanges)
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              group = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

autoplot(gr)
autoplot(gr, geom = "full")
autoplot(gr, geom = "segment")
autoplot(gr, geom = "line", y = value)
autoplot(gr, geom = "point", y = value)
autoplot(gr, geom = "coverage.line")
autoplot(gr, geom = "coverage.polygon")

gr.sub <- gr[seqnames(gr) == "chr1"] #or 
p1 <- autoplot(gr.sub, geom = "full") + opts(title = "full")
p2 <- autoplot(gr.sub, geom = "point", y = value) + opts(title = "point")
p3 <- autoplot(gr.sub, geom = "line", y = value) + opts(title = "line")
p4 <- autoplot(gr.sub, geom = "coverage.line") + opts(title = "coverage.line")   
p5 <- autoplot(gr.sub, geom = "coverage.polygon") + opts(title = "coverage.polygon")   
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

autoplot(gr, ncol = 2)
## faceting, use facets not facet
autoplot(gr, facets = group ~ seqnames)
autoplot(gr, geom = "segment", facets = group ~ seqnames)
autoplot(gr, geom = "line", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "point", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "coverage.line", facets = group ~ seqnames)
autoplot(gr, geom = "coverage.polygon", facets = group ~ seqnames)

## facet gr
gr.region <- GRanges(c("chr1", "chr2", "chr3"), 
                     IRanges(c(100, 200, 250), 
                             width = 70))
## facet_grid
autoplot(gr, facet_gr = gr.region)
## facet_wrap
autoplot(gr, facet_gr = gr.region, nrow = 2) + 
  scale_y_continuous(limits = c(0, 90))


## checvron
gr <- GRanges("chr1", IRanges(c(100, 200, 300), width = 50))
p <- autoplot(gr)
gr.gaps <- gaps(gr)[-1]
values(gr.gaps)$score <- c(1, 100)
p1 <- p + geom_chevron(gr.gaps)
p2 <- p + geom_chevron(gr.gaps, aes(size = score), offset = "score",
                 chevron.height = c(0.1, 0.2))
p3 <- p + geom_chevron(gr.gaps, offset = -0.1)
tracks(p1, p2, p3)

## GRangesList
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
exons.tx <- exonsBy(txdb, by = "tx")
exons.rbm17 <- subsetByOverlaps(exons.tx, genesymbol["RBM17"])
nms <- names(exons.rbm17)
freqs <- c(100, 200, 300)
names(freqs) <- nms
p.splice1 <- autoplot(exons.rbm17)
## when turning on frequency 
p.splice <- autoplot(exons.rbm17, freq = freqs, show.label = TRUE, label.type = "count",
      scale.size = c(1, 5), label.size = 3)
p.splice2 <- autoplot(exons.rbm17, freq = freqs, show.label = TRUE, offset = 0.05,
                   label.type = "count")
print(p.splice1)
print(p.splice2)

ir <- IRanges(c(10, 20, 30) ,width  = 15)
autoplot(ir)
ir <- ranges(gr[seqnames(gr) == "chr1"])[1:40]
p1 <- autoplot(ir) + opts(title = "full")
p2 <- autoplot(ir, geom = "segment")+ opts(title = "segment")
p3 <- autoplot(ir, geom = "coverage.line")+ opts(title = "coverage.line")
p4 <- autoplot(ir, geom = "coverage.polygon")+ opts(title = "coverage.polygon")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

library(IRanges)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)


autoplot(xRle)
autoplot(xRle, geom = "line")
autoplot(xRle, geom = "segment")
autoplot(xRle, type = "viewMaxs", lower = 5)
autoplot(xRle, type = "viewMins", lower = 5)
autoplot(xRle, type = "viewMeans", lower = 5)
autoplot(xRle, type = "viewSums", lower = 5)

autoplot(xRleList)
autoplot(xRleList, geom = "segment")
autoplot(xRleList, geom = "line")
autoplot(xRleList, type = "viewMaxs", lower = 5)
autoplot(xRleList, type = "viewMaxs", lower = 5, geom = "line")
autoplot(xRleList, type = "viewSums", lower = 5, geom = "segment",
      facetByRow = FALSE, color = I("red"), size = I(5))
autoplot(xRle, size = y)
autoplot(xRle, type = "viewSums", lower = 5)

autoplot(xRle, type = "viewSums", lower = 5, size = I(10), color = I("red"),
      alpha = y)
}
}