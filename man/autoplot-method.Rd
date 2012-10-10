\name{autoplot}

\alias{autoplot}
\alias{autoplot,GRanges-method}
\alias{autoplot,GRangesList-method}
\alias{autoplot,IRanges-method}
\alias{autoplot,Seqinfo-method}
\alias{autoplot,BSgenome-method}
\alias{autoplot,GappedAlignments-method}
\alias{autoplot,BamFile-method}
\alias{autoplot,TranscriptDb-method}
\alias{autoplot,character-method}
\alias{autoplot,Rle-method}
\alias{autoplot,RleList-method}
\alias{autoplot,matrix-method}
\alias{autoplot,Views-method}
\alias{autoplot,ExpressionSet-method}
\alias{autoplot,SummarizedExperiment-method}
\alias{autoplot,VCF-method}


\title{Generic autoplot function}

\description{
  
  \code{autoplot} is a generic function to visualize various data
  object, it tries to give better default graphics and customized
  choices for each data type, quick and convenient to explore your
  genomic data compare to low level \code{ggplot} method, it is much
  simpler and easy to produce fairly complicate graphics, though you may
  lose some flexibility for each layer.
  
}

\usage{
\S4method{autoplot}{GRanges}(object, ..., xlab, ylab, main, truncate.gaps = FALSE,
                 truncate.fun = NULL, ratio = 0.0025, space.skip = 0.1,
                 legend = TRUE, geom = NULL, stat = NULL, coord =
                 c("default", "genome", "truncate_gaps"), layout =
                 c("linear", "karyogram", "circle"))


\S4method{autoplot}{GRangesList}(object, ..., xlab, ylab, main, indName = "grl_name",
                 geom = NULL, stat = NULL, type = c("none", "sashimi"),
                 coverage.col = "gray50", coverage.fill = coverage.col,
                 group.selfish = FALSE, arch.offset = 1.3)


\S4method{autoplot}{IRanges}(object, ..., xlab, ylab, main)

\S4method{autoplot}{Seqinfo}(object, single.ideo = TRUE, ... )

\S4method{autoplot}{GappedAlignments}(object, ..., xlab, ylab, main, which,
      geom = NULL, stat = NULL)

\S4method{autoplot}{BamFile}(object, ..., which, xlab, ylab, main,
              bsgenome, geom = "line", stat = "coverage",
              method = c("estimate", "raw"),
              coord = c("linear", "genome"), resize.extra = 10,
              space.skip = 0.1, show.coverage = TRUE)

\S4method{autoplot}{character}(object, ..., xlab, ylab, main, which, asRangedData = FALSE)

\S4method{autoplot}{TranscriptDb}(object, which, ..., xlab, ylab, main, truncate.gaps =
                 FALSE, truncate.fun = NULL, ratio = 0.0025, geom =
                 c("alignment"), stat = c("identity", "reduce"),
                 names.expr = "tx_name(gene_id)")



\S4method{autoplot}{BSgenome}(object, which, ...,
                    xlab, ylab, main, geom = c("text",
                   "segment", "point", "rect"))

\S4method{autoplot}{Rle}(object, ..., xlab, ylab, main, binwidth, nbin = 30,
          geom = NULL, stat = c("bin", "identity", "slice"),
          type = c("viewSums", "viewMins", "viewMaxs", "viewMeans"))

\S4method{autoplot}{RleList}(object, ..., xlab, ylab, main, nbin = 30, binwidth,
         facetByRow = TRUE, stat = c("bin", "identity", "slice"),
         geom = NULL, type = c("viewSums", "viewMins", "viewMaxs", "viewMeans"))

\S4method{autoplot}{matrix}(object, ..., xlab, ylab, main,
              geom = c("tile", "raster"), axis.text.angle = NULL,
              hjust = 0.5, na.value = NULL,
              rownames.label = TRUE, colnames.label = TRUE,
              axis.text.x = TRUE, axis.text.y = TRUE)


\S4method{autoplot}{ExpressionSet}(object, ..., type = c("heatmap", "none",
                 "scatterplot.matrix", "pcp", "MA", "boxplot",
                 "mean-sd", "volcano", "NUSE", "RLE"), test.method =
                 "t", rotate = FALSE, pheno.plot = FALSE, main_to_pheno
                 = 4.5, padding = 0.2)


\S4method{autoplot}{SummarizedExperiment}(object, ..., type = c("heatmap", "link", "pcp",
                 "boxplot", "scatterplot.matrix"), pheno.plot = FALSE,
                 main_to_pheno = 4.5, padding = 0.2, assay.id = 1)


\S4method{autoplot}{VCF}(object, ...,
              xlab, ylab, main,
              assay.id,                                      
              type = c("geno", "info", "fixed"),
              full.string = FALSE,
              ref.show = TRUE,
              genome.axis = TRUE,
              transpose = TRUE)


}

\arguments{
  \item{object}{
    object to be plot.
  }
  \item{transpose}{
    logical value, defaut TRUE, always make features from VCF as x, so
    we can use it to map to genomic position.
  }
  \item{axis.text.angle}{
    axis text angle.
  }
  \item{axis.text.x}{
    logical value indicates whether to show x axis and labels or not.
  }
  \item{axis.text.y}{
    logical value indicates whether to show y axis and labels or not.    
  }
  \item{hjust}{
    horizontal just for axis text.
  }
  \item{rownames.label}{
    logical value indicates whether to show rownames of matrix as y
  label or not.
  }
  \item{colnames.label}{
    logical value indicates whether to show colnames of matrix as y
  label or not.    
  }
  \item{na.value}{
    color for NA value.
  }
  \item{rotate}{
    
  }
  \item{pheno.plot}{
    show pheno plot or not.
  }
  \item{main_to_pheno}{
    main matrix plot width to pheno plot width ratio.
  }
  \item{padding}{
    padding between plots.
  }
  \item{assay.id}{
    index for assay you are going to use.
  }
  \item{geom}{
    Geom to use (Single character for now). Please see section Geometry
    for details.
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
  \item{legend}{
    A logical value indicates whether to show legend or not. Default is
    \code{TRUE}
  }

  \item{which}{
    A \code{\link{GRanges}} object to subset the result, usually passed
    to the \code{\link{ScanBamParam}} function.
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
  \item{binwidth}{
    width of the bins.
  }
  \item{nbin}{
    number of bins.
  }
  \item{genome.axis}{
    logical value, if TRUE, whenever possible, try to parse genomic postition
    for each column(e.g. SummarizedExperiment), show column as exatcly
    the genomic position instead of showing them side by side and
    indexed from 1.
  }
  \item{full.string}{
    logical value. If TRUE, show full string of indels in plot for VCF.
  }
  \item{ref.show}{
    logical value. If TRUE, show REF in VCF at bottom track.
  }
  \item{single.ideo}{
    for object 'Seqinfo', if length of object is 1, then this control
    whether to use \code{plotSingleChrom} or \code{layout_karyogram}.
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
library(ggbio)

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
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

idx <- sample(1:length(gr), size = 50)


###################################################
### code chunk number 3: default
###################################################
autoplot(gr[idx])


###################################################
### code chunk number 4: bar-default-pre
###################################################
set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b2 <- GRanges(seqnames = "chr2", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b <- c(gr.b, gr.b2)
head(gr.b)


###################################################
### code chunk number 5: bar-default
###################################################
p1 <- autoplot(gr.b, geom = "bar")
## use value to fill the bar
p2 <- autoplot(gr.b, geom = "bar", aes(fill = value))
tracks(default = p1, fill = p2)


###################################################
### code chunk number 6: autoplot.Rnw:236-237
###################################################
autoplot(gr[idx], geom = "arch", aes(color = value), facets = sample ~ seqnames)


###################################################
### code chunk number 7: gr-group
###################################################
library(ggbio)
gra <- GRanges("chr1", IRanges(c(1,7,20), end = c(4,9,30)), group = c("a", "a", "b"))
## if you desn't specify group, then group based on stepping levels, and gaps are computed without
## considering extra group method
p1 <- autoplot(gra, aes(fill = group), geom = "alignment")
## when use group method, gaps only computed for grouped intervals.
## default is group.selfish = TRUE, each group keep one row.
## in this way, group labels could be shown as y axis.
p2 <- autoplot(gra, aes(fill = group, group = group), geom = "alignment")
## group.selfish = FALSE, save space
p3 <- autoplot(gra, aes(fill = group, group = group), geom = "alignment", group.selfish = FALSE)
tracks('non-group' = p1,'group.selfish = TRUE' = p2 , 'group.selfish = FALSE' = p3)


###################################################
### code chunk number 8: gr-facet-strand
###################################################
autoplot(gr, stat = "coverage", geom = "area", 
         facets = strand ~ seqnames, aes(fill = strand))


###################################################
### code chunk number 9: gr-autoplot-circle
###################################################
autoplot(gr[idx], layout = 'circle') 


###################################################
### code chunk number 10: gr-circle
###################################################
seqlengths(gr) <- c(400, 500, 700)
values(gr)$to.gr <- gr[sample(1:length(gr), size = length(gr))]
idx <- sample(1:length(gr), size = 50)
gr <- gr[idx]
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, 
                aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
  layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6, trackWidth = 1)


###################################################
### code chunk number 11: seqinfo-src
###################################################
data(hg19Ideogram, package = "biovizBase")
sq <- seqinfo(hg19Ideogram)
sq


###################################################
### code chunk number 12: seqinfo
###################################################
autoplot(sq[paste0("chr", c(1:22, "X"))])


###################################################
### code chunk number 13: ir-load
###################################################
set.seed(1)
N <- 100
ir <-  IRanges(start = sample(1:300, size = N, replace = TRUE),
               width = sample(70:75, size = N,replace = TRUE))
## add meta data 
df <- DataFrame(value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))
values(ir) <- df
ir


###################################################
### code chunk number 14: ir-exp
###################################################
p1 <- autoplot(ir)
p2 <- autoplot(ir, aes(fill = pair)) + theme(legend.position = "none")
p3 <- autoplot(ir, stat = "coverage", geom = "line", facets = sample ~. )
p4 <- autoplot(ir, stat = "reduce")
tracks(p1, p2, p3, p4)


###################################################
### code chunk number 15: grl-simul
###################################################
set.seed(1)
N <- 100
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(30:40, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))


grl <- split(gr, values(gr)$pair)


###################################################
### code chunk number 16: grl-exp
###################################################
## default gap.geom is 'chevron'
p1 <- autoplot(grl, group.selfish = TRUE)
p2 <- autoplot(grl, group.selfish = TRUE, main.geom = "arrowrect", gap.geom = "segment")
tracks(p1, p2)


###################################################
### code chunk number 17: grl-name
###################################################
autoplot(grl, aes(fill = ..grl_name..))
## equal to 
## autoplot(grl, aes(fill = grl_name))


###################################################
### code chunk number 18: rle-simul
###################################################
library(IRanges)
library(ggbio)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))

## @knitr create
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRle


###################################################
### code chunk number 19: rle-bin
###################################################
p1 <- autoplot(xRle)
p2 <- autoplot(xRle, nbin = 80)
p3 <- autoplot(xRle, geom = "heatmap", nbin = 200)
tracks('nbin = 30' = p1, "nbin = 80" = p2, "nbin = 200(heatmap)" = p3)


###################################################
### code chunk number 20: rle-id
###################################################
p1 <- autoplot(xRle, stat = "identity")
p2 <- autoplot(xRle, stat = "identity", geom = "point", color = "red")
tracks('line' = p1, "point" = p2)


###################################################
### code chunk number 21: rle-slice
###################################################
p1 <- autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5)
p2 <- autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
tracks('bar' = p1, "heatmap" = p2)


###################################################
### code chunk number 22: rlel-simul
###################################################
xRleList <- RleList(xRle, 2L * xRle)
xRleList


###################################################
### code chunk number 23: rlel-bin
###################################################
p1 <- autoplot(xRleList)
p2 <- autoplot(xRleList, nbin = 80)
p3 <- autoplot(xRleList, geom = "heatmap", nbin = 200)
tracks('nbin = 30' = p1, "nbin = 80" = p2, "nbin = 200(heatmap)" = p3)


###################################################
### code chunk number 24: rlel-id
###################################################
p1 <- autoplot(xRleList, stat = "identity")
p2 <- autoplot(xRleList, stat = "identity", geom = "point", color = "red")
tracks('line' = p1, "point" = p2)


###################################################
### code chunk number 25: rlel-slice
###################################################
p1 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5)
p2 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
tracks('bar' = p1, "heatmap" = p2)


###################################################
### code chunk number 26: txdb
###################################################
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene


###################################################
### code chunk number 27: txdb-visual
###################################################
p1 <- autoplot(txdb, which = genesymbol["ALDOA"], names.expr = "tx_name:::gene_id")
p2 <- autoplot(txdb, which = genesymbol["ALDOA"], stat = "reduce", color = "brown", 
               fill = "brown")
tracks(full = p1, reduce = p2, heights = c(5, 1)) + ylab("")


###################################################
### code chunk number 28: ga-load
###################################################
library(Rsamtools)
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE)


###################################################
### code chunk number 29: ga-exp
###################################################
p1 <- autoplot(ga)
p2 <- autoplot(ga, geom = "rect")
p3 <- autoplot(ga, geom = "line", stat = "coverage")
tracks(default = p1, rect = p2, coverage = p3)


###################################################
### code chunk number 30: bf-load (eval = FALSE)
###################################################
## library(Rsamtools)
## bamfile <- "./wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
## bf <- BamFile(bamfile)


###################################################
### code chunk number 31: bf-est-cov (eval = FALSE)
###################################################
## autoplot(bamfile)
## autoplot(bamfile, which = c("chr1", "chr2"))
## autoplot(bf)
## autoplot(bf, which = c("chr1", "chr2"))
## 
## data(genesymbol, package = "biovizBase")
## autoplot(bamfile,  method = "raw", which = genesymbol["ALDOA"])
## 
## library(BSgenome.Hsapiens.UCSC.hg19)
## autoplot(bf, stat = "mismatch", which = genesymbol["ALDOA"], bsgenome = Hsapiens)


###################################################
### code chunk number 32: char-bam (eval = FALSE)
###################################################
## bamfile <- "./wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
## autoplot(bamfile)


###################################################
### code chunk number 33: char-gr
###################################################
library(rtracklayer)
test_path <- system.file("tests", package = "rtracklayer")
test_bed <- file.path(test_path, "test.bed")
autoplot(test_bed, aes(fill = name))


###################################################
###  matrix
###################################################
volcano <- volcano[20:70, 20:60] - 150
autoplot(volcano)
autoplot(volcano, xlab = "xlab", main = "main", ylab = "ylab")
## special scale theme for 0-centered values
autoplot(volcano, geom = "raster")+scale_fill_fold_change()

## when a matrix has colnames and rownames label them by default
colnames(volcano) <- sort(sample(1:300, size = ncol(volcano), replace = FALSE))
autoplot(volcano)+scale_fill_fold_change()

rownames(volcano) <- letters[sample(1:24, size = nrow(volcano), replace = TRUE)]
autoplot(volcano)

## even with row/col names, you could also disable it and just use numeric index
autoplot(volcano, colnames.label = FALSE)
autoplot(volcano, rownames.label = FALSE, colnames.label = FALSE)

## don't want the axis has label??
autoplot(volcano, axis.text.x = FALSE)
autoplot(volcano, axis.text.y = FALSE)


# or totally remove axis
colnames(volcano) <- lapply(letters[sample(1:24, size = ncol(volcano),
replace = TRUE)],
function(x){
   paste(rep(x, 7), collapse = "")
})

## Oops, overlapped
autoplot(volcano)
## tweak with it.
autoplot(volcano, axis.text.angle =  -45, hjust = 0)

## when character is the value
x <- sample(c(letters[1:3], NA), size = 100, replace = TRUE)
mx <- matrix(x, nrow = 5)
autoplot(mx)
## tile gives you a white margin
rownames(mx) <- LETTERS[1:5]
autoplot(mx, color = "white") 
colnames(mx) <- LETTERS[1:20]
autoplot(mx, color = "white")
autoplot(mx, color = "white", size = 2)
## weird in aes(), though works
## default tile is flexible
autoplot(mx, aes(width = 0.6, height = 0.6))
autoplot(mx, aes(width = 0.6, height = 0.6), na.value = "white")
autoplot(mx,  aes(width = 0.6, height = 0.6)) + theme_clear()

###################################################
### Views
###################################################
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- dnorm(1:5e3, mean = 1e3, sd = 200)
xRle <- Rle(xVector)
v1 <- Views(xRle, start = sample(.4e3:.6e3, size = 50, replace = FALSE), width =1000)
autoplot(v1)
names(v1) <- letters[sample(1:24, size = length(v1), replace = TRUE)]
autoplot(v1)
autoplot(v1, geom = "tile", aes(width = 0.5, height = 0.5))
autoplot(v1, geom = "line")
autoplot(v1, geom = "line", aes(color = row)) + theme(legend.position = "none")
autoplot(v1, geom = "line", facets = NULL)
autoplot(v1, geom = "line", facets = NULL, alpha  = 0.1)


###################################################
### ExpressionSet
###################################################
library(Biobase)
data(sample.ExpressionSet)
sample.ExpressionSet
set.seed(1)
## select 50 features
idx <- sample(seq_len(dim(sample.ExpressionSet)[1]), size = 50)
eset <- sample.ExpressionSet[idx,]
eset
autoplot(as.matrix(pData(eset)))

## default heatmap
p1 <- autoplot(eset)
p2 <- p1 + scale_fill_fold_change()
p2
autoplot(eset)
autoplot(eset, geom = "tile", color = "white", size = 2)
autoplot(eset, geom = "tile", aes(width = 0.6, height = 0.6))

autoplot(eset, pheno.plot = TRUE)
idx <- order(pData(eset)[,1])
eset2 <- eset[,idx]
autoplot(eset2, pheno.plot = TRUE)

## parallel coordainte plot
autoplot(eset, type = "pcp")

## boxplot
autoplot(eset, type = "boxplot")


## scatterplot.matrix
## slow, be carefull
autoplot(eset[, 1:7], type = "scatterplot.matrix")

## mean-sd
autoplot(eset, type = "mean-sd")


## volcano
autoplot(eset, type = "volcano", fac = pData(sample.ExpressionSet)$type)


###################################################
### SummarizedExperiment
###################################################
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
autoplot(sset) + scale_fill_fold_change()
autoplot(sset, pheno.plot = TRUE)


###################################################
### pcp
###################################################
autoplot(sset, type = "pcp")


###################################################
### boxplot
###################################################
autoplot(sset, type = "boxplot")


###################################################
### scatterplot matrix
###################################################
autoplot(sset, type = "scatterplot.matrix")


###################################################
### vcf
###################################################
library(VariantAnnotation)
vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(vcffile, "hg19")
## default use type 'geno'
## default use genome position
autoplot(vcf)
## or disable it
autoplot(vcf, genome.axis = FALSE)
## not transpose
autoplot(vcf, genome.axis = FALSE, transpose = FALSE, rownames.label = FALSE)
autoplot(vcf)
## use
autoplot(vcf, assay.id = "DS")
## equivalent to 
autoplot(vcf, assay.id = 2)
\dontrun{
## doesn't work when assay.id cannot find
autoplot(vcf, assay.id = "NO")
}
## use AF or first
autoplot(vcf, type = "info")
## geom bar
autoplot(vcf, type = "info", aes(y  = THETA))
autoplot(vcf, type = "info", aes(y  = THETA, fill = VT, color = VT)) 
autoplot(vcf, type = "fixed")
autoplot(vcf, type = "fixed", size = 10) + xlim(c(50310860, 50310890)) + ylim(0.75, 1.25)

p1 <- autoplot(vcf, type = "fixed") + xlim(50310860, 50310890) 
p2 <- autoplot(vcf, type = "fixed", full.string = TRUE) + xlim(50310860, 50310890)
tracks("full.string = FALSE" = p1, "full.string = TRUE" = p2)+
  scale_y_continuous(breaks = NULL, limits = c(0, 3))
p3 <- autoplot(vcf, type = "fixed", ref.show = FALSE) + xlim(50310860, 50310890) +
    scale_y_continuous(breaks = NULL, limits = c(0, 2))
p3


###################################################
### code chunk number 56: bs-v
###################################################
library(BSgenome.Hsapiens.UCSC.hg19)
data(genesymbol, package = "biovizBase")
p1 <- autoplot(Hsapiens, which = resize(genesymbol["ALDOA"], width = 50))
p2 <- autoplot(Hsapiens, which = resize(genesymbol["ALDOA"], width = 50), geom = "rect")
tracks(text = p1, rect = p2)


###################################################
### code chunk number 57: sessionInfo
###################################################
sessionInfo()
}
