\name{qplot}

\alias{qplot}
\alias{qplot,data.frame-method}
\alias{qplot,matrix-method}
\alias{qplot,GRanges-method}
\alias{qplot,GRangesList-method}
\alias{qplot,IRanges-method}
\alias{qplot,BSgenome-method}
\alias{qplot,GappedAlignments-method}
\alias{qplot,BamFile-method}
\alias{qplot,TranscriptDb-method}
\alias{qplot,character-method}

\title{Generic qplot function}

\description{
  To visualize different objects describing biological data,
  we develop this generic function, and developed new types of geoms to
  each one. Try to make simple API and following the grammar of graphics,
  use higher level graphic package like ggplot2 to produce high quality
  graphics.
}

\usage{
## For object data.frame
\S4method{qplot}{data.frame}(data, ...)

## For object matrix
\S4method{qplot}{matrix}(data, ...)

## For object GRanges
\S4method{qplot}{GRanges}(data, x, y,...,
      facet_gr, model, group.name,
      legend = TRUE, ignore.strand = TRUE,
      show.coverage = TRUE,
      geom = c("full", "line","point",
               "segment", "histogram", "reduce", "disjoin",
               "coverage.line", "coverage.polygon", "splice",
               "mismatch.summary"))


## For object GRangesList
\S4method{qplot}{GRangesList}(data, ...)

## For object IRanges
\S4method{qplot}{IRanges}(data, ..., legend = TRUE,
      geom = c("full", "segment", "histogram",
               "coverage.line", "coverage.polygon", "reduce"))

## For object GappedAlignments
\S4method{qplot}{GappedAlignments}(data, ..., which,
      geom = c("gapped.pair", "full"),
      show.junction = FALSE)

## For object BamFile
\S4method{qplot}{BamFile}(data, ..., which, model,
      bsgenome, resize.extra = 10, show.coverage = TRUE,
      geom = c("gapped.pair", "full",
               "fragment.length", "coverage.line",
               "coverage.polygon", "mismatch.summary"))

## For object character
\S4method{qplot}{character}(data, x, y, ..., which, model,
      bsgenome, resize.extra = 10, show.coverage = TRUE,
      geom = c("gapped.pair", "full", "fragment.length",
               "coverage.line", "coverage.polygon",
               "mismatch.summary"))

## For object TranscriptDb
\S4method{qplot}{TranscriptDb}(data, x, y, which, ...,
      geom = c("full", "single"))

## For object BSgenome
\S4method{qplot}{BSgenome}(data, name, ...,
      geom = c("text", "segment", "point", "rectangle"))
}

\arguments{
  \item{data}{
    A \code{\link{data.frame}}, \code{\link{matrix}},
    \code{\link{GRanges}}, \code{\link{BSgenome}},
    \code{\link{TranscriptDb}}, \code{\link{GappedAlignments}} object
    or any other objects for which the qplot method is defined.    
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
  \item{facet_gr}{
    A \code{\link{GRanges}} object which contains the regions you want
  to facet on. 
  }
  \item{model}{
    A \code{\link{GRanges}} object which providing a known or customized
    genomic structure or gene model.
  }
  \item{group.name}{
    A character with quote, one of the variables in elementMetadata,
    used for grouping the intervals when computing the splice summary.
    e.g. grouping exons into one transcript model by calling
    \code{group.name = "tx\_ed"}
  }
  \item{name}{
    Passed to \code{getSeq} in BSgenome package.
    
    A character vector containing the names of the sequences in
    'x' where to get the subsequences from, or a GRanges object,
    or a RangedData object, or a named RangesList object, or a
    named Ranges object.  The RangesList or Ranges object must be
    named according to the sequences in 'x' where to get the
    subsequences from.

    If 'names' is missing, then 'seqnames(x)' is used.
  }
  \item{legend}{
    A logical value indicates whether to show legend or not. Default is
    \code{TRUE}
  }
  \item{ignore.strand}{
    A logical value indicates whether to ignore the strand or
    not. Usually used in geom \code{reduce} and \code{disjoin}.
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
  \item{...}{
    Extra parameters. Usually are those parameters used in qplot to
    control aesthetics or geometries.
  }
}

\value{
  A \code{ggplot} object, so you can use common features from ggplot2
  package to manipulate the plot.

  For fragment.length geom, it's kind of special, it returns tracks not
  single \code{ggplot} object.
}

\section{Introduction}{
  \code{qplot} is redefined as generic s4 method inside this package,
  user could use \code{qplot} in the way they are familiar with, and
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
  just use formal \code{qplot} function in ggplot2, or \code{qplot}
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
  \item{histogram}{
    Showing interval data as histogram. Other parameters like
    \code{binwidth} will be passed to qplot function for delicate
    control. This essential compute the coverage for interval data first
    then bin the coverage into histogram.
  }
  \item{coverage.line}{
    Coverage showing as lines for interval data.
  }
  \item{coverage.polygon}{
    Coverage showing as polygon for interval data.
  }
  \item{reduce}{
    Run \code{\link{reduce}} for the interval data first, then show them
  as rectangle. User could also just reduce the data by themselves and
  then use \code{qplot} to show as different geom,
  e.g. segment. \code{ignore.strand} could be provided.
  }
  \item{disjoin}{
    Run \code{\link{disjoin}} for the interval data first, then show them
  as rectangle. User could also just reduce the data by themselves and
  then use \code{qplot} to show as different geom,
  e.g. segment. \code{ignore.strand} could be provided.
  }
  \item{splice}{
    Splicing summary. The size and width of the line and rectangle
    should represent the counts in each model. Need to provide model.
  }
  \item{single}{
    For \code{TranscripDb} object, showing single(reduced) model only.
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
  \item{fragment.length}{
    Instead of return a single plot. This geom return tracks including
  gene model, reduced model, splicing summary, fragment length
  estimation.
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
## sample data
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
## subset
gr.sub <- gr[seqnames(gr) == "chr1"] #or 
## common geom
p1 <- qplot(gr.sub, geom = "full") + opts(title = "full")
p2 <- qplot(gr.sub, geom = "point", y = value) +
                                  opts(title = "point")
p3 <- qplot(gr.sub, geom = "line", y = value) +
                                  opts(title = "line")
p4 <- qplot(gr.sub, geom = "coverage.line") +
                                  opts(title = "coverage.line")   
p5 <- qplot(gr.sub, geom = "coverage.polygon") + 
                                  opts(title = "coverage.polygon")   
p6 <- qplot(gr.sub, geom = "reduce") + 
                                  opts(title = "reduce")   
p7 <- qplot(gr.sub, geom = "disjoin") +
                                  opts(title = "disjoin")      
p8 <- qplot(gr.sub, geom = "histogram") +
                                  opts(title = "histogram")      
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7 ,p8, ncol = 2)

## overview
data(hg19IdeogramCyto)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
p <- plotOverview(new.ideo, cytoband = TRUE)
print(p)

# no cytoband and with extra data
data(darned_hg19_subset500)
# rename 
new.darned <- renameSeqlevels(darned_hg19_subset500, new.chrs)
p <- p + geom_hotregion(new.darned)
print(p)

}

