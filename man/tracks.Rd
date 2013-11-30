\name{tracks}
\alias{tracks}
\alias{alignPlots}
\alias{align.plots}
\alias{Tracks-class}
\alias{xlim}
\alias{xlim,Tracks-method}
\alias{xlim,numeric-method}
\alias{xlim,IRanges-method}
\alias{xlim,GRanges-method}
\alias{xlim<-}
\alias{xlim<-,Tracks,numeric-method}
\alias{xlim<-,Tracks,IRanges-method}
\alias{xlim<-,Tracks,GRanges-method}
\alias{bgColor}
\alias{bgColor,GGbio-method}
\alias{bgColor,Tracked-method}
\alias{bgColor,gg-method}
\alias{bgColor,gtable-method}
\alias{bgColor<-}
\alias{bgColor<-,GGbio,character-method}
\alias{bgColor<-,Tracked,character-method}
\alias{bgColor<-,gg,character-method}
\alias{bgColor<-,gtable,character-method}
\alias{fixed,GGbio-method}
\alias{fixed,Tracked-method}
\alias{fixed,gg-method}
\alias{fixed<-,GGbio,logical-method}
\alias{fixed<-,Tracked,logical-method}
\alias{fixed<-,gg,logical-method}
\alias{hasAxis}
\alias{hasAxis,GGbio-method}\alias{hasAxis,GGbio-method}
\alias{hasAxis,Tracked-method}
\alias{hasAxis,gg-method}
\alias{hasAxis,Tracked-method}
\alias{hasAxis,gg-method}
\alias{hasAxis<-}
\alias{hasAxis<-,GGbio,logical-method}
\alias{hasAxis<-,Tracked,logical-method}
\alias{hasAxis<-,gg,logical-method}
\alias{height}
\alias{height,GGbio-method}
\alias{height,Tracked-method}
\alias{height,gg-method}
\alias{height<-}
\alias{height<-,GGbio,numericORunit-method}
\alias{height<-,Tracked,numericORunit-method}
\alias{height<-,gg,numericORunit-method}
\alias{labeled}
\alias{labeled,GGbio-method}
\alias{labeled,Ideogram-method}
\alias{labeled,Tracked-method}
\alias{labeled,gTree-method}
\alias{labeled,gg-method}
\alias{labeled,gtable-method}
\alias{labeled,text-method}
\alias{labeled<-}
\alias{labeled<-,GGbio,logical-method}
\alias{labeled<-,Ideogram,logical-method}
\alias{labeled<-,Tracked,logical-method}
\alias{labeled<-,gg,logical-method}
\alias{labeled<-,gtable,logical-method}
\alias{mutable}
\alias{mutable,GGbio-method}
\alias{mutable,Tracked-method}
\alias{mutable,gg-method}
\alias{mutable<-}
\alias{mutable<-,GGbio,logical-method}
\alias{mutable<-,Tracked,logical-method}
\alias{mutable<-,gg,logical-method}
\alias{summary}
\alias{summary,Tracks-method}
\alias{show}
\alias{show,Tracks-method}
\alias{print}
\alias{print,Tracks-method}
\alias{Arith}
\alias{Arith,Tracks,ANY-method}
\alias{reset}
\alias{reset,Tracks-method}
\alias{backup}
\alias{backup,Tracks-method}
\alias{c,Tracks-method}
\alias{c,PlotList-method}
\alias{cbind,Tracks-method}
\alias{rbind,Tracks-method}
\alias{[,Tracks,numeric,missing-method}
\alias{[,PlotList,numeric,missing-method}
\title{Tracks for genomic graphics}
\usage{
tracks(..., heights, xlim, xlab = NULL, main = NULL,
            title = NULL, theme = NULL, 
            track.plot.color = NULL,
            track.bg.color = NULL,
            main.height = unit(2, "lines"),
            scale.height = unit(2, "lines"),
            xlab.height = unit(2, "lines"),
            padding = unit(-1, "lines"),
            label.bg.color =  "white",
            label.bg.fill = "gray80",
            label.text.color = "black",
            label.text.cex = 1,                   
            label.width = unit(2.5, "lines"))
}
\arguments{
  \item{...}{plots of class ggplot, generated from ggplot2 or ggbio.}
  \item{heights}{numeric vector of the same length of passed graphic
    object to indicate the ratio of each track.}
  \item{xlim}{limits on x. could be \code{\linkS4class{IRanges}}, \code{\linkS4class{GRanges}}, \code{numeric} value}
  \item{xlab}{label for x axis.}
  \item{main}{title for the tracks.}
  \item{title}{title for the tracks, alias like main.}
  \item{theme}{theme object used for building tracks, this will
    set to default, which could be reseted later. }
  \item{track.plot.color}{
    Vector of characters of length 1 or the same length of passed plots,
    background color for each track, default is white.
  }
  \item{track.bg.color}{
    background color for the whole tracks.
  }
  \item{main.height}{
    unit. Height to control the title track height.
  }
  \item{scale.height}{
    unit. Height to control the scale track height.
  }
  \item{xlab.height}{
    unit. Height to control the xlab track height.
  }        
  \item{padding}{
    single numeric value or unit, if numeric value, the unit would be
    "lines" by default.
  }          
  \item{label.bg.color}{
    track labeling background rectangle border color.
  }
  \item{label.bg.fill}{
    track labeling background fill color.
  }
  \item{label.text.color}{
    track labeling text color.
  }
  \item{label.text.cex}{
    track labeling text size.
  }
  \item{label.width}{
    track labeling size.
  } 
}
\section{Track class}{

  constructor \code{tracks} will return a Tracks object, which has
  following slots.
  \describe{
  \item{\code{grobs}}{
    a \code{ggplotGrobList} object contains a list of \code{ggplot}
  object, which is our passed graphics.
  }
  \item{\code{backup}}{
    a backup of all the slots for holding the original tracks, so users
  could edit it and reset it back at any time later, and \code{backup}
  method will reset the backupped copy.
  }
  \item{\code{ylim}}{
    y limits for each plot.
  }
  \item{\code{labeled}}{
    vector of logical value indicates whether a track is labeled or not,
  for \code{labeled} attributes please check utilities section.
  }
  \item{\code{mutable}}{
    vector of logical value indicates whether a track is mutable for
  theme editing or not, for \code{mutable} attributes please check utilities section.
  }
  \item{\code{hasAxis}}{
    vector of logical value indicates whether a track has axis or  not,
    for \code{hasAxis} attributes please check utilities section.
  }
  \item{\code{heights, xlim, xlab, main, title, theme, fixed, track.plot.color,
    track.bg.color, main.height, scale.height, xlab.height, padding,
    label.bg.color, label.bg.fill, label.text.color, label.text.cex, label.width}}{
    those slots are described in arguments section for constructor.
  }
  }
}

\section{Utilities}{
  Please check examples for usage.
  \describe{
    \item{\code{summary(object)}}{
      summary information about tracks object.
    }
    \item{\code{fixed(x)}, \code{fixed(x) <- value}}{ \code{x} is the
      ggplot object, this controls if a track has a fixed x scale or not, if the \code{fixed}
      attributes is \code{TRUE}, then when you pass this plot to a
      tracks, this plot won't be re-aligned with other tracks and will
      keep the original x-axis, this allow you to pass some plot like
      ideogram. \code{fixed} function will return a logical value 
    }
    \item{\code{labeled(x), labeled(x) <- value}}{\code{x} is the
      ggplot object,  if you pass named graphics into \code{tracks}, it will create the
      labels on the left for you. Several ways supported to name it. You can
      pass a list of graphics with names. Or you can use
      \code{tracks('name1' = p1, 'name 2' = p2, ...)}  with quotes for
      complicated words or simply \code{tracks(part1 = p1, part = p2, ...)}. 
    }
    \item{\code{mutable(x), mutable(x) <- value}}{
      \code{x} is the ggplot object, this controls whether a plot in the
      tracks mutable to theme changing or not, when you use \code{+}
      method for Tracks object, add-on edit will only be applied to the
      the mutable plots.
    }
    \item{\code{bgColor(x), bgColor(x) <- value}}{
      \code{x} is the ggplot object, this change the background color
      for single plot shown in the tracks.
    }
    \item{\code{xlim(x), xlim(x) <- value}}{
      when \code{x} is the numeric value, it calls
      ggplot2::coord_cartesian(xlim = ...) method, we doesn't use
      ggplot2::xlim() for the reason it will cut data outside the range,
      and we believe the best behavior would be zoom-in/out like
      most browser. when \code{x} is \code{\linkS4class{IRanges}},
      \code{\linkS4class{GRanges}}, it get the range and passed to
      ggplot2::coord_cartesian function.
      
      when \code{x} is Tracks object, \code{xlim(x)} will
      return x limits for that tracks. \code{xlim(x) <- value}
      replace method only works for Tracks object. \code{value} could be
      \code{numeric}, \code{\linkS4class{IRanges}},
	\code{\linkS4class{GRanges}} object. This will change the x
      limits associated with tracks.

      \code{+ xlim(obj)}:\code{obj} is the numeric range, or \code{\linkS4class{IRanges}},
      \code{\linkS4class{GRanges}} object.

      \code{+ coord_cartesian()}:
      please read manual in ggplot2, this controls both xlim an
      ylim, only accept numerical range.
    }
    \item{\code{+}}{
      The most nice features about \code{\linkS4class{Tracks}} object is
      the one inherited from ggplot2's components additive features,
      with \code{+} method you can use any \code{theme} object and
      utilities in ggplot2 package, to add them on a
      \code{\linkS4class{Tracks}} object, for example, if x is our
      \code{\linkS4class{Tracks}} object, \code{x + theme} would  apply
      theme to any plots in the tracks except those are immutable.
    }
  }  
}

\section{Backup and reset}{
  \describe{
    \item{reset(obj)}{
      \code{obj} is the Tracks object, this reset the tracks back to
      original or backuped version.
    }
    \item{backup(obj)}{
      \code{obj} is the Tracks object, this clear previous backup and
      use current setting for a new backup.
    }
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
}

\details{
  \code{tracks} did following modification for passed plots.
  \itemize{
    \item{}{

      remove x-axis, ticks, xlab and tile for each track and add scales
  at bottom. We suppose a new xlab and title would be provided by the
  \code{tracks} function for the whole tracks, but we still keep
  individual's y axis.
      
    }
    \item{}{
      align x-scale limits to make sure every plots sitting on exactly
      the same x scale.
    }
    \item{}{
      squeezing plots together to some extent.
    }
    \item{
      labeling tracks if names are provided, please check utilities section
      about \code{labeled} method.
    }
    \item{
      return a track object. This would allow many features introduced
      in this manual.
    }
  }
}


\seealso{\code{\link{align.plots}}}
\value{
 A \code{Tracks} object.
}
\examples{
## make a simulated time series data set
df1 <- data.frame(time = 1:100, score = sin((1:100)/20)*10)
p1 <- qplot(data = df1, x = time, y = score, geom = "line")
df2 <- data.frame(time = 30:120, score = sin((30:120)/20)*10, value = rnorm(120-30 + 1))
p2 <- ggplot(data = df2, aes(x = time, y = score)) + 
  geom_line() + geom_point(size = 4, aes(color = value))
## check p2
p1
## check p2
p2

## binding
tracks(p1, p2)

## or
tks <- tracks(p1, p2)
tks

## combine
c(tks, tks)
tks + tks

cbind(tks, tks)
rbind(tks, tks) ## different wth c()!
library(grid)
x <- ggbio:::get_gtable(tks)
grid.draw(cbind(x, x))


## labeling: default labeling a named graphic
## simply pass a name with it
tracks(time1 = p1, time2 = p2)
## or pass a named list with it
lst <- list(time1 = p1, time2 = p2)
tracks(lst)

## more complicated case please use quotes
tracks(time1 = p1, "second time" = p2)

## set heights
tracks(time1 = p1, time2 = p2, heights = c(1, 3))

## if you want to disable label arbitrarily
## default label is always TRUE
labeled(p2) 
labeled(p2) <- FALSE
## set labeled to FALSE, remove label even the plot has a name
tracks(time1 = p1, time2 = p2)
labeled(p2) <- TRUE

## fix a plot, not synchronize with other plots
p3 <- p1
## default is always FALSE
fixed(p3)
## set to TRUE
fixed(p3) <- TRUE
fixed(p3)

tracks(time1 = p1, time2 = p2, "time3(fixed)" = p3) 


fixed(p3) <- FALSE
## otherwise you could run
%% tracks(time1 = p1, time2 = p2, "time3(fixed)" = p3, fixed = c(FALSE, FALSE, TRUE))


## control axis
hasAxis(p1)
hasAxis(p1) <- TRUE
# ready for weird looking
tracks(time1 = p1, time2 = p2)
# set it back
hasAxis(p1) <- FALSE



## mutable
mutable(p1)
tracks(time1 = p1, time2 = p2) + theme_bw()
mutable(p1) <- FALSE
# mutable for "+" method
tracks(time1 = p1, time2 = p2) + theme_bw()
mutable(p1) <- TRUE

## bgColor
bgColor(p1)
tracks(time1 = p1, time2 = p2)
bgColor(p1) <- "brown"
# mutable for "+" method
tracks(time1 = p1, time2 = p2)
# set it back
bgColor(p1) <- "white"

## apply a theme to each track
tks <- tracks(time1 = p1, time2 = p2) + theme_bw()
tks
reset(tks)

## store it with tracks
tks <- tracks(time1 = p1, time2 = p2, theme = theme_bw())
tks
tks <- tks + theme_gray()
tks
## reset will be introduced later
reset(tks)

## apply a pre-defiend theme for tracks!
tracks(time1 = p1, time2 = p2) + theme_tracks_sunset()
 tracks(p1, p2) + theme_tracks_sunset()

## change limits
tracks(time1 = p1, time2 = p2) + xlim(c(1, 40))
tracks(time1 = p1, time2 = p2) + xlim(1, 40)
tracks(time1 = p1, time2 = p2) + coord_cartesian(xlim = c(1, 40))
# change y
tracks(time1 = p1, time2 = p2) + xlim(1, 40) + ylim(0, 10)
library(GenomicRanges)
gr <- GRanges("chr", IRanges(1, 40))
# GRanges
tracks(time1 = p1, time2 = p2) + xlim(gr)
# IRanges
tracks(time1 = p1, time2 = p2) + xlim(ranges(gr))
tks <- tracks(time1 = p1, time2 = p2)
xlim(tks)
xlim(tks) <- c(1, 35)
xlim(tks) <- gr
xlim(tks) <- ranges(gr)

## xlab, title
tracks(time1 = p1, time2 = p2, xlab = "time")
tracks(time1 = p1, time2 = p2, main = "title")
tracks(time1 = p1, time2 = p2, title = "title")
tracks(time1 = p1, time2 = p2, xlab = "time", title = "title") + theme_tracks_sunset()


## backup and restore
tks <- tracks(time1 = p1, time2 = p2)
tks
tks <- tks + xlim(1, 40)
tks
reset(tks)
tks <- tks + xlim(1, 40)
tks
tks <- backup(tks)
tks <- tks + theme_bw()
tks
reset(tks)

## padding(need to be fixed for more delicate control)
tracks(time1 = p1, time2 = p2, padding = 2)

## track color
tracks(time1 = p1, time2 = p2, track.bg.color = "yellow")
tracks(time1 = p1, time2 = p2, track.plot.color = c("yellow", "brown"))

}
\author{Tengfei Yin}


