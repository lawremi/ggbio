\name{theme}
\alias{theme_null}
\alias{theme_noexpand}
\alias{theme_pack_panels}
\alias{theme_alignment}
\alias{theme_clear}
\alias{theme_tracks_sunset}
\alias{theme_genome}
\title{theme in ggbio}
\description{
  Theme defined in ggbio for plot or tracks. 
}
\details{Themes speciall designed for tracks, are named following naming
schema theme_tracks_*}
\usage{
  theme_null()
  theme_noexpand()    
  theme_alignment(ylabel = FALSE, base_size = 12, base_family = "", 
  axis = TRUE, border = TRUE, grid = TRUE)
  theme_pack_panels(strip.bg = FALSE, strip.text.y = TRUE)
  theme_clear(grid.y = FALSE, grid.x.minor = FALSE, grid.x.major = FALSE,
              panel.background.fill = "white", panel.border.color = NA,
              axis.ticks.x = FALSE, axis.ticks.y = TRUE, grid.color = "gray95",
              axis.line.color = "gray80")
  theme_tracks_sunset(bg = "#fffedb", alpha = 1, ...)
  theme_genome()
}
\arguments{
  
  \item{alpha}{
   alpha blending from 0(transparent) to 1(solid).
 }
  \item{axis}{
    logical value, show axis or not.
  }
 
  \item{axis.line.color}{
    color for axis line .
  }
  \item{axis.ticks.x}{
    show x ticks or not.
  }
  \item{axis.ticks.y}{
    show y ticks or not.
  }
  \item{base_family}{
    family for font.
  }
  
  \item{base_size}{
    size for font.
  }
 
  \item{bg}{
    background color for tracks.
  }
  
  \item{border}{
    logical value, show border or not.
  }
  \item{grid}{
    logical value, show background grid or not.
  }
  \item{grid.color}{
    grid line color.
  }
  \item{grid.x.major}{
    show x major grid line or not.
  }
  \item{grid.x.minor}{
    show x minor grid line or not.
  }
  \item{grid.y}{
    show y grid or not.
  }
  \item{panel.background.fill}{
    panel background fill color.
  }
  \item{panel.border.color}{
    panel border color.
  }
  \item{strip.bg}{
    if strip background is removed.
  }
  \item{strip.text.y}{
    if strip text is removed.
  }
  \item{ylabel}{
    logical value. Show labels or not.
  }
  \item{...}{
    passed to \code{theme_clear}.
  }
}
\value{
  Return a theme.
}
\examples{
##  load
library(ggbio)
p <- qplot(data = mtcars, x = mpg,  y = wt, facets = cyl ~ .)
p + theme_null()
p + theme_clear()
p + theme_pack_panels()
p + theme_alignment()
p1 <- qplot(data = mtcars, x = mpg,  y = wt)
tracks(p1 = p, p2 = p1)
tracks(p1 = p, p2 = p1) + theme_tracks_sunset()
}
\author{Tengfei Yin}

