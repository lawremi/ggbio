\name{Tracked-class}
\alias{Tracked-class}
\alias{bgColor}
\alias{bgColor<-}
\alias{bgColor,Tracked-method}
\alias{bgColor<-,Tracked,character-method}
\alias{fixed}
\alias{fixed<-}
\alias{fixed,Tracked-method}
\alias{fixed<-,Tracked,logical-method}
\alias{labeled}
\alias{labeled<-}
\alias{labeled,Tracked-method}
\alias{labeled<-,Tracked,logical-method}
\alias{mutable}
\alias{mutable<-}
\alias{mutable,Tracked-method}
\alias{mutable<-,Tracked,logical-method}
\alias{hasAxis}
\alias{hasAxis<-}
\alias{hasAxis,Tracked-method}
\alias{hasAxis<-,Tracked,logical-method}
\alias{height}
\alias{height<-}
\alias{height,Tracked-method}
\alias{height<-,Tracked,numericORunit-method}

\title{Tracked Class}
\description{
  This virtual class is an abstraction that describe the object designed to be
  embed into tracks. 
}
\arguments{
  Tracked(mutable = TRUE, fixed = FALSE, labeled = TRUE,
          hasAxis = FALSE, bgColor = "white", height = unit(1, "null"))
}
\section{Slots}{
  \describe{
  \item{mutable}{
    logical value, default \code{TRUE}, used for to define if an object
    could be changed and updated when embed in trakcs, especially to
    control the responsiveness to \code{+} method. 
  }
  \item{fixed}{
    logical value, default \code{FALSE}, used to control if an object
    update its x-scale when embed in tracks.
  }
  \item{labeled}{
    logical value, default \code{TRUE}, used to control if an object is
    labeled with track name or not. 
  }
  \item{hasAxis}{
    logical value, default \code{FALSE}, used to control if an object
    has axis or not when embed in tracks.
  }  
  \item{bgColor}{
    character to indicate background color, default is 'white'. 
  }  
  \item{height}{
    numeric value or unit, default is \code{1null}, used to indicate the height of object embed
    in the tracks. 
  }  
}
}
\value{
  A 'Tracked' class.
}
\seealso{
  \link{Plot-class}
}
\examples{
showClass("Tracked")

obj <- new("Tracked")
obj <- Tracked()
height(obj) <- 1
height(obj)  # return unit
mutable(obj)
fixed(obj)
labeled(obj)
hasAxis(obj)
bgColor(obj)

}
\author{Tengfei Yin}

