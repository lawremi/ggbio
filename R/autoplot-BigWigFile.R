## ======================================================================
##        autoplot,BigWigFile (Coverage or other genomic vector)
## ======================================================================

normArg_xlim <- function(xlim) {
  ans <- try(as(xlim, "GRanges"), silent=TRUE)
  if (is(ans, "try-error"))
    stop("'xlim' must be coercible to a GRanges")
  ans
}

normArg_xlab <- function(xlab) {
  if (!isSingleString(xlab))
    stop("'xlab' must be a single, non-NA string")
  xlab
}

normArg_ylab <- function(ylab) {
  if (!isSingleString(ylab))
    stop("'ylab' must be a single, non-NA string")
  ylab
}

normArg_main <- function(main) {
  if (!isSingleString(main))
    stop("'main' must be a single, non-NA string")
  main
}

## TO TENGFEI: I noticed that there was a lot of code duplication in
## the autoplot methods. Mostly revolving around the trivial tweaks
## like main, xlab, etc. Perhaps this would make things easier? Notice
## that the user can just pass all aesthetics as the 'mapping'
## argument, so there is no need for argument parsing anymore (?)

## FIXME: do we really need this, or could every autoplot method
## reduce to a GRanges and delegate to autoplot,GenomicRanges?
setAutoplotMethod <- function(signature, definition, ...) {
  wrapper <- eval(substitute({
    function(object, mapping=NULL, xlim=seqinfo(object), ylim, main="",
             xlab="position", ylab="", ...)
      {
        ## FIXME: add the call quoting (GGbio@cmd) stuff here?
        xlim <- normArg_xlim(xlim)
        xlab <- normArg_xlab(xlab)
        ylab <- normArg_ylab(ylab)
        main <- normArg_main(main)
        .autoplot <- ..DEFINITION..
        p <- .autoplot(object, mapping=mapping, xlim=xlim, ...)
        ## FIXME: this is unnecessary for methods that wrap other
        ## autoplot methods (like the BigWigFile one below). Could
        ## control this by a parameter, or maybe it does not matter?
        if(!missing(xlab))
          p <- p + ggplot2::xlab(xlab)
        if(!missing(ylab))
          p <- p + ggplot2::ylab(ylab)
        if(!missing(main))
          p <- p + labs(title = main)
        p
      }
  }, list(..DEFINITION.. = definition)))
  setMethod("autoplot", signature, wrapper, ...)
}

## TO TENGFEI: this is my untested attempt at a BigWigFile method.  I
## think I need to add code to enable dynamic which-based evaluation
## via GGbio@cmd. Perhaps the boilerplate code for that could be
## inserted by the setAutoplotMethod() function above?

setMethod("crunch", "BigWigFile",
          function(object, which=seqinfo(object), binwidth=NA, ...)
          {
            if (is.na(binwidth)) {
              import(object, ...)
            } else {
              size <- round(width(which) / binwidth)
              summary(object, size=size, ...)
            }
          })

getGeomConstructor <- function(name) {
  getGeneric(paste0("geom_", name))
}

setAutoplotMethod("autoplot", "BigWigFile",
                  function(object, mapping=NULL, geom=c("bar", "line"), 
                           xlim=seqinfo(object), ...,
                           binwidth=NA)
                  {
                    gr <- crunch(object, binwidth=binwidth, which=xlim)
                    Geom <- getGeomConstructor(match.arg(geom))
### FIXME: There is an undesirable redundancy here: the GGbio object
### has the data, but we pass it again to the geom constructor. We
### should probably get away from having geom generics and methods:
### all we really want is a way to generate default aesthetics based
### on the geom and the type of data. This could be done via a generic
### like 'default_aes' that dispatches on the data inside GGbio and
### the geom class. One pain point is that ggplot2 geoms do not have a
### meaningful class attribute. Thus, we need to get the 'objname'
### property and map it to an S4 class within ggbio.
                    ggplot(gr) + Geom(mapping, object, ...)
                  })

## TO TENGFEI: maybe this all points to a more modular approach: there
## is one basic autoplot method that delegates to generics, i.e., the
## "strategy" design pattern.

##' A modular approach to autoplotting. The idea is to rely on S4
##' generics for determining the appropriate plot components from the
##' type of object.
##'
##' The process begins by crunch()ing the object to a canonical
##' GRanges.  The crunch() stage should only load/process the data
##' necessary for the plot. This requires figuring out from each
##' component which variables are needed, and then communicating that
##' somehow, along with the range of interest (if any), to the crunch
##' function.
##'
##' Besides the GRanges, each plot will need a set of layers, along
##' with scales, labels and a facetting specification. Each layer
##' needs a particular geom, stat and position. Layers can be
##' constructed from either the geom or stat perspective, but usually
##' layers are constructed by calling a "geom" constructor (honestly,
##' stats implying geoms never really made sense to me). Therefore, we
##' assume the user selects a geom (or accepts the default) and the
##' stat and position are selected based on the geom. Multiple
##' geom/stat/position objects can be passed (as lists or character
##' vectors of names). They are treated as parallel vectors for
##' constructing a list of layers. The layer() generic dispatches on
##' the data and geom, and is also passed the position and any
##' relevant parameters from "...".
##'
##' Once we have the layers, they are combined with the the scales,
##' labels and facetting into a final plot.
##' 
##' @title Modular autoplot
##' @param object The object to display.
##' @param geom Draws the data based on some geometry.  This is the
##' primary parameter that determines the plot. Default chosen based
##' on the object.
##' @param mapping Aesthetic mappings that connect geometric
##' parameters with variables in the data. Choosing a default depends
##' on the object and the geometry. Note that these apply *after* the
##' statistical transformation, and any other preprocessing. If the
##' user overrides this parameter, it is merged with the defaults.
##' @param stat Statistical transformation of the data. This is often
##' required for the data to fit a particular geometry.  The default
##' then depends on both the object and the geometry. User rarely
##' overrides this one.
##' @param position Position adjustment for overlapping
##' geometry. Default chosen according to object and geometry.
##' @param facets Faceting into small multiple plots. Default chosen
##' based on the object. Typically need to facet by sequence.
##' @param scales Parameterizations of the aesthetic mappings. Most
##' important aspects are the limits and guide labels. Default scales
##' depend on the object, geometry and the mappings, as well as user
##' limit overrides.
##' @param labs Determines default axis labels based on the mapping
##' and the user-specified overrides.
##' @param xlim x limits, typically a GRanges
##' @param ylim y limits
##' @param main title
##' @param xlab x axis label
##' @param ylab y axis label
##' @param ... Parameters that apply to the mapping, geom, stat, and
##' crunch, in order.
##' @return GGbio plot object
##' @author Tengfei Yin
.autoplot_default <- function(object,
                              geom=default_geom(object),
                              mapping=default_aes(object, geom), 
                              stat=default_stat(object, geom),
                              position=default_position(object, geom),
                              facets=default_facets(object),
                              scales=default_scales(object, geom, mapping,
                                xlim=xlim, ylim=ylim),
                              labs=default_labs(mapping, main=main, xlab=xlab,
                                ylab=ylab),
                              xlim=c(NA_real_, NA_real_),
                              ylim=c(NA_real_, NA_real_),
                              main=NULL, xlab=NA_character_, ylab=NA_character_,
                              ...)
{
  
}



## TO TENGFEI: feel free to email me and tell me I'm crazy

## TO TENGFEI: maybe the stat_ generics should have a more ggplot2-like
## argument set, like this:
setGeneric("stat_aggregate2",
           function(mapping = NULL, data = NULL, geom = "histogram",
                    position = "stack", ...) standardGeneric("stat_aggregate2"),
           signature="data")

setGeneric("geom_bar2",
           function(mapping = NULL, data = NULL, stat = "bin",
                    position = "stack", ...) standardGeneric("geom_bar2"),
           signature="data")

