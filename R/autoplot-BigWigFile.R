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
##' A ggplot2 plot consists of a series of layers. The layer holds the
##' data, as well as the geom, stat, position and all related
##' parameters. Other things, like the scales, labels, faceting and
##' coordinate system transcend layers. The data can be anything! But
##' before plotting, we need to convert the data to a data.frame. This
##' "fortify" step should come before everything else.
##' 
##' It would be possible to create a LazyLayer object that queries the
##' data source whenever the plot is rendered. The result of the query
##' would be a GRanges or something else that is obviously
##' fortified. The tricky part is generating the appropriate query.
##'
##' Enter autoplot(). Each object type has an autoplot method. The
##' autoplot() methods should do two things:
##' (1) initialize a means to query the data source
##' (2) generate a default plot specification
##'
##' The latter is relatively straightforward, and could be implemented
##' with S4 generics that dispatch on the data type. Generating the
##' query is more complicated, because it depends on BOTH the data AND
##' the plot, in particular the aesthetic mappings. So we will need to
##' rely on S4 dispatch again.
##' 
##' Once we have the layers, they are combined with the the scales,
##' labels and facetting into a final plot.
##' 
##' @title Modular autoplot
##' @param object The object to display.
##' @param query Queries a data source and reduces the data to yield a
##' data.frame. This can involve a significant amount of processing
##' and is a large determinant of the plot itself.
##' @param geom Draws the data based on some geometry.  This is the
##' primary way to indicate the plot type. Default chosen based
##' on the object.
##' @param mapping Aesthetic mappings that connect geometric
##' parameters with variables in the data. Note that these apply
##' *after* the statistical transformation, and any other
##' preprocessing. These are merged with the defaults from the
##' geom and thus default to NULL.
##' @param stat Statistical transformation of the data. This is often
##' required for the data to fit a particular geometry.  The default
##' is communicated by the geom. User rarely overrides this one.
##' @param position Position adjustment for overlapping
##' geometry. Default indicated by the geom.
##' @param facets Faceting into small multiple plots. Default chosen
##' based on the query. Typically need to facet by sequence.
##' @param xlim x limits, typically a GRanges
##' @param ylim y limits
##' @param main title
##' @param xlab x axis label
##' @param ylab y axis label
##' @param ... Parameters that apply to the mapping, geom, stat, and
##' query, in order.
##' @return GGbio plot object
##' @author Tengfei Yin
.autoplot_default <- function(object,
                              query=default_query(object),
                              geom=default_geom(query),
                              mapping=NULL,
                              stat=default_stat(geom),
                              position=default_position(geom)
                              facets=default_facets(query),
                              xlim=c(NA_real_, NA_real_),
                              ylim=c(NA_real_, NA_real_),
                              main=NULL, xlab=NA_character_, ylab=NA_character_,
                              ...)
{
  labs <- default_labs(mapping, main=main, xlab=xlab, ylab=ylab)
}

## So that people can easily add just a layer...
.autolayer_default <- function(object,
                               query=default_query(object),
                               geom=default_geom(query),
                               mapping=NULL,
                               stat=default_stat(geom),
                               position=default_position(geom))
{
  
}

## TO TENGFEI: feel free to email me and tell me I'm crazy

  
setGeneric("default_geom", function(data, ...) standardGeneric("default_geom"))

setMethod("default_geom", "BigWigFile", function(data) "bar")

