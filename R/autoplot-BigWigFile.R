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
          function(object, which=seqinfo(object), nbins=NA, ...)
          {
            if (is.na(nbins)) {
              import(object, ...)
            } else {
              rle.list <- summary(object, size=nbins, ...)
              which.list <- as(which, "List")
              do.call(c, mapply(function(rle, which) {
                chunks <- breakInChunks(width(which), width(which)/nbins)
                GRanges(seqnames(which),
                        IRanges(start(chunks)+start(which)-1L, width(chunks)),
                        score = rle)
              }, rle.list, which.list, SIMPLIFY=FALSE))
            }
          })

getGeomConstructor <- function(name) {
  getGeneric(paste0("geom_", name))
}

setAutoplotMethod("autoplot", "BigWigFile",
                  function(object, mapping=NULL, geom=c("bar", "line"), 
                           xlim=seqinfo(object), ...,
                           nbins=NA)
                  {
                    object <- crunch(object, nbins=nbins, which=xlim)
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
                    ggplot(object) + Geom(mapping, object, ...)
                  })

## TO TENGFEI: maybe this all points to a more modular approach: there
## is one basic autoplot method that delegates to generics, i.e., the
## "strategy" design pattern. Steps:
## 1) crunch() from complex data to interpretable summary
## 2) ggplot() to initialize plot, relies on mold() => data.frame
## 3) default_geom() chooses a default geom based on data type
##    - could be a "meta" geom for complex cases
## 4) default_aes() chooses a default set of aesthetics based on geom and data
##    - similarly, could have default_stat(), default_position() if needed

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

