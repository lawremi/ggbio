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

setMethod("crunch", "BigWigFile", function(object, nbins=NA, ...) {
  ### FIXME: faster to get as a GRanges directly, rather than asRle?
  if (is.na(nbins)) {
    import(object, ..., asRle=TRUE)
  } else {
    summary(object, size=nbins, ...)
  }
})

## TO TENGFEI: the main idea is to coerce to RleList and delegate
setMethod("autoplot", "BigWigFile",
          function(object, mapping=NULL, xlim=seqinfo(object), ..., nbins=NA) {
            object <- crunch(object, nbins=nbins, which=xlim)
            callGeneric()
          })

## TO TENGFEI: maybe the stat_ generics should have a more ggplot2-like
## argument set, like this:
setGeneric("stat_aggregate2",
           function(mapping = NULL, data = NULL, geom = "histogram",
                    position = "stack", ...) standardGeneric("stat_aggregate2"),
           signature="data")
