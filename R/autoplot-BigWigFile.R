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
##' and is a large determinant of the plot itself. Note that multiple
##' queries should be supported, resulting in multiple geoms.
##' @param geom Draws the data based on some geometry.  This is the
##' primary way to indicate the plot type. Default chosen based on the
##' object. Note that multiple geoms can be passed, resulting in one
##' layer per geom.
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
                              position=default_position(geom),
                              facets=default_facets(query),
                              xlim=c(NA_real_, NA_real_),
                              ylim=c(NA_real_, NA_real_),
                              main=NULL, xlab=NA_character_, ylab=NA_character_,
                              ...)
{
  query <- normArg_query(query)
  labs <- default_labs(mapping, main=main, xlab=xlab, ylab=ylab)
}

normArg_query <- function(query) {
  if (!isSingleString(query))
    stop("'query' must be a single, non-NA string")
  
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

setGeneric("default_query",
           function(object, ...) standardGeneric("default_query"))

## Design of query objects: Instead of having a special class for
## every query type and data source, we could have a generic (eval) that
## dispatches on a "query type" object and the data source.

## The query is just a function, usually a generic that
## dispatches on the object type. This makes it super easy to write
## new/optimized queries. We could store the parameters by storing a
## prototypical call to the function. When plot parameters are
## changed (like the xlim), we override the parameters in the call.

## What sort of API should be enforced for the callback, i.e., which
## parameters will the user be able to adjust? The 'xlim' is obvious.
## Could have a queryParam()<- accessor to change specific parameters
## of the query.  Or maybe the ggplot2 way is: p +
## query(binwidth=15). Either way, the xlim<- and ylim<- should
## attempt to change the corresponding parameter in the query.

## But dispatching to choose the right geom (and facets) will not work
## with a plain function, so we need a special class.

## TO TENGFEI: feel free to email me and tell me I'm crazy

setClass("Query", contains = "standardGeneric")

setClass("QueryAggregate", contains = "Query")

setGeneric("query_aggregate",
           function(x, binwidth = 1L, ...) standardGeneric("query_aggregate"))

query_aggregate <- new("QueryAggregate", query_aggregate)

setMethod("default_query", "BigWigFile", function(object) query_aggregate)

setMethod("query_aggregate", "BigWigFile",
          function(x, binwidth = 1L, xlim = seqinfo(x)) {
            xlim <- normArg_xlim_GRanges(xlim)
            if (binwidth == 1L) {
              import(object, which=xlim, ...)
            } else {
              size <- round(width(xlim) / binwidth)
              summary(object, size=size, which=xlim, ...)
            }
          })


## Choosing a geom based on a query.

## The challenge: how to represent a geom? The ggplot2 API hides the
## notion of a geom being an object. Instead, each geom has a geom_[x]
## function that constructs a *layer* containing that geom. The hidden
## geom object is a factory of the layer. The user typically
## communicates the geom by its name (like in qplot). The name is
## looked up against all Geom* symbols, going up from the ggplot2
## namespace to the global environment. To avoid dealing with ggplot2
## internals, perhaps the default_geom generic should return the geom
## name. This is how the user would provide it anyway.

## It is then not clear:
## (a) how to obtain default stat/position/aes for geom AND query
##     - default_stat, etc are generics dispatching on query,
##       methods take geom (name) as argument
##     - introduce our own class hierarchy of geoms, with default_*
##       dispatching on both the geom and request
## (b) how to look up the geom by name without using internals
##     - Reimplement search for GeomX and call $New?
##     - Find geom_X function and call it to make the layer?
##     - Search for our own GeomX class?

setClass("Geom")
setClass("GeomBar", contains="Geom")
setClass("GeomPoint", contains="Geom")

camelToUnderscore <- function(x) {
  sub("^_", "", tolower(gsub("([A-Z])", "_\\1", x)))
}

setMethod("names", "Geom", function(x) {
  camelToUnderscore(sub("^Geom", "", class(x)))
})

setGeneric("default_geom", function(x, ...) standardGeneric("default_geom"))
setMethod("default_geom", "QueryAggregate", function(x) new("GeomBar"))

setGeneric("default_stat", function(query, geom, ...)
           standardGeneric("default_stat"))

layer <- function(geom, ...) {
  fun <- match.fun(paste0("geom_", names(geom)))
  fun(...)
}

ggplot2_geom <- function(geom) {
  layer(geom)$geom
}

default_stat_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_stat()
}

setMethod("default_stat", c("ANY", "ANY"),
          function(query, geom) default_stat_for_geom(geom))
setMethod("default_stat", c("QueryAggregate", "GeomBar"),
          function(query, geom) "identity")

default_position_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_position()
}

setGeneric("default_position", function(query, geom, ...)
           standardGeneric("default_position"))
setMethod("default_position", c("ANY", "ANY"),
          function(query, geom) default_position_for_geom(geom))

default_mapping_for_geom <- function(geom) {
  ggplot2_geom(geom)$default_aes()
}

setGeneric("default_mapping", function(query, geom, ...)
           standardGeneric("default_mapping"))
setMethod("default_mapping", c("ANY", "ANY"),
          function(query, geom) default_mapping_for_geom(geom))
