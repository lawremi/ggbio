## ======================================================================
##        autoplot,BigWigFile (Coverage or other genomic vector)
## ======================================================================

## Should we implement methods for autoplot, or make a bioplot?

## In favor of autoplot:
## - Integrates with ggplot2 platform
## - 

## In favor of bioplot:
## - No need for methods; everything delegates to helpers
## - Cleanly supports biological interpretations of e.g. matrix and string
##   - But we should expect user to cast to high-level class
## - Does not step on existing (deprecated) autoplot methods
##   - Only a shor-term gain
## - Does not step on other packages based on ggplot2
##   - How likely is this though?
## - Branding; better name than autoplot

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

normArg_query <- function(query) {
  if (!isSingleString(query))
    stop("'query' must be a single, non-NA string")
  
}

##' A modular approach to autoplotting. The idea is to rely on S4
##' generics for determining the appropriate plot components from the
##' type of object.
##'
##' A ggplot2 plot consists of a series of layers. The layer holds the
##' data, as well as the geom, stat, position and all related
##' parameters. Other things, like the scales, labels, faceting and
##' coordinate system transcend layers. 
##'
##' It would be possible to create a LazyLayer object that queries the
##' data source whenever the plot is rendered. The result of the query
##' would be a GRanges or something else that is obviously
##' fortified.
##'
##' In a call to autoplot, there is conceptually a *single* layer. This
##' means a single statistical reduction, which is the query. The geoms
##' may be complex. We allow a *single* level of nesting (like
##' GRangesList). And custom geoms in the list compile to a *single*
##' ggplot2 geom. There is a *single* position that is applied to the
##' compound geom. Layers are compiled based on the geom, and the
##' compilation involves all necessary munging. The compiler iterates
##' over a compound geom. For each atomic geom, it extracts a subset of
##' the data, and a layer is formed from the subset and the
##' corresponding ggplot2 geom. This typically involves a molding of
##' the subset, to match things up with the ggplot2 geom.
##' 
##' Consider the complex example of drawing transcript structures: The
##' geom is compound, at least consisting of a different geom for the
##' exons and introns. The position is custom: dodge (stepping). The
##' stat is the query of the data source to yield a GRanges, with a
##' grouping variable by transcript. The transcript geom does no
##' molding. The position is a scalar, so it is applied only at the
##' top-level. The GRanges with Y adjustment is passed down to the
##' atomic phase. The stat/position are identity, and there are two
##' geoms, say rect and chevron. The chevron is a custom geom, there
##' is a mold() applied so that the segment geom draws chevrons.
##' 
##' Geoms could also be dynamic, in that they would generate a
##' different set of moldings and low-level geoms depending on the
##' amount of data to display. But we will defer that until
##' later. Probably would be based on xlim/ylim, rather than the coord
##' limits, which would be more like a physical zoom.
##'  
##' Once we have the layers, they are combined with the the scales,
##' labels, guides, facetting, coordinate system and theme into a
##' final plot. All of these are automatically chosen based on the
##' type of object.
##'
##' Overall flow of pipeline:
##' Data =query=> =position=> =split=>
##' `-> =mold1=> df1 [=scales=> =stat1=> =coord=> =geom1=\        ]
##' `-> =mold2=> df2 [=scales=> =stat2=> =coord=> =geom2=/`-> plot]
##' Stuff inside [ ] is implemented by ggplot2.
##' 
##' @title Modular autoplot
##' @param object The object to display.
##' @param query A Query object that accesses a data source and
##' reduces the data to yield one or more data.frames. This can
##' involve a significant amount of processing and is a large
##' determinant of the plot itself. Each data.frame is drawn by a
##' separate layer (geom).
##' @param geom Draws the data based on some geometry.  This is the
##' primary way to indicate the plot type. Default chosen based on the
##' query. Note that multiple geoms can be passed, resulting in one
##' layer per geom.
##' @param mapping User-specified aesthetic mappings that connect
##' geometric parameters with variables in the data.  These override
##' the defaults and are plot-wide, even when there are multiple layers.
##' @param position Position adjustment for overlapping
##' geometry. Default indicated by the geom.
##' @param facets Faceting into small multiple plots. Default chosen
##' based on the query. Typically need to facet by sequence.
##' @param scales One or more scales, each one of which scales the
##' data to one particular aesthetic.
##' @param xlim x limits, typically a GRanges
##' @param ylim y limits
##' @param main title
##' @param xlab x axis label
##' @param ylab y axis label
##' @param ... Parameters that apply to the mapping, geom, stat, and
##' query, in order.
##' @param aes the result of merging the user mappings with the
##' default aesthetics; other argument defaults depend on this.
##' @return GGbio plot object
##' @author Tengfei Yin
.autoplot_default <- function(object,
                              query=autoquery(object),
                              geom=autogeom(query),
                              mapping=NULL,
                              position=autoposition(geom),
                              facets=autofacets(query),
                              xlim=c(NA_real_, NA_real_),
                              ylim=c(NA_real_, NA_real_),
                              main=NULL,
                              xlab=xlab(aes), ylab=ylab(aes),
                              ...,
                              aes=merge(mapping, autoaes(query)))
{
  query <- normArg_query(query)
  geom <- normArg_geom(geom)
  mapping <- normArg_mapping(mapping)
  facets <- normArg_facets(facets)
  
  layers <- TODO # TODO
  scales <- autoscales(query, aes)
  coord <- autocoord(query)
  
  p <- ggplot() + layers + facets + scales + coord
  
  if (!is.null(main)) {
    p <- p + ggtitle(main)
  }
  if (!missing(xlab)) {
    p <- p + xlab(xlab)
  }
  if (!missing(ylab)) {
    p <- p + ylab(ylab)
  }
  if (!missing(xlim)) {
    p <- p + xlim(xlim)
  }
  if (!missing(ylim)) {
    p <- p + ylim(ylim)
  }
  
  p
}

## So that people can easily add just a layer...
autolayer <- function(object,
                      query=autoquery(object),
                      geom=autogeom(query),
                      mapping=NULL,
                      position=autoposition(geom))
{
  
}

setGeneric("autolayer", function(object, ...) standardGeneric("autolayer"))

setGeneric("autoquery",
           function(object, ...) standardGeneric("autoquery"))

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

setClass("CompoundQuery", contains="SimpleList",
         prototype=prototype(elementType="Query"))

setGeneric("query_aggregate",
           function(x, binwidth = 1L, ...) standardGeneric("query_aggregate"))

query_aggregate <- new("QueryAggregate", query_aggregate)

setMethod("autoquery", "BigWigFile", function(object) query_aggregate)

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
## internals, perhaps the autogeom generic should return the geom
## name. This is how the user would provide it anyway.

## It is then not clear:
## (a) how to obtain default stat/position/aes for geom AND query
##     - autostat, etc are generics dispatching on query,
##       methods take geom (name) as argument
##     - introduce our own class hierarchy of geoms, with auto*
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

setGeneric("autogeom", function(x, ...) standardGeneric("autogeom"))
setMethod("autogeom", "QueryAggregate", function(x) new("GeomBar"))

setGeneric("autostat", function(query, geom, ...)
           standardGeneric("autostat"))

layer <- function(geom, ...) {
  fun <- match.fun(paste0("geom_", names(geom)))
  fun(...)
}

ggplot2_geom <- function(geom) {
  layer(geom)$geom
}

autostat_for_geom <- function(geom) {
  ggplot2_geom(geom)$autostat()
}

setMethod("autostat", c("ANY", "ANY"),
          function(query, geom) autostat_for_geom(geom))
setMethod("autostat", c("QueryAggregate", "GeomBar"),
          function(query, geom) "identity")

autoposition_for_geom <- function(geom) {
  ggplot2_geom(geom)$autoposition()
}

setGeneric("autoposition", function(query, geom, ...)
           standardGeneric("autoposition"))
setMethod("autoposition", c("ANY", "ANY"),
          function(query, geom) autoposition_for_geom(geom))

autoaes_for_geom <- function(geom) {
  ggplot2_geom(geom)$autoaes()
}

setGeneric("autoaes", function(query, geom, ...)
           standardGeneric("autoaes"))
setMethod("autoaes", c("ANY", "ANY"),
          function(query, geom) autoaes_for_geom(geom))

setGeneric("autoscales", function(query, mapping, ...)
           standardGeneric("autoscales"),
           signature = "query")
setMethod("autoscales", "ANY", function(query, mapping) list())

setGeneric("autocoord", function(query) standardGeneric("autocoord"))
setMethod("autocoord", "ANY", function(query) coord_cartesian())

## It makes sense for the type of guide to depend on the scale. The
## user can adjust the guide separately from the scale via the
## guides() function. It seems safest to control this via guides(),
## even though that applies to all layers, since it is relatively
## uncommon to have multiple layers.

## fixes stuff like having redudant guides when a variable is mapped to 'text'
## (the legend will just show the text)
tweak_guides <- function(mapping) {
  
}

## Putting the pieces together: Layer objects

## ISSUE: Is 'stat' largely redundant with the query?

## The query often *is* a reduction, like a stat, but it also involves
## a lot of munging and restriction, in addition to summarization. In
## terms of the API, it is easiest for the user to just specify the
## query, instead of a stat. RESOLUTION: drop the stat?

## IMPORTANT SIMPLIFICATION:


setOldClass(c("Layer", "proto", "environment"))

QueryLayer <- function(query, geom, aes, stat, position, ...) {
  layer <- layer(geom, mapping=aes, stat=stat, position=position, ...)
  new("QueryLayer", layer, query=query)
}

setClass("QueryLayer", representation(query="Query"), contains="Layer")

setClass("LayerList", contains="SimpleList",
         prototype=prototype(elementType="Layer"))

LayerList <- function(...) {
  new("LayerList", SimpleList(...))
}

mlayer <- function(query, geom, aes, stat, position) {
  if (is(query, "List"))
    query <- as.list(query)
  else query <- list(query)
  if (is(geom, "List"))
    geom <- as.list(geom)
  else geom <- list(geom)
  aes <- list(aes)
  LayerList(mapply(QueryLayer, query, geom, aes, stat, position,
                   SIMPLIFY=FALSE))
}

