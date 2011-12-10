## setGeneric("ggplot", function(data, ...) standardGeneric("ggplot"))
## setMethod("ggplot", "data.frame", function(data, mapping=aes(), ...,
##                                            environment = globalenv()){
##   ggplot2::ggplot.data.frame(data = data, mapping = mapping, ...,
##                              environment = environment)
## })

## setMethod("ggplot", "IRanges", function(data, mapping=aes(),
##                                         ..., environment = globalenv()){
##   ## transform IRanges to a data frame
##   df <- as.data.frame(data)
##   p <- ggplot(df, mapping = mapping, ..., environment = environment)
##   p$data.ori <- data
##   p
## })


## StatStepping <- proto(Stat, {
##   ## informed <- FALSE
##   calculate_groups <- function(.) NULL
##   calculate <- function(.) addStepping
##   default_aes <- function(.) aes(xmin = start,
##                                  xmax = end,
##                                  ymin = ..Stepping.. - 0.4,
##                                  ymax = ..Stepping.. + 0.4)
##   default_geom <- function(.) GeomRect
##   objname <- "stepping"
## })


## StatCoverage <- proto(Stat,{
##   informed <- FALSE
##   objname <- "coverage"
##   ## calculate_groups <- function(., data, ...) {
##   ##   .$informed <- FALSE
##   ##   .super$calculate_groups(., data, ...)
##   ## }  
##   calculate <- function(., data,...){
##     if(!.informed)
##       message("stat_coverage:")
##     ## browser()
##     ## cv <- coverage(.$data.ori)
##     ## vals <- as.numeric(cv)
##     ## seqs <- seq.int(from = st, length.out = length(vals))
##     data.frame(xx = 1:3, yy = 2:4)
##   } 
##   default_aes <- function(.) aes(x = ..xx.., y = ..yy..)
##   ## required_aes <- c("x", "y")
##   default_geom <- function(.) GeomLine
## })

## str(stat_bin())
## stat_coverage <- StatCoverage$build_accessor()
## str(stat_coverage())


## ir <- IRanges(start = c(1:4), width = 2)
## p <- ggplot(ir)
## str(p)
## p + stat_coverage()

## p <- ggplot(as.data.frame(ir))
## p + geom_point(aes(x = start, y = width))

## setMethod("ggplot", "GRanges", function(data = NULL, ...){
  
## })

## ## IRanges is probably the most easy one
## ## let's started with this



