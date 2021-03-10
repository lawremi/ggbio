context("stat_aggregate")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(type)                 (type=c("any", "start", "end", "within", "equal"))
##                    * length(others)               (by, FUN, maxgap, minoverlap, window, facets, geom)
##                    + length(select)               (select=c("all", "first", "last", "arbitrary"))
##                    * length(others)               (by, FUN, maxgap, minoverlap, window, facets, geom)
##                    + length(method)               (method = c("mean", "median","max", "min", "sum", "count", "identity"))
##                    * length(others)               (by, FUN, maxgap, minoverlap, window, facets, geom)
## Total test cases = 3 + (5 * 7) + (4 * 7) + (7 * 7)  => 115

# Note : by is not tested so : 115 - 16 = 99

# simulate testing data
source('data.R')

source('test-generator.R')

# common
types <- list("any", "start", "end")#, "within", "equal")
selects <- list("all", "first", "last", "arbitrary")
methods <- list("mean", "median","max", "min", "sum", "count", "identity")

# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_aggregate(GRanges)", {
    test <- stat_aggregate(data, xlab = "x-axis", aes(y = value))
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_aggregate(GRanges)", {
    test <- stat_aggregate(data, ylab = "y-axis", aes(y = value))
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_aggregate(GRanges)", {
    test <- stat_aggregate(data, main = "Title", aes(y = value))
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

function_selection <- function(method, FUN, y) {
    if(missing(FUN)) {
        .FUN <- switch(method, identity = {function(x) x},
                               mean = {function(x) mean(values(x)[, y])},
                               median = {function(x) median(values(x)[, y])},
                               max = {function(x) max(values(x)[, y])},
                               min = {function(x) min(values(x)[, y])},
                               sum = {function(x) sum(values(x)[, y])},
                               count ={function(x) length(x)})
    } else {
        .FUN <- FUN
    }
}

transformGRL <- function(grl, window, geom, maxgap, minoverlap, type, select, .FUN) {
    lst <- lapply(grl, function(dt) {
        snm <- unique(seqnames(dt))
        seqs <- seq(from = min(start(dt)), to = max(end(dt)), by = window)
        .by <- IRanges(start = seqs, width = window)
        if(select != "all") {
          hits <- findOverlaps(ranges(dt), .by, maxgap = maxgap, minoverlap = minoverlap,
                               type = type, select = select)
          res <- rep(NA, length(.by))
          names(res) <- c(1:length(.by))
          res2 <- unlist(lapply(split(dt, hits), .FUN))
          res[names(res2)] <- res2
        } else {
          hits <- findOverlaps(ranges(dt), .by, maxgap = maxgap, minoverlap = minoverlap,
                             type = type, select = select)
          res <- rep(NA, length(.by))
          names(res) <- c(1:length(.by))
          res2 <- unlist(lapply(base::by(as.data.frame(hits), subjectHits(hits), 
          function(x){
            x[,1]
          }), function(id){
            x <- dt[id]
            .FUN(x)
          }))
          res[names(res2)] <- res2
        }
        if(!geom %in% c("boxplot")) {
            df <- as.data.frame(.by)
            df$.value <- res
            df$.mid <- start(.by) + width(.by)/2
            df$seqnames <-snm
        } else {
            if(select != "all"){
                 grq <- GRanges(snm, .by)
                idx <- findOverlaps(dt, grq, maxgap = maxgap, minoverlap = minoverlap,
                                    type = type, select = select)
                values(dt)$.mid <- start(grq[idx]) + width(grq[idx])
                df <- as.data.frame(dt)
            } else {
                 grq <- GRanges(snm, .by)
                idx <- findOverlaps(dt, grq, maxgap = maxgap, minoverlap = minoverlap,
                                    type = type, select = select)
                dt <- dt[queryHits(idx)]
                values(dt)$.mid <- start(grq[subjectHits(idx)]) + width(grq[subjectHits(idx)])
                df <- as.data.frame(dt)
            }
        }
        df
    })
}

make_stat_aggregate_GRanges <- function(data, ..., by, FUN, maxgap = -1L, minoverlap = 0L,
                                        type = "any", select = "all", y = NULL, 
                                        window = as.integer(width(range(ranges(data)))/20),
                                        facets = NULL, method = "mean", geom = "bar") {
    args <- list(...)
    args$facets <- facets
    args$geom <- geom
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    y <- quo_name(args.aes$y)
    args.facets$scales <- "free_x"
    facet <- ggbio:::.buildFacetsFromArgs(data, args.facets)
    grl <- biovizBase::splitByFacets(data, facets)
    if(geom %in% c("boxplot")) method <- "identity"
    .FUN <- function_selection(method, FUN, y)
    lst <- transformGRL(grl, window, geom, maxgap, minoverlap, type, select, .FUN)
    
    res <- do.call(rbind, lst)
    if(!geom %in% c("boxplot", "bar")) {
        args.aes$x <- substitute(.mid)
        args.aes$y <- substitute(.value)
    } else {
        args.aes$x <- substitute(factor(.mid))
        if(geom == "boxplot") {
            if(!"y" %in% names(args.aes))
                stop("for geom boxplot, y must be provied in aes()")
            } else {
                args.aes$y <- substitute(.value)
            }
    }
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(data = res), list(aes.res), args.non)
    if(!geom %in% c("boxplot")) {
        p <- ggbio:::do.ggcall(ggplot2::stat_identity, args.res)
    } else {
        args.res <- args.res[!names(args.res) %in% "geom"]
        p <- ggbio:::do.ggcall(stat_boxplot, args.res)
    }
    p <- c(list(p), list(facet))
    p <- c(p, list(xlab("")))
    p <- ggbio:::setStat(p)
}

# wrapper to avoid unlist/list of GRanges
call_stat_aggregate_GRanges <- function(...) {
    stat_aggregate(data, ...)
}

# wrapper to avoid unlist/list of GRanges
call_make_stat_aggregate_GRanges <- function(...) {
    make_stat_aggregate_GRanges(data, ...)
}

others <- list("FUN", "maxgap", "minoverlap", "window", "facets", "geom")
# no need to restest y parameter and by is not implemented
seed_values <- list(list(FUN = function(x) {x}, aes(y = value)),
                    list(maxgap = 1L, aes(y = value)),
                    list(minoverlap = 1L, aes(y = value)),
                    list(window = 10, aes(y = value)),
                    list(facets = ~ seqnames, aes(y = value)),
                    list(geom = "boxplot", aes(y = value)))

# # for type = c("any", "start", "end")
td <- TestDetails("type", types, "stat_aggregate", "GRanges", call_stat_aggregate_GRanges,
                  call_make_stat_aggregate_GRanges, others, seed_values)
test_generator(td)

# # for select = c("all", "first", "last", "arbitrary"
td <- TestDetails("select", selects, "stat_aggregate", "GRanges", call_stat_aggregate_GRanges,
                  call_make_stat_aggregate_GRanges, others, seed_values)
test_generator(td)

# for method = c("mean", "median","max", "min", "sum", "count", "identity")
td <- TestDetails("method", methods, "stat_aggregate", "GRanges", call_stat_aggregate_GRanges,
                  call_make_stat_aggregate_GRanges, others, seed_values)
test_generator(td)
