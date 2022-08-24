context("stat_reduce")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (drop.empty.ranges, min.gapwidth, facets, geom)
## Total test cases = 3 + 4 => 7

# simulate testing data
source('data.R')

source('test-generator.R')

# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_reduce(GRanges)", {
    test <- stat_reduce(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_reduce(GRanges)", {
    test <- stat_reduce(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_reduce(GRanges)", {
    test <- stat_reduce(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_reduce_GRanges <- function(data, ..., drop.empty.ranges = FALSE,
                                     min.gapwidth = 1L, facets = NULL,
                                     geom = NULL) {
    args <- list(...)
    args$facets <- facets
    args$geom <- geom
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    data <- reduce(data, drop.empty.ranges = drop.empty.ranges,
                   min.gapwidth = min.gapwidth)
    args.non$data <- data
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- list(do.call(ggbio:::stat_stepping, args.res))
    p <- c(p, list(xlab("")), list(ylab("")))
    attr(p, "isStat") <- TRUE
    p
}

# wrapper to avoid unlist/list of GRanges
call_stat_reduce_GRanges <- function_wrapper(stat_reduce, data)
call_make_stat_reduce_GRanges <- function_wrapper(make_stat_reduce_GRanges, data)

others <- list("drop.empty.ranges", "min.gapwidth", "facets", "geom")
seed_values <- list(list(drop.empty.ranges = TRUE),
                    list(min.gapwidth = 2L),
                    list(facets = ~ seqnames),
                    list(geom = "rect"))

td <- TestDetails(module_name = "stat_reduce", generic_name = "GRanges",
                  test_func = call_stat_reduce_GRanges,
                  expected_func = call_make_stat_reduce_GRanges,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (drop.empty.ranges, min.gapwidth, with.inframe.attrib, facets, geom)
## Total test cases = 3 + 5 => 8

# Testing for IRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_reduce(IRanges)", {
    data <- data@ranges
    test <- stat_reduce(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_reduce(IRanges)", {
    data <- data@ranges
    test <- stat_reduce(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Position"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_reduce(IRanges)", {
    data <- data@ranges
    test <- stat_reduce(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Position"), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_reduce_IRanges <- function(data, ..., drop.empty.ranges = FALSE,
                                     min.gapwidth = 1L, with.inframe.attrib=FALSE,
                                     facets = NULL, geom = NULL) {
    data <- reduce(data, drop.empty.ranges = drop.empty.ranges,
                   min.gapwidth = min.gapwidth,
                   with.inframe.attrib = with.inframe.attrib)
    df <- values(data)
    values(data) <- NULL
    data <- GRanges("chr_non", data)
    values(data) <- df
    args <- list(...)
    args$facets <- facets
    args$geom <- geom
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    args.non$data <- data
    aes.res <- do.call(aes, args.aes)
    args.res <- c(list(aes.res), args.non)
    p <- list(do.call(ggbio::stat_stepping, args.res))
    p <- c(p, list(xlab("Position")), list(ylab("")))
    attr(p, "isStat") <- TRUE
    p
}

# wrapper to avoid unlist/list of IRanges
call_stat_reduce_IRanges <- function_wrapper(stat_reduce, data@ranges)
call_make_stat_reduce_IRanges <- function_wrapper(make_stat_reduce_IRanges, data@ranges)

others <- c(others, list("with.inframe.attrib"))
seed_values <- c(seed_values, list(list(with.inframe.attrib = TRUE)))

td <- TestDetails(module_name = "stat_reduce", generic_name = "IRanges",
                  test_func = call_stat_reduce_IRanges,
                  expected_func = call_make_stat_reduce_IRanges,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)
