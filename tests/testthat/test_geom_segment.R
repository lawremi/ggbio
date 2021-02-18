context("geom_segment")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(stat)                 ('stat' = c("stepping", "identity"))
##                     * length(other_parameters)     (facets', 'group.selfish')
## Total test cases = 3 + (2 * 2) => 7
##
## The length of 'stat' and other parameters are multiplied,
## As other parameters can affect the state of both the 'stat'

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_segment", {
    test <- geom_segment(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_segment", {
    test <- geom_segment(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_segment", {
    test <- geom_segment(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

test_that("Test facets parameter with stat = 'stepping' of geom_segment", {
    test <- geom_segment(data, facets = sample ~ seqnames, stat = "stepping")
    # simulate expected facets
    args <- list(facets = sample ~ seqnames, stat = "stepping")
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    grl <-  biovizBase::splitByFacets(data, args$facets)
    res <- endoapply(grl, biovizBase::addStepping, extend.size = 0)
    df <- biovizBase::mold(unlist(res))
    seg_aes <- aes(x = start, y = stepping, xend = end, yend = stepping)
    seg_args <- list(data = df, seg_aes)
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     scale_y_continuous(breaks = NULL))
    expected <- list(expected, facets, xlab(""))
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'identity' of geom_segment", {
    test <- geom_segment(data, facets = sample ~ seqnames,
                      stat = "identity", aes(y = score))
    # simulate expected facets
    args <- list(facets = sample ~ seqnames, stat = "identity", aes(y = score))
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    seg_aes <- aes(x = start, y = score, xend = end, yend = score)
    seg_args <- c(list(data = df), list(seg_aes, facets = args$facets))
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args))
    expected <- list(expected, facets, xlab(""))
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'stepping' of geom_segment", {
    test <- geom_segment(data, stat = "stepping", aes(y = score, group = pair), group.selfish = FALSE)
    # simulate expected facets
    args <- list(stat = "stepping", aes(y = score, group = pair), group.selfish = FALSE)
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    grl <-  biovizBase::splitByFacets(data, args$facets)
    res <- endoapply(grl, biovizBase::addStepping, extend.size = 0, group.selfish = FALSE, group.name = "pair")
    df <- biovizBase::mold(unlist(res))
    seg_aes <- aes(x = start, y = stepping, xend = end, yend = stepping)
    seg_args <- list(data = df, seg_aes)
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     scale_y_continuous(breaks = NULL))
    expected <- list(expected, facets, xlab(""))
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'identity' of geom_segment", {
    test <- geom_segment(data, stat = "identity", aes(y = score, group = pair), group.selfish = FALSE)
    # simulate expected facets
    args <- list(stat = "identity", aes(y = score, group = pair), group.selfish = FALSE)
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    seg_aes <- aes(x = start, y = score, xend = end, yend = score)
    seg_args <- c(list(data = df), list(seg_aes, facets = args$facets))
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args))
    expected <- list(expected, facets, xlab(""))
    expect_equal(test, expected)
})
