context("stat_stepping")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(geom)                 (geom = c("rect","alignment", "segment"))
##                    * length(other_paramerts)      (facets)
## Total test cases = 3 + (3 * 1) => 6

# simulate testing data
source('data.R')

# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_stepping(GRanges)", {
    test <- stat_stepping(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    # first x,y labels are coming from geom_rect
    expected <- list(xlab(""), ylab(""), xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_stepping(GRanges)", {
    test <- stat_stepping(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_stepping(GRanges)", {
    test <- stat_stepping(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_stepping_GRanges <- function(data, ..., facets = NULL, geom = "rect") {
    geom <- match.arg(geom)
    args <- list(...)
    args$facets <- facets
    args$stat <- "stepping"
    args$data <- data
    p <- switch(geom, rect = ggbio:::do.ggcall(ggbio:::geom_rect, args),
                      alignment = do.call(ggbio:::geom_alignment, args),
                      segment = ggbio:::do.ggcall(ggbio:::geom_segment, args))
    p <- c(p, list(xlab(""), ylab("")))
    p <- ggbio:::setStat(p)
}

test_that("Test facet parameter with geom = 'rect' of stat_stepping(GRanges)", {
    test <- stat_stepping(data, geom = "rect", facets = sample ~ seqnames)
    expected <- make_stat_stepping_GRanges(data, geom = "rect", facets = sample ~ seqnames)
    expect_equal(test, expected)
})

test_that("Test facet parameter with geom = 'alignment' of stat_stepping(GRanges)", {
    test <- stat_stepping(data, geom = "rect", facets = sample ~ seqnames)
    expected <- make_stat_stepping_GRanges(data, geom = "rect", facets = sample ~ seqnames)
    expect_equal(test, expected)
})

test_that("Test facet parameter with geom = 'segment' of stat_stepping(GRanges)", {
    test <- stat_stepping(data, geom = "rect", facets = sample ~ seqnames)
    expected <- make_stat_stepping_GRanges(data, geom = "rect", facets = sample ~ seqnames)
    expect_equal(test, expected)
})
