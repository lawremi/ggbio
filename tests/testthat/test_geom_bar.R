context("geom_bar")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + 1                            (for checking data)
## Total test cases = 3 + 1 => 4

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_bar", {
    test <- geom_bar(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("score"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_bar", {
    test <- geom_bar(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_bar", {
    test <- geom_bar(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("score"),labs(title = "Title"))
    expect_identical(test, expected)
})

test_that("Test data of geom_bar", {
    test <- geom_bar(data)
    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(list(), facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    rec_aes <- aes(xmin = start, xmax = end, ymin = 0, ymax = score)
    rec_args <- list(data = df, rec_aes)
    expected <- list(ggbio:::do.ggcall(geom_rect, rec_args),
                    facets, xlab(""), ylab("score"))
    expect_equal(test, expected)
})
