context("layout_karyogram")

source('data.R')

test_that("Test xlab parameter of plotStackedOverview", {
    test <- plotStackedOverview(data, xlab = "x-axis")
    test <- test$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of plotStackedOverview", {
    test <- plotStackedOverview(data, ylab = "y-axis")
    test <- test$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of plotStackedOverview", {
    test <- plotStackedOverview(data, main = "title")
    test <- test$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test plotStackedOverview()", {
    test <- plotStackedOverview(data)
    test$plot_env <- NULL
    facets <- seqnames ~ .
    p <- layout_karyogram(data, cytobands = FALSE, facets = facets, geom = NULL)
    p <- ggplot() + p
    args.non <- list(geom = "rect", facets = facets)
    args.res <- c(list(data = data), list(aes()), args.non)
    expected <- p + do.call(layout_karyogram, args.res)
    expected$plot_env <- NULL
    expect_equal(test, expected)
})
