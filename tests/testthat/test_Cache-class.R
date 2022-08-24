context("Cache")

cache <- new("Cache", cached_xlim = NULL)

test_that("Test Cache Constructor", {
    test <- ggbio:::Cache()
    expect_identical(test, cache)
})

# cached
test_that("Test cached() of Cache class", {
    test <- ggbio:::cached(cache)
    expect_identical(test, TRUE)
})

test_that("Test cached()<- of Cache class", {
    ggbio:::cached(cache) <- FALSE
    test <- ggbio:::cached(cache)
    expect_identical(test, FALSE)
})

# cached_xlim
test_that("Test cached_xlim() of Cache class", {
    test <- ggbio:::cached_xlim(cache)
    expect_identical(test, NULL)
})

test_that("Test cached_xlim()<- of Cache class", {
    ggbio:::cached_xlim(cache) <- 1
    test <- ggbio:::cached_xlim(cache)
    expect_identical(test, c(1, 1))
})

# cached_ylim
test_that("Test cached_ylim() of Cache class", {
    test <- ggbio:::cached_ylim(cache)
    expect_identical(test, NULL)
})

test_that("Test cached_ylim()<- of Cache class", {
    ggbio:::cached_ylim(cache) <- 1
    test <- ggbio:::cached_ylim(cache)
    expect_identical(test, c(1, 1))
})

# cached_item
test_that("Test cached_item() of Cache class", {
    test <- ggbio:::cached_item(cache)
    expect_identical(test, list())
})

test_that("Test cached_item()<- of Cache class", {
    ggbio:::cached_item(cache) <- list(1, 2)
    test <- ggbio:::cached_item(cache)
    expect_identical(test, list(1, 2))
})

# addItem
test_that("Test addItem() of Cache class", {
    cache <- ggbio:::addItem(cache, 1, 1)
    test <- ggbio:::cached_item(cache)
    expect_identical(test, list(1, 1))
})

# cached_which
test_that("Test cached_which() of Cache class", {
    test <- ggbio:::cached_which(cache)
    expect_identical(test, NULL)
})

test_that("Test cached_which()<- of Cache class", {
    value <- GRanges("chr1", IRanges(1, 2))
    ggbio:::cached_which(cache) <- value
    test <- ggbio:::cached_which(cache)
    expect_identical(test, value)
})

# addWhich
test_that("Test addWhich(GRanges) of Cache class", {
    value <- GRanges("chr1", IRanges(1, 2))
    cache <- ggbio:::addWhich(cache, value)
    test <- ggbio:::cached_which(cache)
    expect_identical(test, value)
})

test_that("Test addWhich(BasicFilterORlist) of Cache class", {
    value <- list(1, 2)
    cache <- ggbio:::addWhich(cache, value)
    test <- ggbio:::cached_which(cache)
    expect_identical(test, value)
})

# cacheSet
test_that("Test cacheSet(GRanges) of Cache class", {
    value <- GRanges("chr1", IRanges(1, 2))
    expected_item <- list(cache)
    expected_which <- value
    cache <- ggbio:::cacheSet(cache, value)
    test_which <- ggbio:::cached_which(cache)
    test_item <- ggbio:::cached_item(cache)
    expect_identical(test_which, expected_which)
    expect_identical(test_item, expected_item)
})

test_that("Test cacheSet(BasicFilterORlist) of Cache class", {
    value <- list(1, 2)
    expected_item <- list(cache)
    expected_which <- value
    cache <- ggbio:::cacheSet(cache, value)
    test_which <- ggbio:::cached_which(cache)
    test_item <- ggbio:::cached_item(cache)
    expect_identical(test_which, expected_which)
    expect_identical(test_item, expected_item)
})
