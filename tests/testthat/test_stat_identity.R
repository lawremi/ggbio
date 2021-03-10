context("stat_identity")
## Total test cases = 2                              (geom)
##                    + length(other_paramerts)      (facets)
## Total test cases = 3

## simulate testing data
source('data.R')

# Testing for GRanges --------------------------------------------------------

make_stat_identity_GRanges <- function(data, ..., geom = NULL) {
    args <- list(...)
    gr.geoms <- c("chevron", "arrow", "arrowrect", "segment", "rect", "alignment")
    args.facets <-biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- ggbio:::.buildFacetsFromArgs(data, args.facets)
    if(is.null(geom))  geom <- "segment"
    if(!geom %in% gr.geoms) {
        args$geom <- geom
        data <- biovizBase::mold(data)
        args$data <- data
        p <- ggbio:::do.ggcall(ggplot2::stat_identity, args)
    } else {
        .geom.fun <- ggbio:::getGeomFun(geom)
        args$stat <- "identity"
        args$data <- data
        p <- ggbio:::do.ggcall(.geom.fun, args)
    }
    p <- c(list(p), list(facet))
    p <- ggbio:::setStat(p)
}

test_that("Test geom parameter of stat_identity(GRanges)", {
    test <- stat_identity(data, aes(y = value), geom = "segment")
    expected <- make_stat_identity_GRanges(data, aes(y = value), geom = "segment")
    expect_equal(test, expected)
})

test_that("Test geom parameter of stat_identity(GRanges)", {
    test <- stat_identity(data, aes(x = start, y = value), geom = "point")
    expected <- make_stat_identity_GRanges(data, aes(x = start, y = value), geom = "point")
    expect_equal(test, expected)
})

test_that("Test facets parameter of stat_identity(GRanges)", {
    test <- stat_identity(data, aes(x = start, y = value), geom = "point",
                          facets = sample ~ seqnames)
    expected <- make_stat_identity_GRanges(data, aes(x = start, y = value), geom = "point",
                                           facets = sample ~ seqnames)
    expect_equal(test, expected)
})

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (geom)
## Total test cases = 3 + 1 => 4

# Testing for Rle ------------------------------------------------------------

make_stat_identity_Rle <- function(data, ..., geom = NULL) {
    args <- list(...)
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    if(is.null(geom)) geom <- "line"
    x <- 1:length(data)                
    y <- as.numeric(data)
    df <- data.frame(x = x, y = y)
    args.non$geom <- geom
    args.non$data <- df
    args.aes <- list(x = substitute(x), y = substitute(y))
    args.aes <- list(do.call(aes, args.aes))
    p <- ggbio:::do.ggcall(ggplot2::stat_identity, c(args.non, args.aes))
    p <- c(p, list(xlab("x")), list(ylab("y")))
    p <- ggbio:::setStat(p)
}

test_that("Test xlab parameter of stat_identity(Rle)", {
    test <- stat_identity(xRle, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("y"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_identity(Rle)", {
    test <- stat_identity(xRle, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_identity(Rle)", {
    test <- stat_identity(xRle, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y"),labs(title = "Title"))
    expect_identical(test, expected)
})

test_that("Test geom parameter of stat_identity(Rle)", {
    test <- stat_identity(xRle, geom = "point")
    expected <- make_stat_identity_Rle(xRle, geom = "point")
    expect_equal(test, expected)
})

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (geom, indName)
## Total test cases = 3 + 2 => 5

# Testing for RleList --------------------------------------------------------

make_stat_identity_RleList <- function(data, ..., geom = NULL, indName = "sample") {
    args <- list(...)
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    geom <- if(is.null(geom)) "line"
    x <- do.call(c,lapply(elementNROWS(data), function(n) 1:n))
    y <- as.numeric(unlist(data))
    if(is.null(names(data)))
        nms <- rep(1:length(data), times = elementNROWS(data))
    else
        nms <- rep(names(data), times = elementNROWS(data))
    df <- data.frame(x = x, y = y, z = nms)
    colnames(df) <- c("x", "y", indName)
    facets <- as.formula(paste(indName, "~ .", sep = ""))
    facet <- facet_grid(facets)
    args.non$geom <- geom
    args.non$data <- df
    args.aes <- list(x = substitute(x), y = substitute(y))
    args.aes <- list(do.call(aes, args.aes))
    p <- ggbio:::do.ggcall(ggplot2::stat_identity, c(args.non, args.aes))  
    p <- c(p, list(xlab("x")), list(ylab("y")))
    p <- c(list(p), list(facet))
    p <- ggbio:::setStat(p)
}

test_that("Test xlab parameter of stat_identity(RleList)", {
    test <- stat_identity(xRleList, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("y"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_identity(RleList)", {
    test <- stat_identity(xRleList, ylab = "y-axis")
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_identity(RleList)", {
    test <- stat_identity(xRleList, main = "Title")
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y"),labs(title = "Title"))
    expect_identical(test, expected)
})

test_that("Test geom parameter of stat_identity(RleList)", {
    test <- stat_identity(xRleList, geom = "point")
    expected <- make_stat_identity_RleList(xRleList, geom = "point")
    expect_equal(test, expected)
})

test_that("Test indName parameter of stat_identity(RleList)", {
    test <- stat_identity(xRleList, indName = "")
    expected <- make_stat_identity_RleList(xRleList, indName = "")
    expect_equal(test, expected)
})
