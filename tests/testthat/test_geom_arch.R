context("geom_arch")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(other_parameters)     (facets', 'rect.height', 'max.height')
## Total test cases = 3 + 3 => 6

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

test_that("Test facets parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, facets = sample ~ seqnames)
    # simulate expected facets
    args <- list(facets = sample ~ seqnames)
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    signs <- 1
    rect.height <- 0
    df$.y <- rep(0, nrow(df)) + rect.height * signs
    arch_aes <- aes(x = start, xend = end, y = .y)
    arch_args <- c(list(data = df), facets = args$facets,
                   max.height = 10, list(arch_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_arch, arch_args)),
                  list(facets), list(xlab("")))
    expect_equal(test, expected)
})

test_that("Test rect.height parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, rect.height = 5)
    # simulate expected facets
    args <- list()
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    signs <- 1
    rect.height <- 5
    df$.y <- rep(0, nrow(df)) + rect.height * signs
    arch_aes <- aes(x = start, xend = end, y = .y)
    arch_args <- c(list(data = df), facets = args$facets,
                   max.height = 10, list(arch_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_arch, arch_args)),
                  list(facets), list(xlab("")))
    expect_equal(test, expected)
})

test_that("Test max.height parameter of geom_arch(GRanges)", {
    test <- geom_arch(data, max.height = 5)
    # simulate expected facets
    args <- list()
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    signs <- 1
    rect.height <- 0
    df$.y <- rep(0, nrow(df)) + rect.height * signs
    arch_aes <- aes(x = start, xend = end, y = .y)
    arch_args <- c(list(data = df), facets = args$facets,
                   max.height = 5, list(arch_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_arch, arch_args)),
                  list(facets), list(xlab("")))
    expect_equal(test, expected)
})

## Total test cases =  length(other_parameters)     ('n', 'max.height')
## Total test cases = 2

# Testing for data.frame ---------------------------------------------------------

swap <- function(x, y) {
    x.sub <- substitute(x)
    y.sub <- substitute(y)
    x.val <- x
    e <- parent.frame()
    do.call("<-", list(x.sub, y.sub), envir = e)
    do.call("<-", list(y.sub, x.val), envir = e)
}

makeArches <- function(data, startX, endX, n, y, h, bottom = TRUE, flip = FALSE) {
    xx <- c()
    yy <- c()
    for (i in 1:n) {
        ang <- i * pi / (2 * n)
        xx[i] <- cos(ang)
        yy[i] <- sin(ang)
    }
    # takes the quarter of the curve calculated, flips a copy over the y axis
    # reduces time spent in for loop
    if (flip) {
        swap(xx, yy)
        if(bottom) {
            yy <- c(1, yy, rev(-yy), -1, 1)
            xx <- c(0, xx, rev(xx), 0, 0)
        } else {
            yy <- c(1, yy, rev(-yy), -1)
            xx <- c(0, xx, rev(xx), 0)
        }
        swap(xx, yy)
    } else {
        xx <- c(1, xx, rev(-xx), -1)
        yy <- c(0, yy, rev(yy), 0)
    }
    # sets up dataframe to keep track of all points to draw all arches
    junc <- rep(seq_along(startX), each = length(xx))
    startX <- rep(startX, each = length(xx))
    endX <- rep(endX, each = length(xx))
    h <- rep(h, each = length(xx))
    y <- rep(y, each = length(xx))
    jump <- abs(endX - startX)
    jumpAdj <- if (length(jump)) max(jump) / max(abs(h)) else NA
    apoint <- data.frame(xx = xx * (abs(startX - endX) / 2) + (startX + endX) / 2,
                         yy = yy * h + y, junc,
                         s = ((abs(h) - jump / jumpAdj)) /
                         if (length(jump)) max(jump) else NA)
    data$junc <- seq_len(nrow(data))
    apoint <- merge(apoint, data, by = "junc")
}

test_that("Test n parameter of geom_arch(data.frame)", {
    df <- as.data.frame(data)
    test <- geom_arch(df, aes(x = score, xend = value), n = 2)
    # simulate expected data
    startX <- df$score
    endX <- df$value
    max.height <- 10
    n <- 2
    h <- rep(max.height, length(startX))
    y <- rep(0, length(startX))
    df <- makeArches(df, startX, endX, n, y, h)
    arch_aes <- aes(x = xx, y = yy, group = junc)
    arch_args <- c(list(data = df), list(arch_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_line, arch_args)),
                  list(ylab("")))
    expect_equal(test, expected)
})

test_that("Test max.height parameter of geom_arch(data.frame)", {
    df <- as.data.frame(data)
    test <- geom_arch(df, aes(x = score, xend = value), max.height = 20)
    # simulate expected data
    startX <- df$score
    endX <- df$value
    max.height <- 20
    n <- 25
    h <- rep(max.height, length(startX))
    y <- rep(0, length(startX))
    df <- makeArches(df, startX, endX, n, y, h)
    line_aes <- aes(x = xx, y = yy, group = junc)
    line_args <- c(list(data = df), list(line_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_line, line_args)),
                  list(ylab("")))
    expect_equal(test, expected)
})

## Total test cases =  length(other_parameters)     ('n', 'max.height', bottom)
## Total test cases = 3

# Testing for geom_arch_flip------------------------------------------------------

test_that("Test n parameter of geom_arch_flip(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip(df, aes(x = start, y = value,
                                           xend = end, yend = score),
                                   n = 20)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 10
    n <- 20
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    ploygon_aes <- aes(x = xx, y = yy, group = junc)
    ploygon_args <- c(list(data = df), list(ploygon_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_polygon, ploygon_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})

test_that("Test max.height parameter of geom_arch_flip(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip(df, aes(x = start, y = value,
                                           xend = end, yend = score),
                                   max.height = 20)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 20
    n <- 25
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    ploygon_aes <- aes(x = xx, y = yy, group = junc)
    ploygon_args <- c(list(data = df), list(ploygon_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_polygon, ploygon_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})

test_that("Test bottom parameter of geom_arch_flip(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip(df, aes(x = start, y = value,
                                           xend = end, yend = score),
                                   bottom = FALSE)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 10
    n <- 25
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE, bottom = FALSE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    ploygon_aes <- aes(x = xx, y = yy, group = junc)
    ploygon_args <- c(list(data = df), list(ploygon_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_polygon, ploygon_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})

## Total test cases =  length(other_parameters)     ('n', 'max.height', bottom)
## Total test cases = 3

# Testing for geom_arch_flip2-----------------------------------------------------

test_that("Test n parameter of geom_arch_flip2(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip2(df, aes(x = start, y = value,
                                            xend = end, yend = score),
                                    n = 20)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 10
    n <- 20
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE, bottom = FALSE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    path_aes <- aes(x = xx, y = yy, group = junc)
    path_args <- c(list(data = df), list(path_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_path, path_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})

test_that("Test max.height parameter of geom_arch_flip2(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip2(df, aes(x = start, y = value,
                                            xend = end, yend = score),
                                    max.height = 20)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 20
    n <- 25
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE, bottom = FALSE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    path_aes <- aes(x = xx, y = yy, group = junc)
    path_args <- c(list(data = df), list(path_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_path, path_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})

test_that("Test bottom parameter of geom_arch_flip2(data.frame)", {
    df <- as.data.frame(data)
    test <- ggbio:::geom_arch_flip2(df, aes(x = start, y = value,
                                            xend = end, yend = score),
                                    bottom = TRUE)
    # simulate expected data
    startY <- df$value
    endY <- df$score
    max.height <- 10
    n <- 25
    h <- rep(max.height, length(startY))
    x <- df$start
    df <- makeArches(df, startY, endY, n, x, h, flip = TRUE, bottom = TRUE)
    names(df)[2] <- "yy"
    names(df)[3] <- "xx"
    path_aes <- aes(x = xx, y = yy, group = junc)
    path_args <- c(list(data = df), list(path_aes))
    # simulate expected return list
    expected <- c(list(ggbio:::do.ggcall(geom_path, path_args)),
                  list(ylab("value")))
    expect_equal(test, expected)
})
