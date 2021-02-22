context("geom_arrow")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(stat)                 ('stat' = c("stepping", "identity"))
##                     * length(other_parameters)     ('angle', 'length', 'type', 'facets', 'arrow.rate', 'group.selfish')
## Total test cases = 3 + 2 * 6 => 15

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_arrow", {
    test <- geom_arrow(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_arrow", {
    test <- geom_arrow(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_arrow", {
    test <- geom_arrow(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_arrow <- function(data, args) {

    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    arrow.rate <- args_non$arrow.rate
    angle <- args_non$angle
    length <- args_non$length
    type <- args_non$type
    group.selfish <- args_non$group.selfish

    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)

    # arrow specific pre-processing
    arrow.r <- max(1L, round(width(range(ranges(data))) * arrow.rate, 0))
    idx <- width(data) > 1 # remove width = 1
    data <- data[idx]

    # simulate expected data
    if (identical(stat, "stepping")) {
        grl <-  biovizBase::splitByFacets(data, args$facets)
        if ("group" %in% names(args_aes)) {
            res <- endoapply(grl, biovizBase::addStepping, group.name = quo_name(args_aes$group),
                             group.selfish = group.selfish)
        } else {
            res <- endoapply(grl, biovizBase::addStepping)
        }
        df <- biovizBase::mold(unlist(res))
        lst <- apply(df, 1, function(x) {
            x <- as.data.frame(t(x))
            x.s <- as.numeric(as.character(x$start))
            x.e <- as.numeric(as.character(x$end))
            N <- (x.e - x.s) %/% arrow.r
            N <- ifelse(N <= 2, 2, N)
            res <- approx(c(x.s, x.e), rep(as.numeric(as.character(x$stepping)), 2), n = N)
            res.df <- do.call(rbind, lapply(1:N, function(i) x))
            res.df$temp.x <- res$x
            .res <- res.df[-N,]
            .res$temp.x2 <- res.df[-1, "temp.x"]
            .res
        })
        res <- do.call(rbind, lst)
        res$stepping <- as.numeric(res$stepping)
        args_aes$x <- as.name("temp.x")
        args_aes$xend <- as.name("temp.x2")
        args_aes$y <- args_aes$yend <- as.name("stepping")
    }

    if (identical(stat, "identity")) {
        df <- biovizBase::mold(data)
        lst <- lapply(split(df, seq_len(nrow(df))), function(x) {
            x.s <- x$start
            x.e <- x$end
            N <- (x.e - x.s) %/% arrow.r
            N <- ifelse(N <= 2, 2, N)
            res <- approx(c(x.s, x.e ), rep(0, 2),n = N)
            res.df <- x[rep(1L, N),]
            res.df$start <- res$x
            .res <- res.df[-N,]
            .res$end <- res.df[-1L, "start"]
            .res
      })
      res <- do.call(rbind,lst)
      .y <- args_aes$y
      args_aes$x <- as.name("start")
      args_aes$xend <- as.name("end")
      args_aes$y <-  args_aes$yend <- .y
    }

    temp_aes <- do.call(aes, args_aes)

    p <- ggbio:::by2(res, res$strand, function(x) {
        s <- unique(as.character(x$strand))
        p <- switch(s,
                "+" = {
                    arrow <- list(arrow = arrow(length = length, ends = "last",
                                                type = type, angle = angle))
                    ggbio:::do.ggcall(ggplot2::geom_segment,
                                      c(list(data = x), list(temp_aes), arrow))

                },
                "-" = {
                    arrow <- list(arrow = arrow(length = length, ends = "first",
                                                type = type, angle = angle))
                    ggbio:::do.ggcall(ggplot2::geom_segment,
                                      c(list(data = x), list(temp_aes), arrow))
                },
                "*" = {
                    ggbio:::do.ggcall(ggplot2::geom_segment,
                                      c(list(data = x), list(temp_aes)))
                })
        p
    })
    # simulate expected return list
    expected <- c(list(p), list(facets), list(xlab("")))
}

test_that("Test angle parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, angle = 20)
    args <- list(stat = "stepping", arrow.rate = 0.03, angle = 20,
                 length = unit(0.12, "cm"), type = "open")
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test angle parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, angle = 20, stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.rate = 0.03, angle = 20,
                 length = unit(0.12, "cm"), type = "open", aes(y = value))
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test length parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, length = unit(0.10, "cm"))
    args <- list(stat = "stepping", arrow.rate = 0.03, angle = 30,
                 length = unit(0.10, "cm"), type = "open")
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test length parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, length = unit(0.10, "cm"), stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.rate = 0.03, angle = 30,
                 length = unit(0.10, "cm"), type = "open", aes(y = value))
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test type parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, type = "closed")
    args <- list(stat = "stepping", arrow.rate = 0.03, angle = 30,
                 length = unit(0.12, "cm"), type = "closed")
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test type parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, type = "closed", stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.rate = 0.03, angle = 30,
                 length = unit(0.12, "cm"), type = "closed", aes(y = value))
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, facets = sample ~ seqnames)
    args <- list(facets = sample ~ seqnames, stat = "stepping", arrow.rate = 0.03,
                 angle = 30, length = unit(0.12, "cm"), type = "open")
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, facets = sample ~ seqnames, stat = "identity", aes(y = value))
    args <- list(facets = sample ~ seqnames, stat = "identity", arrow.rate = 0.03, angle = 30,
                 length = unit(0.12, "cm"), type = "open", aes(y = value))
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.rate parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, arrow.rate = 0.01)
    args <- list(stat = "stepping", arrow.rate = 0.01, angle = 30,
                 length = unit(0.12, "cm"), type = "open")
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.rate parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, arrow.rate = 0.01, stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.rate = 0.01, angle = 30,
                 length = unit(0.12, "cm"), type = "open", aes(y = value))
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'stepping' of geom_arrow", {
    test <- geom_arrow(data, group.selfish = FALSE, aes(y = score, group = pair))
    args <- list(stat = "stepping", arrow.rate = 0.03, angle = 30,
                 length = unit(0.12, "cm"), type = "open", aes(y = score, group = pair),
                 group.selfish = FALSE)
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'identity' of geom_arrow", {
    test <- geom_arrow(data, group.selfish = FALSE, stat = "identity", aes(y = score, group = pair))
    args <- list(stat = "identity", arrow.rate = 0.03, angle = 30,
                 length = unit(0.12, "cm"), type = "open", aes(y = score, group = pair),
                 group.selfish = FALSE)
    expected <- make_arrow(data, args)
    expect_equal(test, expected)
})
