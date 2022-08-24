context("geom_arrowrect")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(stat)                 ('stat' = c("stepping", "identity"))
##                     * length(other_parameters)     ('rect.height', 'arrow.head', 'arrow.head.rate', 'arrow.head.fix', 'group.selfish')
## Total test cases = 3 + 2 * 5 => 13

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_arrowrect", {
    test <- geom_arrowrect(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_arrowrect", {
    test <- geom_arrowrect(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_arrowrect", {
    test <- geom_arrowrect(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_arrowrect <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    rect.height <- args_non$rect.height
    arrow.head <- args_non$arrow.head
    arrow.head.rate <- args_non$arrow.head.rate
    arrow.head.fix <- args_non$arrow.head.fix
    group.selfish <- args_non$group.selfish

    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)

    # simulate expected data
    if (identical(stat, "stepping")) {
        if(is.null(rect.height))
            rect.height <- 0.4
        grl <-  biovizBase::splitByFacets(data, args$facets)
        if ("group" %in% names(args_aes)) {
            res <- endoapply(grl, biovizBase::addStepping, group.name = quo_name(args_aes$group),
                             group.selfish = group.selfish)
        } else {
            res <- endoapply(grl, biovizBase::addStepping)
        }
        res <- unlist(res)
        df <- ggbio:::breakGrTo5polyDf(res, y = "stepping", rect.height = rect.height, arrow.head = arrow.head,
                               arrow.head.rate = arrow.head.rate, arrow.head.fix = arrow.head.fix)
        args_aes$x <- as.name(".temp.x")
        args_aes$y <- as.name(".temp.y")
        args_aes$group <- as.name(".id")
        aes_temp <- do.call(aes, args_aes)
    }

    if (identical(stat, "identity")) {
        if(is.null(rect.height)) {
            rect.height <- diff(range(values(data)[,quo_name(args_aes$y)]))/20
            if (rect.height == 0)
                rect.height <- 1L
        }
        df <- ggbio:::breakGrTo5polyDf(data, y = quo_name(args_aes$y), rect.height = rect.height,
                                       arrow.head = arrow.head, arrow.head.rate = arrow.head.rate,
                                       arrow.head.fix = arrow.head.fix)
        args_aes$x <- as.name(".temp.x")
        args_aes$y <- as.name(".temp.y")
        args_aes$group <- as.name(".id")
        aes_temp <- do.call(aes, args_aes)
    }
    p <- ggbio:::do.ggcall(geom_polygon, c(list(data = df), list(aes_temp)))
    p <- c(list(p) , list(facets), list(xlab("")), list(ylab("")))
}

test_that("Test rect.height parameter with stat = 'stepping' of geom_arrowrect", {
    test <- geom_arrowrect(data, rect.height = 0.8)
    args <- list(stat = "stepping", rect.height = 0.8, arrow.head = 0.06, arrow.head.rate = 0.06,
                 group.selfish = TRUE)
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height parameter with stat = 'identity' of geom_arrowrect", {
    test <- geom_arrowrect(data, rect.height = 0.8, stat = "identity", aes(y = value))
    args <- list(stat = "identity", rect.height = 0.8, arrow.head = 0.06, arrow.head.rate = 0.06,
                 group.selfish = TRUE, aes(y = value))
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head parameter with stat = 'stepping' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head = 0.08)
    args <- list(stat = "stepping", arrow.head = 0.08, arrow.head.rate = 0.08,
                 group.selfish = TRUE)
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head parameter with stat = 'identity' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head = 0.08, stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.head = 0.08, arrow.head.rate = 0.08,
                 group.selfish = TRUE, aes(y = value))
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head.rate parameter with stat = 'stepping' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head.rate = 0.08)
    args <- list(stat = "stepping", arrow.head = 0.06, arrow.head.rate = 0.08,
                 group.selfish = TRUE)
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head.rate parameter with stat = 'identity' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head.rate = 0.08, stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.head = 0.06, arrow.head.rate = 0.08,
                 group.selfish = TRUE, aes(y = value))
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head.fix parameter with stat = 'stepping' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head.fix = 0.08)
    args <- list(stat = "stepping", arrow.head = 0.06, arrow.head.rate = 0.06,
                 arrow.head.fix = 0.08, group.selfish = TRUE)
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.head.fix parameter with stat = 'identity' of geom_arrowrect", {
    test <- geom_arrowrect(data, arrow.head.fix = 0.08, stat = "identity", aes(y = value))
    args <- list(stat = "identity", arrow.head = 0.06, arrow.head.rate = 0.06,
                 arrow.head.fix = 0.08, group.selfish = TRUE, aes(y = value))
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'stepping' of geom_arrowrect", {
    test <- geom_arrowrect(data, group.selfish = FALSE)
    args <- list(stat = "stepping", arrow.head = 0.06, arrow.head.rate = 0.06,
                 group.selfish = FALSE)
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'identity' of geom_arrowrect", {
    test <- geom_arrowrect(data, stat = "identity", aes(y = value), group.selfish = FALSE)
    args <- list(stat = "identity", arrow.head = 0.06, arrow.head.rate = 0.06,
                group.selfish = TRUE, aes(y = value))
    expected <- make_arrowrect(data, args)
    expect_equal(test, expected)
})
