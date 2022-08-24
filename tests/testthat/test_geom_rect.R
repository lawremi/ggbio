context("geom_rect")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(stat)                 ('stat' = c("stepping", "identity"))
##                     * length(other_parameters)     (facets', 'rect.height', 'group.selfish')
## Total test cases = 3 + (2 * 3) => 9
##
## The length of 'stat' and other parameters are multiplied,
## As other parameters can affect the state of both the 'stat'


## simulate testing data
source('data.R')

labels <- list(ggplot2::xlab(""), ggplot2::ylab(""))
# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_rect", {
    test <- geom_rect(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    labels[[1]] <- ggplot2::xlab("x-axis")
    expected <- labels
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_rect", {
    test <- geom_rect(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    labels[[2]] <- ggplot2::ylab("y-axis")
    expected <- labels
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_rect", {
    test <- geom_rect(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- c(labels, list(labs(title = "Title")))
    expect_identical(test, expected)
})

test_that("Test facets parameter with stat = 'stepping' of geom_rect", {
    test <- geom_rect(data, facets = sample ~ seqnames, stat = "stepping")
    # simulate expected facets
    args <- list(facets = sample ~ seqnames, stat = "stepping")
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    grl <-  biovizBase::splitByFacets(data, args$facets)
    res <- endoapply(grl, biovizBase::addStepping, extend.size = 0)
    df <- biovizBase::mold(unlist(res))
    seg_aes <- aes(x = start, y = stepping - 0.4, xend = start, yend = stepping + 0.4)
    seg_args <- list(data = df, seg_aes)
    rec_aes <- aes(xmin = start, xmax = end, ymin = stepping - 0.4, ymax = stepping + 0.4)
    rec_args <- list(data = df, rec_aes)
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args),
                     scale_y_continuous(breaks = NULL))
    expected <- c(list(expected), list(facets), labels)
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'identity' of geom_rect", {
    test <- geom_rect(data, facets = sample ~ seqnames,
                      stat = "identity", aes(y = score))
    # simulate expected facets
    args <- list(facets = sample ~ seqnames, stat = "identity", aes(y = score))
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    rect.height <- diff(range(values(data)[,as.character("score")]))/20
    seg_aes <- list(x = as.name("start"),
                    y = substitute(y + rect.height, list(y = as.name("score"), rect.height= rect.height)),
                    xend = as.name("start"),
                    yend = substitute(y - rect.height, list(y =as.name("score"), rect.height= rect.height)))
    seg_aes <- do.call(aes, seg_aes)
    seg_args <- c(list(data = df), list(seg_aes, facets = args$facets))
    rec_aes <- list(y = as.name("score"),
                    xmin = as.name("start"),
                    xmax = as.name("end"),
                    ymin = substitute(y + rect.height, list(y = as.name("score"), rect.height = rect.height)),
                    ymax = substitute(y - rect.height, list(y = as.name("score"), rect.height = rect.height)))
    rec_aes <- do.call(aes, rec_aes)
    rec_args <- c(list(data = df), list(rec_aes, facets = args$facets))
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args))
    expected <- c(list(expected), list(facets), list(xlab("")))
    expect_equal(test, expected)
})

test_that("Test rect.height parameter with stat = 'stepping' of geom_rect", {
    test <- geom_rect(data, rect.height = 0.1, stat = "stepping")
    # simulate expected facets
    args <- list(rect.height = 0.1, stat = "stepping")
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    grl <-  biovizBase::splitByFacets(data, args$facets)
    res <- endoapply(grl, biovizBase::addStepping, extend.size = 0)
    df <- biovizBase::mold(unlist(res))
    seg_aes <- aes(x = start, y = stepping - 0.1, xend = start, yend = stepping + 0.1)
    seg_args <- list(data = df, seg_aes)
    rec_aes <- aes(xmin = start, xmax = end, ymin = stepping - 0.1, ymax = stepping + 0.1)
    rec_args <- list(data = df, rec_aes)
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args),
                     scale_y_continuous(breaks = NULL))
    expected <- c(list(expected), list(facets), labels)
    expect_equal(test, expected)
})

test_that("Test rect.height parameter with stat = 'identity' of geom_rect", {
    test <- geom_rect(data, rect.height = 0.1, stat = "identity", aes(y = score))
    # simulate expected facets
    args <- list(rect.height = 0.1, stat = "identity", aes(y = score))
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    rect.height <- args$rect.height
    seg_aes <- list(x = as.name("start"),
                    y = substitute(y + rect.height, list(y = as.name("score"), rect.height= rect.height)),
                    xend = as.name("start"),
                    yend = substitute(y - rect.height, list(y = as.name("score"), rect.height= rect.height)))
    seg_aes <- do.call(aes, seg_aes)
    seg_args <- c(list(data = df), list(seg_aes, facets = args$facets))
    rec_aes <- list(y = as.name("score"),
                    xmin = as.name("start"),
                    xmax = as.name("end"),
                    ymin = substitute(y + rect.height, list(y = as.name("score"), rect.height = rect.height)),
                    ymax = substitute(y - rect.height, list(y = as.name("score"), rect.height = rect.height)))
    rec_aes <- do.call(aes, rec_aes)
    rec_args <- c(list(data = df), list(rec_aes, facets = args$facets))
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args))
    expected <- c(list(expected), list(facets), list(xlab("")))
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'stepping' of geom_rect", {
    test <- geom_rect(data, stat = "stepping", aes(y = score, group = pair), group.selfish = FALSE)
    # simulate expected facets
    args <- list(stat = "stepping", aes(y = score, group = pair), group.selfish = FALSE)
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    grl <-  biovizBase::splitByFacets(data, args$facets)
    res <- endoapply(grl, biovizBase::addStepping, extend.size = 0, group.selfish = FALSE, group.name = "pair")
    df <- biovizBase::mold(unlist(res))
    seg_aes <- aes(x = start, y = stepping - 0.4, xend = start, yend = stepping + 0.4)
    seg_args <- list(data = df, seg_aes)
    rec_aes <- aes(xmin = start, xmax = end, ymin = stepping - 0.4, ymax = stepping + 0.4)
    rec_args <- list(data = df, rec_aes)
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args),
                     scale_y_continuous(breaks = NULL))
    expected <- c(list(expected), list(facets), labels)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'identity' of geom_rect", {
    test <- geom_rect(data, stat = "identity", aes(y = score, group = pair), group.selfish = FALSE)
    # simulate expected facets
    args <- list(stat = "identity", aes(y = score, group = pair), group.selfish = FALSE)
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)
    # simulate expected data
    df <- biovizBase::mold(data)
    rect.height <- diff(range(values(data)[,as.character("score")]))/20
    seg_aes <- list(x = as.name("start"),
                    y = substitute(y + rect.height, list(y = as.name("score"), rect.height= rect.height)),
                    xend = as.name("start"),
                    yend = substitute(y - rect.height, list(y = as.name("score"), rect.height= rect.height)))
    seg_aes <- do.call(aes, seg_aes)
    seg_args <- c(list(data = df), list(seg_aes, facets = args$facets))
    rec_aes <- list(y = as.name("score"),
                    xmin = as.name("start"),
                    xmax = as.name("end"),
                    ymin = substitute(y + rect.height, list(y = as.name("score"), rect.height = rect.height)),
                    ymax = substitute(y - rect.height, list(y = as.name("score"), rect.height = rect.height)))
    rec_aes <- do.call(aes, rec_aes)
    rec_args <- c(list(data = df), list(rec_aes, facets = args$facets))
    # simulate expected return list
    expected <- list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args),
                     ggbio:::do.ggcall(ggplot2::geom_rect, rec_args))
    expected <- c(list(expected), list(facets), list(xlab("")))
    expect_equal(test, expected)
})
