context("geom_chevron")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(stat)                 ('stat' = c("stepping", "identity"))
##                     * length(other_parameters)     ('offset', 'facets', 'chevron.height.rescale', 'group.selfish')
## Total test cases = 3 + 2 * 4 => 11

## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_chevron", {
    test <- geom_chevron(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_chevron", {
    test <- geom_chevron(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_chevron", {
    test <- geom_chevron(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

getY <- function(n, offset){
    switch(n,
            {
                y.offset <- 0
                yend.offset <- offset
                data.frame(y.offset = y.offset,
                            yend.offset = yend.offset)
            },
            {
                y.offset <- offset
                yend.offset <- 0
                data.frame(y.offset = y.offset,
                            yend.offset = yend.offset)
            })
}

make_chevron <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    offset <- args_non$offset
    chevron.height.rescale <- args_non$chevron.height.rescale
    group.selfish <- args_non$group.selfish

    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)

    # simulate expected data
    if (identical(stat, "stepping")) {
        if ("group" %in% names(args_aes)) {
            data <- biovizBase::addStepping(data, group.name = quo_name(args_aes$group),
                                            group.selfish = group.selfish)
        } else {
            data <- biovizBase::addStepping(data)
        }
        data.new <- ggbio:::breakGr(data)
        names(data.new) <- NULL
        df <- as.data.frame(data.new)
        ydf <- do.call(rbind, lapply(df$.bioviz.chevron, getY, offset))
        df <- cbind(df, ydf)
        aes_lst <- args_aes
        args <- c(aes_lst, list(x = substitute(start),
                                xend = substitute(end),
                                y = substitute(stepping + y.offset),
                                yend = substitute(stepping + yend.offset)))
        seg_args <- c(list(data = df), list(do.call(aes, args)))
        seg <- ggbio:::do.ggcall(ggplot2::geom_segment, seg_args)
        p <- c(list(seg), list(ggplot2::ylab("")))
        if("group" %in% names(args_aes))
            gpn <- quo_name(args_aes$group)
        else gpn <- "stepping"

        .df.lvs <- unique(df$stepping)
        .df.sub <- df[, c("stepping", gpn)]
        .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]

        if(gpn != "stepping" & group.selfish) {
            p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                               labels = as.character(.df.sub[, gpn]))))
        } else {
            p <- c(p, list(scale_y_continuous(breaks = NULL)))
        }
    }

    if (identical(stat, "identity")) {
        .y <- args_aes$y
        args_aes$x <- as.name("start")
        args_aes$xend <- as.name("end")
        args_aes$y <- substitute(y + offset,
                                 list(y = .y, offset = as.name("y.offset")))
        args_aes$yend <- substitute(yend + offset ,
                                    list(yend = .y, offset = as.name("yend.offset")))
        data.new <- ggbio:::breakGr(data)
        names(data.new) <- NULL
        df <- as.data.frame(data.new)
        ydf <- do.call(rbind, lapply(df$.bioviz.chevron, getY, offset))
        df <- cbind(df, ydf)

        .y <- args_aes$y
        .yend <- args_aes$yend
        args_aes$y <- substitute(y + y.offset, list(y = .y))
        args_aes$yend <- substitute(yend + yend.offset, list(yend = .yend))
        seg_args <- c(list(data = df), list(do.call(aes, args_aes)))
        p <- c(list(ggbio:::do.ggcall(ggplot2::geom_segment, seg_args)),
               list(ggplot2::ylab("")))
    }

    p <- c(list(p), list(facets), list(ggplot2::xlab("")))
}

test_that("Test offset parameter with stat = 'stepping' of geom_chevron", {
    test <- geom_chevron(data, offset = 0.5)
    args <- list(offset = 0.5, stat = "stepping",
                 chevron.height.rescale = c(0.1, 0.8), group.selfish = TRUE)
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test offset parameter with stat = 'identity' of geom_chevron", {
    test <- geom_chevron(data, offset = 0.5, stat = "identity", aes(y = value))
    args <- list(offset = 0.5, stat = "identity", chevron.height.rescale = c(0.1, 0.8),
                 group.selfish = TRUE, aes(y = value))
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'stepping' of geom_chevron", {
    test <- geom_chevron(data, facets = sample ~ seqnames)
    args <- list(offset = 0.1, stat = "stepping", chevron.height.rescale = c(0.1, 0.8),
                 group.selfish = TRUE, facets = sample ~ seqnames)
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test facets parameter with stat = 'identity' of geom_chevron", {
    test <- geom_chevron(data, stat = "identity", aes(y = value), facets = sample ~ seqnames)
    args <- list(offset = 0.1, stat = "identity", chevron.height.rescale = c(0.1, 0.8),
                 group.selfish = TRUE, aes(y = value), facets = sample ~ seqnames)
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test chevron.height.rescale parameter with stat = 'stepping' of geom_chevron", {
    test <- geom_chevron(data, chevron.height.rescale = c(0.5, 0.5))
    args <- list(offset = 0.1, stat = "stepping", chevron.height.rescale = c(0.5, 0.5),
                 group.selfish = TRUE)
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test chevron.height.rescale parameter with stat = 'identity' of geom_chevron", {
    test <- geom_chevron(data, stat = "identity", aes(y = value), chevron.height.rescale = c(0.1, 0.8))
    args <- list(offset = 0.1, stat = "identity", chevron.height.rescale = c(0.5, 0.5),
                 group.selfish = TRUE, aes(y = value))
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'stepping' of geom_chevron", {
    test <- geom_chevron(data, group.selfish = FALSE)
    args <- list(offset = 0.1, stat = "stepping", chevron.height.rescale = c(0.1, 0.8),
                 group.selfish = FALSE)
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish parameter with stat = 'identity' of geom_chevron", {
    test <- geom_chevron(data, stat = "identity", aes(y = value, group = pair), group.selfish = FALSE)
    args <- list(offset = 0.1, stat = "identity", chevron.height.rescale = c(0.1, 0.8),
                 group.selfish = FALSE, aes(y = value, group = pair))
    expected <- make_chevron(data, args)
    expect_equal(test, expected)
})
