context("geom_alignment")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(range.geom)           (range.geom = c("rect", "arrowrect"))
##                     * length(other_parameters)     ('facets', 'rect.height', 'group.selfish')
##                     + length(gap.geom) - 1         (gap.geom = c("chevron", "arrow", "segment"))
##                     * length(other_parameters)     ('facets', 'rect.height', 'group.selfish')
## Total test cases = 3 + (2 * 3) + (2 * 3) => 15
##


## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_GRanges_alignment <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    range.geom <- args_non$range.geom
    gap.geom <- args_non$gap.geom
    rect.height <- args_non$rect.height
    group.selfish <- args_non$group.selfish
    if (is.null(group.selfish))
        group.selfish <- TRUE

    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)

    if(is.null(rect.height))
        rect.height <- 0.4
    args_non$rect.height <- rect.height

    main_fun <- switch(range.geom,
                       rect = {
                           geom_rect
                       },
                       arrowrect = {
                           geom_arrowrect
                       })

    gap_fun <- switch(gap.geom,
                      chevron = {
                          geom_chevron
                      },
                      arrow = {
                          geom_arrow
                      },
                      segment = {
                          geom_segment
                      })

    # simulate expected data
    if (identical(stat, "stepping")) {
        grl <-  biovizBase::splitByFacets(data, args$facets)
        if("group" %in% names(args_aes)) {
            gpn <- quo_name(args_aes$group)
            res <- endoapply(grl, biovizBase::addStepping, group.name = quo_name(args_aes$group),
                             group.selfish = group.selfish,
                             extend.size = 0)
        } else {
            gpn <- "stepping"
            res <- endoapply(grl, biovizBase::addStepping, extend.size = 0)
        }
        res <- unlist(res)
        df <- biovizBase::mold(res)
        gps <- biovizBase::getGaps(res, group.name = gpn, args$facets)
        if (length(gps)) {
            gps <- GenomeInfoDb::keepSeqlevels(gps, names(seqlengths(res)))
            args_gaps <- args_aes[!names(args_aes) %in% c("x", "y", "xend", "yend",
                                                          "label.type", "label.size",
                                                          "label.color", "size",
                                                          "fill", "color", "colour")]
            args_gaps$y <- as.name("stepping")
            aes_lst <- do.call("aes", args_gaps)
            gps_lst <- c(list(aes_lst), list(data = gps, stat = "identity"))
            p <- list(ggbio:::do.ggcall(gap_fun, gps_lst))
        } else {
            p <- NULL
        }
        ## plot main
        args_aes$y <- as.name("stepping")
        args_aes <- args_aes[names(args_aes) != "size"]
        args_non$stat = "identity"
        aes <- do.call(aes, args_aes)
        args_res <- c(list(data = res), list(aes), args_non)

        p <- c(p, list(do.call(main_fun, args_res)))
        # p <- .changeStrandColor(p, args_aes)
        .df.lvs <- unique(df$stepping)
        .df.sub <- df[, c("stepping", gpn)]
        .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
        if(gpn != "stepping" & group.selfish)
            p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                               labels = as.character(.df.sub[, gpn]))))
        else
            p <- c(p, list(scale_y_continuous(breaks = NULL)))
    }
    p <- c(list(p), list(facets), list(xlab("")), list(ylab("")))
}

test_that("Test facets with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", rect.height = 0.5)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", group.selfish = FALSE)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, gap.geom = "arrow", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5, gap.geom = "arrow")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE, gap.geom = "arrow")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, gap.geom = "segment", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5, gap.geom = "segment")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE, gap.geom = "segment")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})
