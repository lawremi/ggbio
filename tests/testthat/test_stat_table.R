context("stat_table")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (geom, stat)
##                    + 1                            (default geom, stat)
## Total test cases = 3 + 3 => 6

# simulate testing data
source('data.R')

# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_table(GRanges)", {
    test <- stat_table(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab("x-axis"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_table(GRanges)", {
    test <- stat_table(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_table(GRanges)", {
    test <- stat_table(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_table_GRanges <- function(data, geom = NULL, stat = NULL) {
    args <- args.aes  <- args.non <- list()
    tab <- table(paste(seqnames(data), start(data), end(data), strand(data), sep = ":"))
    key_mat <- matrix(unlist(strsplit(names(tab), ":", fixed=TRUE)), 4)
    gr <- GRanges(key_mat[1,],
                  IRanges(as.integer(key_mat[2,]), as.integer(key_mat[3,])),
                  key_mat[4,], score = as.integer(tab),
                  seqlengths = seqlengths(data))
    seqinfo(gr) <- seqinfo(data)
    args.non$data <- gr
    if(is.null(stat) & is.null(geom)) {
        stat <- "stepping"
        args.non$geom <- "rect"
        args.non$stat <- stat
        args.aes$color <- args.aes$fill <- as.name("score")
        .fun <- ggbio:::stat_stepping
    } else {
        .fun <- ggbio:::getDrawFunFromGeomStat(geom, stat)
        if(!is.null(geom)) {
            if(geom != "arch") {
                if(is.null(stat))
                    args.non$stat <- stat <- "identity"
                else
                    args.non$geom <- geom
             }
        }
    }
    aes.res <- do.call(aes, args.aes)
    args.res <- c(args.non, list(aes.res))
    p <- ggbio:::do.ggcall(.fun, args.res)
    p <- c(p, list(xlab("")))
    p <- ggbio:::setStat(p)
}

test_that("Test geom parameter of stat_table(GRanges)", {
    test <- stat_table(data, geom = "bar")
    expected <- make_stat_table_GRanges(data, geom = "bar")
    expect_equal(test, expected)
})

test_that("Test stat parameter of stat_table(GRanges)", {
    test <- stat_table(data, stat = "coverage")
    expected <- make_stat_table_GRanges(data, stat = "coverage")
    expect_equal(test, expected)
})

test_that("Test default case of stat_table(GRanges)", {
    test <- stat_table(data)
    expected <- make_stat_table_GRanges(data)
    expect_equal(test, expected)
})

# ## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
# ##                    + 1                            (data representation)
# ## Total test cases = 3 + 1 => 4

# # Testing for GRangesList ----------------------------------------------------

test_that("Test xlab parameter of stat_table(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_table(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab(""),
                     xlab("x-axis"), ylab("Score"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_table(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_table(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab(""),
                     xlab("Genomic Coordinates"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_table(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_table(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), xlab(""), ylab(""), xlab(""),
                     xlab("Genomic Coordinates"), ylab("Score"), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_table_GRangesList <- function(data, geom = NULL) {
    args <- args.aes <- args.non <- list()
    aes.res <- do.call(aes, args.aes)
    gr <- biovizBase::flatGrl(data)
    args.non$data <- gr
    p <- do.call(stat_table, c(list(aes.res), args.non))
    p <- c(p, list(xlab("Genomic Coordinates")), list(ylab("Score")))
    p <- ggbio:::setStat(p)
}

test_that("Test default case of stat_table(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_table(data)
    expected <- make_stat_table_GRangesList(data)
    expect_equal(test, expected)
})
