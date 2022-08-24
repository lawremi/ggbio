context("utils")

source('data.R')

test_that("Test getLegendGrob()", {
    plot <- ggbio()
    test <- ggbio:::getLegendGrob(plot)
    grob <- ggplotGrob(plot@ggplot)
    expected <- gtable::gtable_filter(grob, "guide-box")
    expect_equal(test, expected)
})

test_that("Test arrangeGrobByParsingLegend()", {
    data <- as.data.frame(data)
    p1 <- qplot(x=start, y = end, data=data, color = sample)
    p2 <- qplot(x=end, y = start, data=data, color = sample)
    args <- list(p1, p2)
    gg <- lapply(args, ggbio:::getLegendGrob)[1]
    lg <- lapply(args, function(x) {
        x <- x + theme(legend.position = "none", aspect.ratio = 1)
        ggplotGrob(x)
    })
    gg2 <- do.call(gridExtra::arrangeGrob, c(gg, list(ncol = 1)))
    expect_equal(arrangeGrobByParsingLegend(p1, p2, legend.idx = 1),
                 print(gridExtra::grid.arrange(do.call(gridExtra::arrangeGrob, lg),
                       gg2, ncol = 2, widths = c(4,1)))
                )
})

test_that("subsetByChrs(GRanges)", {
    test <- ggbio:::subsetByChrs(data)
    expected <- keepSeqlevels(data[seqnames(data) %in% "chr2"], "chr2")
    expect_equal(test, expected)
})

test_that("subsetByChrs(Seqinfo)", {
    data <- seqinfo(data)
    test <- ggbio:::subsetByChrs(data)
    expected <- data["chr2"]
    expect_equal(test, expected)
})

test_that("ggsave()", {
    ggplot() + geom_rect(data)
    on.exit(unlink("test.jpg"))
    ggsave("test.jpg")
    expect_identical(file.exists("test.jpg"), TRUE)
})

test_that("trans_seq()", {
    Mb <- ggbio:::trans_seq("Mb")
    kb <- ggbio:::trans_seq("kb")
    bp <- ggbio:::trans_seq("bp")
    expect_equal(Mb(1), 1e-06)
    expect_equal(kb(1), 0.001)
    expect_equal(bp(1), 1)
})

test_that("trans_seq_rev()", {
    Mb <- ggbio:::trans_seq_rev("Mb")
    kb <- ggbio:::trans_seq_rev("kb")
    bp <- ggbio:::trans_seq_rev("bp")
    expect_equal(Mb(1), 1e+06)
    expect_equal(kb(1), 1000)
    expect_equal(bp(1), 1)
})

test_that("trans_seq_format()", {
    Mb <- ggbio:::trans_seq_format("Mb")
    kb <- ggbio:::trans_seq_format("kb")
    bp <- ggbio:::trans_seq_format("bp")
    expect_equal(Mb(1), paste(1e-06, "Mb"))
    expect_equal(kb(1), paste(0.001, "kb"))
    expect_equal(bp(1), paste(1, "bp"))
})

test_that("need_color()", {
    expect_identical(ggbio:::need_color(list(color = "red")), FALSE)
    expect_identical(ggbio:::need_color(list()), TRUE)
})

test_that("highlight(GRanges)", {
    expect_error(ggbio:::highlight(data))
    data <- ggbio:::subsetByChrs(data, "chr1")
    test <- ggbio:::highlight(data)
    test <- lapply(test, function(x) {
                    x$geom_params$grob  <- NULL
                    x
    })
    ranges <- ranges(data)
    df <- data.frame(start(ranges), end(ranges))
    expected <- ggbio:::highlight(df, col = "red", fill = "red", alpha = 1)
    expected <- lapply(expected, function(x) {
                    x$geom_params$grob  <- NULL
                    x
    })
    expect_equal(test, expected)
})

test_that("highlight(numeric)", {
    test <- ggbio:::highlight(c(1,2))
    test$geom_params$grob <- NULL
    expected <- annotation_custom(grob = grid::rectGrob(gp =
                    grid::gpar(fill = "red", col = "red", alpha = 1)),
                    xmin = 1, xmax = 2, ymin = -Inf, ymax = Inf)
    expected$geom_params$grob <- NULL
    expect_equal(test, expected)
})
