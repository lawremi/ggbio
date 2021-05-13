context("Ideogram")

test_that("Test Ideogram()", {
    test <- Ideogram(genome = "hg19")
    test@ggplot$plot_env <- NULL
    data(ideoCyto, package = "biovizBase")
    obj <- ideoCyto[["hg19"]]
    obj.ori <- obj
    subchr <- sort(unique(as.character(seqnames(obj))))[1]
    obj <- obj[seqnames(obj) == subchr]
    obj <- keepSeqlevels(obj, subchr)
    p <- ggplot() + layout_karyogram(obj, cytobands = TRUE, geom = NULL)
    p <- ggbio:::applyTheme(p, FALSE, subchr, 1/20)

    expected <- new("Ideogram", ggbio(p, data = obj.ori),
        subchr = subchr, xlabel = FALSE,
        cytoband = TRUE, color = "red", fill = "red", alpha = 0.7,
        zoom.offset = 0.2, size = 1, aspect.ratio = 1/20,
        zoom.region = NULL)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test applyTheme()", {
    p <- ggplot()
    test <- ggbio:::applyTheme(p, FALSE, "chr1", 1/20)
    p <- p + theme_alignment(grid = FALSE, ylabel = TRUE, border = FALSE) +
            scale_y_continuous(breaks = 5, labels = "chr1") +
            theme(strip.background = element_rect(colour = 'NA', fill = 'NA')) +
            theme(strip.text.y = element_text(colour = 'white')) +
            theme(legend.position = "none") + ggplot2::xlab("") +
            theme(aspect.ratio = 1/20, axis.ticks.y = element_blank()) +
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    expected <- p
    expect_equal(test, expected)
})

test_that("Test adjustZoom()", {
    p <- ggplot()
    obj <- GRanges("chr1", IRanges(1, 100))
    test <- ggbio:::adjustZoom(obj, p, c(1, 5), 0.2, "red", "red", 1, 0.7)
    zoom.df <- data.frame(x1 = 1,
                          x2 = 5,
                          y1 = 0 - 0.2,
                          y2 = 10 + 0.2,
                          seqnames = unique(as.character(seqnames(obj))))
    expected <- p + ggplot2::geom_rect(data = zoom.df,
                                do.call(aes, list(xmin = substitute(x1),
                                                  xmax = substitute(x2),
                                                  ymin = substitute(y1),
                                                  ymax = substitute(y2))),
                                color = "red", fill = "red", size = 1,
                                alpha = 0.7)
    expect_equal(test, expected)
})

test_that("Test selectChromosome()", {
    obj <- GRanges(c("chr1", "chr2"), IRanges(c(1, 101), width = 100))
    test <- ggbio:::selectChromosome(obj, NULL)
    expected <- keepSeqlevels(obj[1], "chr1")
    expect_identical(test, expected)
})
