context("plotRangesLinkedToData")

source('data.R')

# data initialization
data(genesymbol, package = "biovizBase")
exons <- exons(txdb)
exon17 <- subsetByOverlaps(exons, genesymbol["RBM17"])
exon.new <- reduce(exon17)
values(exon.new)$sample1 <- rnorm(length(exon.new), 10, 3)
values(exon.new)$sample2 <- rnorm(length(exon.new), 10, 10)
values(exon.new)$score <- rnorm(length(exon.new))
values(exon.new)$significant <- sample(c(TRUE,FALSE), size = length(exon.new),replace = TRUE)

test_that("Test error for invalid stat.y parameter of plotRangesLinkedToData(GenomicRanges_OR_GRangesList)", {
    data <- exon.new
    expect_error(plotRangesLinkedToData(data, stat.y = c("", "")))
    expect_error(plotRangesLinkedToData(data, stat.y = c(list(1))))
})

test_that("Test data representation of plotRangesLinkedToData(GenomicRanges_OR_GRangesList)", {
    data <- exon.new
    test <- plotRangesLinkedToData(data)

    test@grobs[[1]]@ggplot$plot_env <- NULL
    test@grobs[[2]]@ggplot$plot_env <- NULL
    test@grobs[[1]]@ggplot$plot_env <- NULL
    test@grobs[[2]]@ggplot$plot_env <- NULL
    test@plot[[1]]$plot_env <- NULL
    test@plot[[2]]$plot_env <- NULL
    test@plot[[3]]$plot_env <- NULL
    test@backup <- list()


    width.ratio <- 0.8
    annotation <- list()
    stat.y <- seq_len(ncol(mcols(data)))
    heights <- unit(c(2.5, 0.5, 1, rep(1, length(annotation))), "null")
    linetype <- 3
    stat.coord.trans <- coord_trans()
    theme.stat <- theme_gray()
    theme.align <- theme_gray()

    gr <- biovizBase::transformGRangesForEvenSpace(data)
    wd <- width(range(gr))
    N <- length(gr)
    wid <- wd/N/2 * width.ratio
    df <- as.data.frame(gr)
    stat.y <- stat.y + 5
    df.new <- reshape2::melt(df, measure.vars = stat.y)
    stat.label <- colnames(df)[stat.y]
    df.new$.ggbio.group <- factor(rep(stat.label, each = nrow(df)))
    p <- ggplot(df.new)

    args.aes.seg <- list(x = substitute(x.new-wid, list(wid = wid)),
                         xend = substitute(x.new+wid, list(wid = wid)),
                         color = substitute(.ggbio.group),
                         y = substitute(value))
    args.aes.seg$yend <- args.aes.seg$y
    aes.res.seg <- do.call(aes, args.aes.seg)
    p <- p + do.call(ggplot2::geom_segment, list(aes.res.seg))
    part <- PartitioningByWidth(rep(length(gr), nlevels(df.new$.ggbio.group)))

    df.dash <- data.frame(x = df.new[-end(part), "x.new"] + wid,
                          xend = df.new[-start(part), "x.new"] - wid,
                          y = df.new[-end(part), "value"],
                          yend = df.new[-start(part), "value"],
                          .ggbio.group = df.new[-end(part),
                          ".ggbio.group"])

    aes.dash <- aes(x = x, y = y, xend = xend, yend = yend, color = .ggbio.group)
    p.stat <- p + do.call(ggplot2::geom_segment, c(list(data = df.dash),
                          c(list(aes.dash)), linetype = linetype))

    p.stat <- p.stat  +  theme.stat + theme(panel.grid.minor=element_blank()) +
              labs(colour = "group") + stat.coord.trans
    df$midpoint <- (df$start + df$end)/2
    p.link <- ggplot(df)
    aes.link <- aes(x = midpoint, xend = x.new, y = 0, yend = 10)
    p.link <- p.link + ggplot2::geom_segment(aes.link) + theme_null()
    p.link <- p.link + theme(legend.position = "none")
    p.single <- autoplot(data, geom = "alignment") + theme.align +
                scale_y_continuous(breaks = NULL)
    p.link <- p.link +   theme(plot.margin = unit(c(0, 1, 0, 0.5), "lines"),
                               panel.margin = unit(c(0, 0.25, 0, 0.25), "lines"))
    args.tracks <- c(list(p.stat, p.link, p.single), list(heights = heights))
    expected <- do.call(tracks, args.tracks)

    expected@grobs[[1]]@ggplot$plot_env <- NULL
    expected@grobs[[2]]@ggplot$plot_env <- NULL
    expected@grobs[[1]]@ggplot$plot_env <- NULL
    expected@grobs[[2]]@ggplot$plot_env <- NULL
    expected@plot[[1]]$plot_env <- NULL
    expected@plot[[2]]$plot_env <- NULL
    expected@plot[[3]]$plot_env <- NULL
    expected@backup <- list()
    expect_equal(test, expected)
})

