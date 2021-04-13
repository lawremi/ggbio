context("plotGrandLinear")

source('data.R')

test_that("Test xlab parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value), xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value), ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value), main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test error when y is not provided for plotGrandLinear(GRanges)", {
    expect_error(plotGrandLinear(data))
})

test_that("Test color parameter of plotGrandLinear(GRanges)", {
    # when color is passed in aes args
    test <- plotGrandLinear(data, aes(y = value, color = seqnames))
    test@ggplot$plot_env <- NULL
    xlab <- ""
    aes.res <- aes(y = value, color = seqnames)
    args.non <- list(coord = "genome", space.skip = 0.01,
                geom = "point", object = data)
    p <- do.call(ggbio:::autoplot, c(list(aes.res), args.non))
    expected <- p + theme(legend.position = "none") +
                theme(panel.grid.minor=element_blank()) + ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # when color is passed in non aes args
    test <- plotGrandLinear(data, aes(y = value), color = c("red", "blue"))
    test@ggplot$plot_env <- NULL
    xlab <- ""
    aes.res <- aes(y = value, color = seqnames)
    args.non <- list(coord = "genome", space.skip = 0.01,
                     geom = "point", object = data)
    p <- do.call(ggbio:::autoplot, c(list(aes.res), args.non))
    p <- p + theme(legend.position = "none")
    .color <- c("red", "blue")
    chrs <- names(seqlengths(data))
    N <- length(chrs)
    cols <- rep(.color, round(N / length(.color)) + 1)[1:N]
    names(cols) <- chrs
    p <- p + scale_color_manual(values = cols)
    expected <- p + theme(panel.grid.minor=element_blank()) +
                ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test facet parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value), facets = sample ~ seqnames)
    test@ggplot$plot_env <- NULL
    xlab <- ""
    aes.res <- aes(y = value, color = seqnames)
    args.non <- list(coord = "genome", space.skip = 0.01,
                     geom = "point", object = data)
    p <- do.call(ggbio:::autoplot, c(list(aes.res), args.non))
    p <- p + theme(legend.position = "none")
    .color <- c("#0080FF", "#4CC4FF")
    chrs <- names(seqlengths(data))
    N <- length(chrs)
    cols <- rep(.color, round(N / length(.color)) + 1)[1:N]
    names(cols) <- chrs
    p <- p + scale_color_manual(values = cols)
    facets <- sample ~ seqnames
    args <- list(facets = facets)
    args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- ggbio:::.buildFacetsFromArgs(data, args.facets)
    p <- p + facet
    expected <- p + theme(panel.grid.minor=element_blank()) +
                ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test spaceline parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value), spaceline = TRUE)
    test@ggplot$plot_env <- NULL
    xlab <- ""
    aes.res <- aes(y = value, color = seqnames)
    args.non <- list(coord = "genome", space.skip = 0.01,
                     geom = "point", object = data, spaceline = TRUE)
    p <- do.call(ggbio:::autoplot, c(list(aes.res), args.non))
    p <- p + theme(legend.position = "none")
    .color <- c("red", "blue")
    chrs <- names(seqlengths(data))
    N <- length(chrs)
    cols <- rep(.color, round(N / length(.color)) + 1)[1:N]
    names(cols) <- chrs
    p <- p + scale_color_manual(values = cols)
    p <- p + theme(panel.grid.minor=element_blank())

    vline.df <- p@ggplot$data
    vline.df <- do.call(rbind, by(vline.df, vline.df$seqnames, function(dd) {
        data.frame(start = min(dd$start), end = max(dd$end))
    }))
    gap <- (vline.df$start[-1] + vline.df$end[-nrow(vline.df)]) / 2
    p <- p + geom_vline(xintercept = gap, alpha = 0.5, color = 'gray70') +
         theme(panel.grid = element_blank())

    expected <- p + ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test highlight.* parameter of plotGrandLinear(GRanges)", {
    test <- plotGrandLinear(data, aes(y = value),
                            highlight.gr = data[1:10],
                            highlight.name = "value")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    aes.res <- aes(y = value, color = seqnames)
    args.non <- list(highlight.gr = data[1:10], highlight.name = "value",
                     coord = "genome", space.skip = 0.01,
                     geom = "point", object = data)
    p <- do.call(ggbio:::autoplot, c(list(aes.res), args.non))
    p <- p + theme(legend.position = "none")
    .color <- c("#0080FF", "#4CC4FF")
    chrs <- names(seqlengths(data))
    N <- length(chrs)
    cols <- rep(.color, round(N / length(.color)) + 1)[1:N]
    names(cols) <- chrs
    p <- p + scale_color_manual(values = cols)
    p <- p + theme(panel.grid.minor=element_blank())

    highlight.gr <- data[1:10]
    highlight.name <- "value"
    highlight.col <- "red"
    highlight.label.size <- 5
    highlight.label.col <- "black"
    highlight.label.offset <- 0.05
    mold <- biovizBase::mold

    args.aes <- list(y = as.name("value"))
    idx <- findOverlaps(data, highlight.gr)
    .h.pos <- lapply(split(queryHits(idx), subjectHits(idx)), function(id) {
        gr <- GRanges(as.character(seqnames(p@data))[id][1],
                      IRanges(start = min(start(p@data[id])),
                              end = max(end(p@data[id]))))
        val <- max(as.numeric(values(p@data[id])[,"value"]))
        val <- val * (1 + highlight.label.offset)
        values(gr)$val <- val
        gr
    })
    .h.pos <- suppressWarnings(do.call("c", unname(.h.pos)))
    highlight.name <- values(highlight.gr)[,highlight.name]

    p <- p +  geom_point(data = mold(p@data[queryHits(idx)]),
                         do.call(aes, list(x = substitute(midpoint),
                                 y = args.aes$y)),
                         color = highlight.col)

    seqlevels(.h.pos, pruning.mode="coarse") <- seqlevels(data)
    suppressWarnings(seqinfo(.h.pos) <- seqinfo(data))
    .trans <- biovizBase::transformToGenome(.h.pos, space.skip = 0.01)
    values(.trans)$mean <- (start(.trans) + end(.trans))/2
    values(.trans)$names <- highlight.name
    p <- p + geom_text(data = mold(.trans), size = highlight.label.size,
                       vjust = 0, color = highlight.label.col,
                       do.call(aes, list(x = substitute(mean),
                                         y = as.name("val"),
                                         label = as.name("names"))))
    expected <- p + ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})
