context("Tracks")

source('data.R')
p1 <- ggplot() + stat_bin(xRle, type = "viewMeans")
p2 <- ggplot() + stat_bin(xRle, type = "viewSums")
.tracks.theme <- setdiff(slotNames("Tracks"), c("backup", "grobs"))

test_that("Test Tracks class constructor", {
    args <- list(p1, p2)
    test <- tracks(args)
    # building expected output
    plots  <- ggbio:::reduceListOfPlots(args)
    plots <- ggbio:::genPlots(plots)
    original_plots <- do.call(ggbio:::plotList, plots)
    PlotList <- do.call(ggbio:::PlotList, plots)
    # extract plot properties
    fixed <- vapply(PlotList, fixed, logical(1L))
    mutable <- vapply(PlotList, mutable, logical(1L))
    hasAxis <- vapply(PlotList, hasAxis, logical(1L))
    heights <- ggbio:::getHeight(PlotList)
    labeled <- vapply(PlotList, labeled, logical(1L))
    isIdeo <- vapply(PlotList, is, logical(1L), "Ideogram")
    isBlank <- vapply(PlotList, function(x) x@blank, logical(1L))
    ylim <- lapply(PlotList[!fixed & !isIdeo & !isBlank], function(grob) {
        scales::expand_range(ggbio:::getLimits(grob)$ylim, mul = 0.05)
    })
    # xlim
    xid <- !fixed & !isIdeo & !isBlank
    lst <- lapply(PlotList[xid], function(obj) {
        res <- ggbio:::getLimits(obj)
        data.frame(xmin = res$xlim[1], xmax = res$xlim[2])
    })
    res <- do.call(rbind, lst)
    xlim <- c(min(res$xmin), max(res$xmax))
    xlim <- scales::expand_range(xlim, mul = 0.1)

    # plot backgroud color
    track.plot.color <- vapply(PlotList, bgColor, character(1L))
    # set default values
    track.bg.color <- theme <- main <- xlab <- NULL
    main.height <- unit(1.5, "lines")
    scale.height <- unit(1, "lines")
    xlab.height <- unit(1.5, "lines")
    padding <- unit(-1, "lines")
    label.bg.color <- "white"
    label.bg.fill <- "gray80"
    label.text.color <- "black"
    label.text.cex <- 1
    label.text.angle <- 90
    label.width <- unit(2.5, "lines")
    backup <- list(grobs = PlotList, plot = original_plots, heights = heights,
                   xlim = xlim,  ylim = ylim, xlab = xlab, main = main,
                   main.height = main.height, scale.height = scale.height,
                   xlab.height = xlab.height, theme = theme, mutable = mutable,
                   hasAxis = hasAxis, fixed = fixed, padding = padding,
                   labeled = labeled, label.bg.color = label.bg.color,
                   label.bg.fill = label.bg.fill, label.text.color = label.text.color,
                   label.text.angle = label.text.angle, track.plot.color = track.plot.color,
                   track.bg.color = track.bg.color, label.text.cex = label.text.cex,
                   label.width = label.width)
    track_args <- list(backup = backup)
    track_args <- c("Tracks", track_args, backup)
    tracks <- do.call(new, track_args)
    ggplot2:::set_last_plot(tracks)
    expected <- tracks
    expect_equal(test, expected)
})

test_that("Test summary() of Tracks class", {
    tracks <- tracks(p1, p2)
    test <- summary(tracks)
    expected <- cat("-------------------------------------------\n
    Tracks contains: ", length(tracks@grobs), " graphic objects\n
    -------------------------------------------\n
    xlim:", tracks@xlim, "\n
    heights", tracks@heights, "\n
    fixed", tracks@fixed, "\n
    track.plot.color", tracks@track.plot.color, "\n
    -------------------------------------------\n")
    expect_identical(test, expected)
})

test_that("Test show() of Tracks class", {
    tracks <- tracks(p1, p2)
    test <- show(tracks)
    expected <- print(tracks)
    expect_identical(test, expected)
})

test_that("Test + (ANY) of Tracks class", {
    tracks <- tracks(p1, p2)
    test <- tracks + xlab("test")
    expected <- tracks
    e2 <- xlab("test")
    idx <- vapply(expected@grobs, mutable, logical(1L))
    N <- length(expected@grobs)
    for(i in (1:N)[idx]) {
        expected@grobs[[i]] <- expected@grobs[[i]] + e2
    }
    expect_equal(test, expected)
})

test_that("Test + (theme) of Tracks class", {
    tracks <- tracks(p1, p2)
    test <- tracks + theme_bw()
    expected <- tracks
    e2 <- theme_bw()
    idx <- vapply(expected@grobs, mutable, logical(1L))
    N <- length(expected@grobs)
    for(i in (1:N)[idx]) {
        expected@grobs[[i]] <- expected@grobs[[i]] + e2
    }
    expected@theme <- e2
    expect_equal(test, expected)
})

test_that("Test + (zoom) of Tracks class", {
    tracks <- tracks(p1, p2)
    test <- tracks + zoom(5)
    expected <- tracks
    e2 <- zoom(5)
    xlim <- expected@xlim
    expected@xlim <- ggbio:::.zoom(xlim, as.numeric(e2))$limits$x
    N <- length(expected@grobs)
    for(i in 1:N) {
        expected@grobs[[i]] <- expected@grobs[[i]] + e2
    }
    expect_equal(test, expected)
})

# test_that("Test + (position_c) of Tracks class", {
#     tracks <- tracks(p1, p2)
#     test <- tracks + scale_x_sequnit("bp")
#     expected <- tracks
#     e2 <- scale_x_sequnit("bp")
#     N <- length(expected@grobs)
#     for(i in 1:N) {
#         expected@grobs[[i]] <- expected@grobs[[i]] + e2
#     }
#     expect_equal(test, expected)
# })

# test_that("Test + (cartesian) of Tracks class", {
#     tracks <- tracks(p1, p2)
#     test <- tracks + coord_cartesian(xlim = c(1, 40), ylim = c(1, 40))
#     expected <- tracks
#     e2 <- coord_cartesian(xlim = c(1, 40), ylim = c(1, 40))
#     N <- length(expected@grobs)
#     for(i in 1:N) {
#         if(!fixed(expected@grobs[[i]]))
#             expected@grobs[[i]] <- expected@grobs[[i]] + e2
#     }
#     expect_equal(test, expected)
# })

test_that("Test xlim_car()", {
    x <- list(1)
    test <- ggbio:::xlim_car(x)
    class(x) <- c(class(x), "xlim")
    expected <- x
    expect_identical(test, expected)
})

test_that("Test xlim(numeric)", {
    test <- xlim(5)
    coord <- ggplot2::coord_cartesian(xlim = 5)
    expected <- ggbio:::xlim_car(coord)
    expect_equal(test, expected)
})

test_that("Test xlim(IRanges)", {
    test <- xlim(IRanges(1, 10))
    xlim <- c(1, 10)
    coord <- ggplot2::coord_cartesian(xlim = xlim)
    expected <- ggbio:::xlim_car(coord)
    expect_equal(test, expected)
})

test_that("Test xlim(GRanges)", {
    test <- xlim(GRanges("chr1", IRanges(1, 10)))
    xlim <- c(1, 10)
    coord <- ggplot2::coord_cartesian(xlim = xlim)
    chr <- "chr1"
    attr(coord, "chr") <- chr
    attr(coord, "ori") <- GRanges("chr1", IRanges(1, 10))
    expected <- ggbio:::xlim_car(coord)
    expect_equal(test, expected)
})

test_that("Test xlim(Tracks)", {
    tracks <- tracks(p1, p2)
    test <- xlim(tracks)
    expected <- tracks@xlim
    expect_identical(test, expected)
})

test_that("Test xlim(Tracks)<- for IRanges", {
    tracks <- tracks(p1, p2)
    xlim(tracks) <- IRanges(1, 10)
    test <- xlim(tracks)
    expected <- c(1, 10)
    expect_equal(test, expected)
})

test_that("Test xlim(Tracks)<- for GRanges", {
    tracks <- tracks(p1, p2)
    xlim(tracks) <- GRanges("chr1", IRanges(1, 10))
    test <- xlim(tracks)
    expected <- c(1, 10)
    expect_equal(test, expected)
})

test_that("Test xlim(Tracks)<- for numeric", {
    tracks <- tracks(p1, p2)
    xlim(tracks) <- c(5)
    test <- xlim(tracks)
    expected <- c(5, 5)
    expect_equal(test, expected)
})

test_that("Test reset(Tracks) of Tracks", {
    tracks <- tracks(p1, p2)
    test <- reset(tracks)
    names <- setdiff(slotNames(tracks), "backup")
    for(name in names)
        slot(tracks, name) <- tracks@backup[[name]]
    xlim(tracks) <- tracks@xlim
    expected <- tracks
    expect_equal(test, expected)
})

test_that("Test backup(Tracks) of Tracks", {
    tracks <- tracks(p1, p2)
    test <- backup(tracks)
    names <- setdiff(slotNames(tracks), "backup")
    for(name in names)
        tracks@backup[[name]] <- slot(tracks, name)
    expected <- tracks
    expect_equal(test, expected)
})

test_that("Test findGrobs()", {
    tracks <- tracks(p1, p2)
    grob <- as(tracks, "grob")
    test <- ggbio:::findGrobs(grob, "layout")
    expected <- rep(TRUE, 3)
    expect_equal(test, expected)
})

test_that("Test getPanelIndex()", {
    tracks <- tracks(p1, p2)
    # for better code coverage
    tracks@labeled <- rep(FALSE, 2)
    grob <- as(tracks, "grob")
    test <- ggbio:::getPanelIndex(grob)
    expected <- rep(FALSE, 3)
    expect_equal(test, expected)
})

test_that("Test getHeight()", {
    PlotList <- do.call(ggbio:::PlotList, list(p1, p2))
    test <- ggbio:::getHeight(PlotList)
    expected <- do.call(grid::unit.c, lapply(PlotList, height))
    expect_identical(test, expected)
})

test_that("Test parseHeight()", {
    PlotList <- do.call(ggbio:::PlotList, list(p1, p2))
    test <- ggbio:::parseHeight(5, length(PlotList))
    expected <- rep(unit(1, "null"), 2)
    expect_identical(test, expected)
})

test_that("Test + (Tracks) of Tracks class", {
    tracks_1 <- tracks(p1, p2)
    tracks_2 <- tracks(p2, p1)
    test <- tracks_1 + tracks_2
    expected <- c(tracks_1, tracks_2)
    expect_identical(test, expected)
})

test_that("Test c() for Tracks class", {
    tracks_1 <- tracks(p1)
    tracks_2 <- tracks(p2)
    args <- list(tracks_1, tracks_2)
    test <- c(tracks_1, tracks_2)
    list_of_grob <- lapply(args, function(x) x@grobs)
    expected <- do.call("tracks", do.call(c, list_of_grob))
    expect_equal(test, expected)
})

test_that("Test cbind() for Tracks class", {
    tracks_1 <- tracks(p1)
    tracks_2 <- tracks(p2)
    test <- cbind(tracks_1, tracks_2)
    list_of_grob <- lapply(list(tracks_1, tracks_2), as, "grob")
    res <- do.call(cbind, list_of_grob)
    expected <- grid::grid.draw(res)
    expect_equal(test, expected)
})

test_that("Test rbind() for Tracks class", {
    tracks_1 <- tracks(p1)
    tracks_2 <- tracks(p2)
    test <- rbind(tracks_1, tracks_2)
    list_of_grob <- lapply(list(tracks_1, tracks_2), as, "grob")
    res <- do.call(rbind, list_of_grob)
    expected <- grid::grid.draw(res)
    expect_equal(test, expected)
})

test_that("Test [ for Tracks class", {
    tracks <- tracks(p1)
    test <- tracks[1]
    expected <- tracks(p1)
    expect_equal(test, expected)
})
