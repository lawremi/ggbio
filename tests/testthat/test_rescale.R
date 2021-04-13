context("rescale")

test_that("Test rescale", {
    plot <- ggplot(data) + geom_rect()
    plot <- plot@ggplot

    test <- rescale(plot, xlim = c(1,2))
    expected <- plot + coord_cartesian(xlim = c(1,2))
    expect_equal(test, expected)

    test <- rescale(plot, ylim = c(1,2))
    expected <- plot + coord_cartesian(ylim = c(1,2))
    expect_equal(test, expected)

    test <- rescale(plot, sx = 12)
    xlim <- ggbio:::.getLimits(plot)$xlim
    xlim.mean <- mean(xlim)
    extra.new <- diff(xlim) * 12/2
    xlim <- c(xlim.mean - extra.new, xlim.mean + extra.new)
    expected <- plot + coord_cartesian(xlim = xlim)
    expect_equal(test, expected)

    test <- rescale(plot, sy = 12)
    ylim <- ggbio:::.getLimits(plot)$ylim
    ylim.mean <- mean(ylim)
    extra.new <- diff(ylim) * 12/2
    ylim <- c(ylim.mean - extra.new, ylim.mean + extra.new)
    expected <- plot + coord_cartesian(ylim = ylim)
    expect_equal(test, expected)
})
