context("layout_circle")

source('data.R')

test_that("Test geom = 'ideogram' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "ideogram")
    data <- biovizBase::getIdeoGR(data)
    res <- biovizBase::transformToRectInCircle(data,
                            space.skip = 0.015, trackWidth = 5,
                            radius = 10, direction = "clockwise",
                            n = 60, chr.weight = NULL)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes <- aes(y = .circle.y, x = .circle.x, group = .biovizBase.group)
    args <- c(list(data = df, aes), list(color = I("black")))
    res <- ggbio:::do.ggcall(geom_polygon, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'text' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "text", aes(y = score, label = "start"))
    obj <- biovizBase::transformToGenome(data, 0.015, chr.weight = NULL)
    obj <- biovizBase::transformToCircle(obj, y = "score",
                            ylim = NULL, radius = 10, trackWidth = 5,
                            direction = "clockwise")

    ags <-  - values(obj)$.circle.angle * 180/pi
    values(obj)$.processed.angle <- ags
    names(obj) <- NULL
    df <- as.data.frame(obj)
    aes <- aes(label = "start", angle = .processed.angle,
               y = .circle.y, x = .circle.x)
    args <- c(list(data = df, aes))
    res <- ggbio:::do.ggcall(geom_text, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'point' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "point", aes(y = score))
    obj <- biovizBase::transformToGenome(data, 0.015, chr.weight = NULL)
    obj <- biovizBase::transformToCircle(obj, y = "score",
                            ylim = NULL, radius = 10, trackWidth = 5,
                            direction = "clockwise")
    names(obj) <- NULL
    df <- as.data.frame(obj)
    aes <- aes(y = .circle.y, x = .circle.x)
    args <- c(list(data = df, aes))
    res <- ggbio:::do.ggcall(geom_point, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'line' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "line", aes(y = score))
    obj <- biovizBase::transformToGenome(data, 0.015, chr.weight = NULL)
    obj <- biovizBase::transformToCircle(obj, y = "score",
                            ylim = NULL, radius = 10, trackWidth = 5,
                            direction = "clockwise")
    names(obj) <- NULL
    df <- as.data.frame(obj)
    aes <- aes(y = .circle.y, x = .circle.x, group = seqnames)
    args <- c(list(data = df, aes))
    res <- ggbio:::do.ggcall(geom_path, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'segment' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "segment")
    res <- biovizBase::transformToSegInCircle(data,
                            space.skip = 0.015,
                            trackWidth = 5, radius = 10,
                            direction = "clockwise",
                            chr.weight = NULL)
    names(res) <- NULL
    df <- as.data.frame(res)
    aes <- aes(y = .circle.y, x = .circle.x, group = .biovizBase.group)
    args <- c(list(data = df, aes))
    res <- ggbio:::do.ggcall(geom_path, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'heatmap' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "heatmap")
    res <- biovizBase:::transformToSegInCircle2(data,
                            space.skip = 0.015,
                            trackWidth = 5, radius = 10,
                            direction = "clockwise", n = 60,
                            chr.weight = NULL)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes <- aes(y = .circle.y, x = .circle.x, group = .biovizBase.group)
    args <- c(list(data = df, aes))
    res <- ggbio:::do.ggcall(geom_path, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'scale' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "scale")
    res <- biovizBase::getIdeoGR(data)
    res <- biovizBase::getScale(res, NULL, n = 60, "M")
    values(res)$.biovizBase.group <- seq_len(length(res))
    res0 <- res
    values(res0)$scale.y <- 0
    values(res0)$.biovizBase.group <- seq_len(length(res0))
    res <- c(res, res0)
    res <- biovizBase::transformToGenome(res, 0.015, chr.weight = NULL)
    res <- biovizBase::transformToCircle(res, y = "scale.y",
                            radius = 10, trackWidth = 5,
                            ylim = NULL, direction = "clockwise")
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group)
    df <- df[idx, ]
    N <- nrow(df)
    res <- df[seq(1, N-1, by = 2),]
    res[,c(".circle.xend", ".circle.yend")] <-
    df[seq(2, N, by = 2), c(".circle.x", ".circle.y")]
    ags <- 90 - res$.circle.angle * 180/pi
    res$.processed.angle <- ags
    aes <- aes(y = .circle.y, x = .circle.x,
               yend = .circle.yend, xend = .circle.xend)
    aes.text <- aes(y = .circle.y, x = .circle.x,
                    angle = .processed.angle, label = text.major)
    args <- c(list(data = res), list(aes.text), list(hjust = 0, size = 3))
    res.text <- ggbio:::do.ggcall(geom_text, args)
    res.seg <- do.call(ggplot2::geom_segment,c(list(data = res), list(aes)))
    p <- c(list(res.text), list(res.seg))
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'rect' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "rect")
    res <- biovizBase::transformToRectInCircle(data,
                            space.skip = 0.015,
                            trackWidth = 5, radius = 10,
                            direction = "clockwise", n = 60,
                            chr.weight = NULL)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes <- aes(y = .circle.y, x = .circle.x, group = .biovizBase.group)
    args <- c(list(data = df, aes), list(color = "black", fill = "black"))
    res <- ggbio:::do.ggcall(geom_polygon, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'bar' of layout_circle(GRanges)", {
    test <- layout_circle(data, geom = "bar")
    res <- biovizBase::transformToBarInCircle(data,
                            space.skip = 0.015,
                            trackWidth = 5, radius = 10,
                            direction = "clockwise", n = 60,
                            chr.weight = NULL)
    names(res) <- NULL
    df <- as.data.frame(res)
    idx <- order(df$.biovizBase.group, df$.int.id)
    df <- df[idx, ]
    aes <- aes(y = .circle.y, x = .circle.x, group = .biovizBase.group)
    args <- c(list(data = df, aes), list(color = "black", fill = "black"))
    res <- ggbio:::do.ggcall(geom_polygon, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'link' of layout_circle(GRanges)", {
    seqlengths(data) <- c(400, 500, 700)
    values(data)$to.gr <- data[sample(1:length(data), size = length(data))]
    test <- layout_circle(data, geom = "link", linked.to = "to.gr")
    link.fun <- function(x, y, n = 30) Hmisc::bezier(x, y, evaluation = n)
    res <- biovizBase::transformToLinkInCircle(data, space.skip = 0.015,
                linked.to = "to.gr", link.fun = link.fun, trackWidth = 5,
                radius = 10, direction = "clockwise", chr.weight = NULL)
    aes <- aes(y = y, x = x, group = .biovizBase.group)
    args <- list(data = res, aes)
    res <- ggbio:::do.ggcall(geom_path, args)
    p <- list(res)
    expected <- c(p, list(theme_null(), theme(aspect.ratio = 1)))
    expect_equal(test, expected)
})

test_that("Test geom = 'ribbon' of layout_circle(GRanges)", {
    expect_error(layout_circle(data, geom = "ribbon"))
})

test_that("Test geom = 'hits' of layout_circle(GRanges)", {
    expect_error(layout_circle(data, geom = "hits"))
})
