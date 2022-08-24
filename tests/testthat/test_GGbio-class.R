context("GGbio")

source('data.R')

test_that("Test GGbio object representation", {
    args <- list("GGbio", data = NULL, fetchable = FALSE, blank = FALSE)

    # test with args for ggplot object
    ggplot <- ggplot()
    test_with_args <- ggbio(ggplot, trackWidth = 1, buffer = 2, radius = 3)
    args[["ggplot"]] <- ggplot
    expected_with_args <- do.call(new, args)
    attr(expected_with_args, "trackWidth") <- 1
    attr(expected_with_args, "buffer") <- 2
    attr(expected_with_args, "radius") <- 3
    expect_identical(test_with_args, expected_with_args)

    # test without args for ggbio object
    ggbio <- ggbio()
    test_without_args <- ggbio(ggbio)
    args[["ggplot"]] <- ggbio@ggplot
    expected_without_args <- do.call(new, args)
    attr(expected_without_args, "trackWidth") <- 7
    attr(expected_without_args, "buffer") <- 2
    attr(expected_without_args, "radius") <- 30
    expect_identical(test_without_args, expected_without_args)
})

test_that("Test $ operator of GGbio class", {
    df <- data.frame(x = 1:3, y = 3:5)
    ggplot <- ggplot(df, aes(x = x))
    ggbio <- ggbio(ggplot)
    test <- ggbio$mapping
    expected <- aes(x = x)
    expect_identical(test, expected)
})

test_that("Test $ replace operator of GGbio class", {
    df <- data.frame(x = 1:3, y = 3:5)
    ggplot <- ggplot(df, aes(x = x))
    ggbio <- ggbio(ggplot)
    ggbio$mapping <- aes(y = y)
    test <- ggbio$mapping
    expected <- aes(y = y)
    expect_identical(test, expected)
})

test_that("Test '+ circle()' of GGbio class", {
    trackWidth <- 5

    # check object structure with implicit default radius value
    test <- ggbio() + circle(data = data, aes(y = "score"))
    ggbio <- ggbio()
    circle <- circle(data = data, aes(y = "score"))
    r.cur <- attr(ggbio, "radius") + trackWidth + attr(ggbio, "buffer")
    attr(ggbio, "radius") <- r.cur
    circle$radius <- r.cur
    circle$trackWidth <- trackWidth
    object <- do.call(layout_circle, circle)
    ggbio@ggplot <- ggbio:::mapToGG(ggbio@ggplot, object)
    ggbio@ggplot <- ggplot2:::add_ggplot(ggbio@ggplot, object, "circle")
    expected <- ggbio
    expect_equal(test, expected)

    # check object structure with explicit radius value
    test_with_radius <- ggbio() + circle(data = data, aes(y = "score"), radius = 30)
    ggbio <- ggbio()
    circle <- circle(data = data, aes(y = "score"), radius = 30)
    r.cur <- 30
    attr(ggbio, "radius") <- max(attr(ggbio, "radius"),  r.cur + trackWidth)
    circle$radius <- r.cur
    circle$trackWidth <- trackWidth
    object <- do.call(layout_circle, circle)
    ggbio@ggplot <- ggbio:::mapToGG(ggbio@ggplot, object)
    ggbio@ggplot <- ggplot2:::add_ggplot(ggbio@ggplot, object, "circle")
    expected <- ggbio
    expect_equal(test_with_radius, expected)

    # no data error
    expect_error(ggbio() + circle())
})

test_that("Test '+ zoom()' of GGbio class", {
    ggplot <- ggplot() + geom_rect(data)
    test <- ggbio(ggplot) + zoom(10)
    ggbio <- ggbio(ggplot)
    xlim <- ggbio:::.getLimits(ggbio@ggplot)$xlim
    expected <- ggbio + ggbio:::.zoom(xlim, 10)
    expect_equal(test, expected)
})

test_that("Test '+ nextView()' of GGbio class", {
    # unit = view
    ggplot <- ggplot() + geom_rect(data)
    test <- ggbio(ggplot) + nextView()
    ggbio <- ggbio(ggplot() + geom_rect(data))
    xlim <- ggbio:::.getLimits(ggbio@ggplot)$xlim
    xlim <- c(max(xlim), max(xlim) + abs(diff(xlim)))
    expected <- ggbio + xlim(xlim)
    expect_equal(test, expected)

    # unit =  gene
    expect_error(nextView("gene"))

    # unit =  exon
    expect_error(nextView("exon"))

    # unit =  utr
    expect_error(nextView("utr"))
})

test_that("Test '+ prevView()' of GGbio class", {
    # unit = view
    ggplot <- ggplot() + geom_rect(data)
    test <- ggbio(ggplot) + prevView()
    ggbio <- ggbio(ggplot() + geom_rect(data))
    xlim <- ggbio:::.getLimits(ggbio@ggplot)$xlim
    xlim <- c(min(xlim) -abs(diff(xlim)), min(xlim))
    expected <- ggbio + xlim(xlim)
    expect_equal(test, expected)

    # unit =  gene
    expect_error(prevView("gene"))

    # unit =  exon
    expect_error(prevView("exon"))

    # unit =  utr
    expect_error(prevView("utr"))
})

test_that("Test setStat()", {
    x <- list(1)
    test <- ggbio:::setStat(x)
    attr(x, "isStat") <- TRUE
    expected <- x
    expect_identical(test, expected)
})

test_that("Test isStat()", {
    x <- list(1)
    test <- ggbio:::setStat(x)
    expect_identical(ggbio:::isStat(test), TRUE)
    expect_identical(ggbio:::isStat(x), FALSE)
})

test_that("Test mapToGG()", {
    ggplot <- ggplot()
    object <- stat_coverage(data)
    test <- ggbio:::mapToGG(ggplot, object)
    protos <- ggbio:::returnProto(object)
    ggplot$mapping <- protos[[1]]$mapping
    ggplot$data <- protos[[1]]$data
    expected <- ggplot
    expect_equal(test, expected)
})

test_that("Test returnProto()", {
    ggplot <- ggplot()
    test <- ggbio:::returnProto(ggplot)
    expected <- rapply(ggplot, function(x) x, c("proto", "ggproto"),
                       how = "unlist")
    expect_equal(test, expected)
})

test_that("Test zoom_in()", {
    ggplot <- ggplot() + geom_rect(data)
    test <- ggbio(ggplot) + zoom_in(5)
    ggbio <- ggbio(ggplot) + zoom(1/5)
    expected <- ggbio
    expect_equal(test, expected)
})

test_that("Test zoom_out()", {
    ggplot <- ggplot() + geom_rect(data)
    test <- ggbio(ggplot) + zoom_out(-2)
    ggbio <- ggbio(ggplot) + zoom(1/-2)
    expected <- ggbio
    expect_equal(test, expected)
})
