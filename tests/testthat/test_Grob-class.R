context("Grob")

## isSupportedPlots
test_that("Test isSupportedPlots()", {
    obj <- list(ggplot())
    test <- ggbio:::isSupportedPlots(obj)
    expect_identical(test, TRUE)
})

## Grob

# gg
test_that("Test Grob(gg) of Grob class ", {
    gg <- ggplot()
    test <- ggbio:::Grob(gg)
    expected <- c("gtable", "gTree", "grob", "gDesc")
    expect_identical(class(test), expected)
})

# gtable
test_that("Test Grob(gtable) of Grob class ", {
    gtable <- gtable::gtable()
    test <- ggbio:::Grob(gtable)
    expected <- c("gtable", "gTree", "grob", "gDesc")
    expect_identical(class(test), expected)
})

# trellis
test_that("Test Grob(trellis) of Grob class ", {
    trellis <- lattice::barchart("")
    test <- ggbio:::Grob(trellis)
    expected <- c("lattice", "grob", "gDesc")
    expect_identical(class(test), expected)
})

# GGbio
test_that("Test Grob(GGbio) of Grob class ", {
    ggbio <- ggbio::ggbio()
    test <- ggbio:::Grob(ggbio)
    expected <- c("gtable", "gTree", "grob", "gDesc")
    expect_identical(class(test), expected)
})

# .validList
test_that("Test Grob(GGbio) of Grob class ", {
    p1 <- ggplot()
    p2 <- ggplot()
    PlotList <- ggbio:::PlotList(p1, p2)
    test <- ggbio:::.validList(PlotList)
    expect_identical(test, TRUE)
    expect_error(ggbio:::.validList(list(1, 2)))
})


# GrobList
g1 <- ggbio:::Grob(ggbio::ggbio())
g2 <- ggbio:::Grob(ggbio::ggbio())


test_that("Test GrobList", {
    test <- ggbio:::GrobList(g1, g2)
    expect_identical(test[[1]], g1)
    expect_identical(test[[2]], g2)
})

# reduceListOfPlots
test_that("Test reduceListOfPlots()", {
    grobList <- ggbio:::GrobList(list(g1))
    test <- ggbio:::reduceListOfPlots(grobList)
    expect_identical(test, g1)
})

# listOfGrobs
test_that("Test reduceListOfPlots()", {
    test <- ggbio:::listOfGrobs(list(g1, g2))
    expect_identical(class(test), "list")
    expect_identical(length(test), 2L)
})
