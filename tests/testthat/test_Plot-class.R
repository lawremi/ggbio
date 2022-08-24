context("Plot")

attr_list <- c("fixed", "labeled", "bgColor", "hasAxis", "mutable", "height")

filter_attr <- function(x) {
    idx <- names(attributes(x)) %in% attr_list
}

create_object <- function(class_name, x, idx, ...) {
    if(sum(idx)) {
        args <- attributes(x)[idx]
        args <- c(args, ...)
        obj <- do.call("new", c(class_name, list(x), args))
    } else {
        obj <- new(class_name, x, ...)
    }
}

set_geom_if_in_attr <- function(x, obj) {
  if("geom" %in% names(attributes(x)))
      attr(obj, "geom") <- attr(x, "geom")
  else obj
}

## ggbio
ggbio <- ggbio::ggbio()

GGbio_Plot <- function(x) {
    idx <- filter_attr(x)
    obj <- create_object("ggbioPlot", x, idx)
    obj <- set_geom_if_in_attr(x, obj)
    obj
}

test_that("Test Plot() of GGbio class", {
    expected <- GGbio_Plot(ggbio)
    expect_identical(ggbio:::Plot(ggbio), expected)
})

## gg
gg <- ggplot()

gg_Plot <- function(x) {
    ggbio <- ggbio::ggbio(x)
    GGbio_Plot(ggbio)
}

test_that("Test Plot() of gg class", {
    expected <- gg_Plot(gg)
    expect_identical(ggbio:::Plot(gg), expected)
})

## trellis
trellis <- lattice::barchart("")

trellis_Plot <- function(x) {
    idx <- filter_attr(x)
    obj <- create_object("latticePlot", x, idx, mutable = FALSE)
    obj <- set_geom_if_in_attr(x, obj)
    obj
}

test_that("Test Plot() of trellis class", {
    expected <- trellis_Plot(trellis)
    expect_identical(ggbio:::Plot(trellis), expected)
})

## Ideogram
ideo <- ggbio::Ideogram(genome = "hg19")

Ideo_Plot <- function(x) {
    new("ideogramPlot", x)
}

test_that("Test Plot() of trellis class", {
    expected <- Ideo_Plot(ideo)
    expect_identical(ggbio:::Plot(ideo), expected)
})

## PlotList
make_plotList <- function(...) {
    items <- list(...)
    items <- ggbio:::reduceListOfPlots(items)
}

make_PlotList <- function(...) {
    items <- list(...)
    items <- ggbio:::reduceListOfPlots(items)
    items <- lapply(items, Ideo_Plot)
    new("PlotList", items)
}

p1 <- ggbio:::Plot(ideo)
p2 <- ggbio:::Plot(ideo)
PlotList <- make_PlotList(p1, p2)

test_that("Test plotList()", {
    test <- ggbio:::plotList(p1, p2)
    expected <- make_plotList(p1, p2)
    expect_identical(test, expected)
})

test_that("Test PlotList()", {
    test <- ggbio:::PlotList(p1, p2)
    expected <- make_PlotList(p1, p2)
    expect_identical(test, expected)
})

test_that("Test c() for PlotList", {
    test <- c(PlotList, PlotList)
    expect_identical(length(test), 4L)
})

test_that("Test [ for PlotList", {
    test <- PlotList[1]
    expected <- make_PlotList(p1)
    expect_identical(test, expected)
})

# genPlots
test_that("Test genPlots()", {
    args <- list(p1, p2)
    test <- ggbio:::genPlots(args)
    expected <- args
    expect_identical(test, expected)
})
