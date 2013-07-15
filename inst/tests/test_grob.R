context("Test Grob Class")
require(lattice)
require(testthat)
require(ggplot2)
require(gridExtra)
x = 1:3
p1 <- qplot(x = mpg, y = wt, data = mtcars)
p2 <- xyplot(1:10 ~ 1:10)
## require(ggbio)
## p3 <- GGbio(p1)
## p <- plot(1:10, 1:10)
## test_that("test grob", {
##   expect_that(p1, is_a("gg"))
##   expect_that(p2, is_a("trellis"))
##   expect_that(x, is_a("integer"))
##   expect_that(Grob(p1), is_a("Grob"))
##   expect_that(Grob(p1), is_a("ggplotGrob"))  
##   expect_that(Grob(p2), is_a("Grob"))
##   expect_that(Grob(p2), is_a("latticeGrob"))
##   expect_that(Grob(p3), is_a("Grob"))
##   expect_that(Grob(p3), is_a("ggplotGrob"))  
##   expect_that(Grob(p), throws_error())    
##   expect_that(GrobList(p1, p2), is_a("GrobList"))
##   expect_that(GrobList(p1, p2, x), gives_warning())
##   expect_that(length(GrobList(p1, p2, x)), equals(2))
## })


## context("Test Tracked Object")
## test_that("Test Tracked", {
##   obj <- new("Tracked")
##   height(obj) <- 1
##   expect_that(obj, is_a("Tracked"))
##   expect_that(height(obj), is_a("unit"))
##   expect_that(mutable(obj), is_a("logical"))
##   expect_that(fixed(obj), is_a("logical"))
##   expect_that(bgColor(obj), is_a("character"))
##   expect_that(labeled(obj), is_a("logical"))
##   expect_that(hasAxis(obj), is_a("logical"))
## })

## context("Test Cache Class")
## test_that("Test Cache Class", {
##   obj <- new("Cache")
##   expect_that(obj, is_a("Cache"))
##   expect_that(Cache(), is_a("Cache"))
##   cached_xlim(obj) <- 1
##   expect_that(cached_xlim(obj), equals(c(1, 1)))
##   cached_xlim(obj) <- c(1, 2, 3)
##   expect_that(cached_xlim(obj), equals(c(1, 3)))

##   cached_ylim(obj) <- 1
##   expect_that(cached_ylim(obj), equals(c(1, 1)))
##   cached_ylim(obj) <- c(1, 2, 3)
##   expect_that(cached_ylim(obj), equals(c(1, 3)))    
## })

## context("Test Plot Class")
## test_that("test Plot", {
##   expect_that(Plot(p1), is_a("Plot"))
##   expect_that(Plot(p2), is_a("Plot"))  
##   expect_that(PlotList(p1, p2), is_a("PlotList"))
##   expect_that(PlotList(p1, p2, x), gives_warning())
##   expect_that(length(PlotList(p1, p2, x)), equals(2))
## })


## context("Test GGbio Class")
## test_that("test Plot", {
##   expect_that(Plot(p1), is_a("Plot"))
##   expect_that(Plot(p2), is_a("Plot"))
##   expect_that(Plot(p3), is_a("Plot"))    
##   expect_that(PlotList(p1, p2, p3), is_a("PlotList"))
##   expect_that(PlotList(p1, p2, p3, x), gives_warning())
##   expect_that(length(PlotList(p1, p2, p3, x)), equals(3))
## })


