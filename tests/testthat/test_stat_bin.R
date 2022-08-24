context("stat_bin")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(geom)                 (geom = c("bar", "heatmap"))
##                    * length(others)               (binwidth, nbin)
##                    + length(type)                 (type = c("viewSums","viewMins", "viewMaxs", "viewMeans"))
##                    * length(others)               (binwidth, nbin)
## Total test cases = 3 + (2 * 2) + (4 * 2) => 15

# simulate testing data
source('data.R')

source('test-generator.R')

# common
geoms <- list("bar", "heatmap")
types <- list("viewSums","viewMins", "viewMaxs", "viewMeans")

stat_geom_bar <- function(args.aes, args.non) {    
    args.non$stat <- "identity"
    aes.args <- do.call(aes, args.aes)
    p <- ggbio:::do.ggcall(geom_bar, c(list(aes.args), args.non))
}

stat_geom_heatmap <- function(args.aes, args.non, binwidth) {
    args.aes$xmin <- substitute(x - binwidth / 2, list(binwidth = binwidth))
    args.aes$xmax <- substitute(x + binwidth / 2, list(binwidth = binwidth))
    args.aes$ymin <- 0
    args.aes$ymax <- 5
    args.aes$color <- as.name("y")
    args.aes$fill <- as.name("y")
    args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    aes.args <- do.call(aes, args.aes)        
    p <- ggbio:::do.ggcall(geom_rect, c(list(aes.args), args.non))
}

# Testing for Rle --------------------------------------------------------

test_that("Test xlab parameter of stat_bin(Rle)", {
    test <- stat_bin(xRle, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("y"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_bin(Rle)", {
    test <- stat_bin(xRle, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_bin(Rle)", {
    test <- stat_bin(xRle, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y"),labs(title = "Title"))
    expect_identical(test, expected)
})

others <- list("binwidth", "nbin")
seed_values <- list(list(binwidth = 10),
                   list(nbin = 10))

make_stat_bin_Rle <- function(data, nbin = 30, binwidth = length(data)/nbin,
                              geom = "bar",type = "viewSums") {
    args.aes <- args.non <- list()
    args.aes$x <- if(!"x" %in% names(args.aes)) substitute(x)
    args.aes$y <- if(!"y" %in% names(args.aes)) substitute(y)
    vs <- Views(data, start = seq(from = 1, to = length(data), by = binwidth),
                width = binwidth)
    x <- seq(from = 1, to = length(data), by = binwidth) + binwidth/2
    y <- switch(type, viewMaxs = viewMaxs(vs),
                      viewMins = viewMins(vs), 
                      viewSums =  viewSums(vs),
                      viewMeans = viewMeans(vs))
    args.non$data <- data.frame(x = x, y = y)
    if(geom == "bar")
        p <- stat_geom_bar(args.aes, args.non)
    if(geom == "heatmap")
        p <- stat_geom_heatmap(args.aes, args.non, binwidth)
    p <- c(p, list(xlab("x")), list(ylab("y")))
}

# wrapper to avoid unlist/list of xRle
call_stat_bin_Rle <- function(...) {
    stat_bin(xRle, ...)
}

# wrapper to avoid unlist/list of xRle
call_make_stat_bin_Rle <- function(...) {
    make_stat_bin_Rle(xRle, ...)
}

# for geom = c("bar", "heatmap")
td <- TestDetails("geom", geoms, "stat_bin", "Rle", call_stat_bin_Rle,
                  call_make_stat_bin_Rle, others, seed_values)
test_generator(td)
# for type = c("viewSums","viewMins", "viewMaxs", "viewMeans")
td <- TestDetails("type", types, "stat_bin", "Rle", call_stat_bin_Rle,
                  call_make_stat_bin_Rle, others, seed_values)
test_generator(td)


## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(geom)                 (geom = c("bar", "heatmap"))
##                    * length(others)               (indName, binwidth, nbin)
##                    + length(type)                 (type = c("viewSums","viewMins", "viewMaxs", "viewMeans"))
##                    * length(others)               (indName, binwidth, nbin)
## Total test cases = 3 + (2 * 3) + (4 * 3) => 21

# Testing for RleList ----------------------------------------------------

test_that("Test xlab parameter of stat_bin(RleList)", {
    test <- stat_bin(xRleList, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("y"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_bin(RleList)", {
    test <- stat_bin(xRleList, ylab = "y-axis")
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_bin(RleList)", {
    test <- stat_bin(xRleList, main = "Title")
    test <- test[[1]]
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y"),labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_bin_RleList <- function(data, nbin = 30, binwidth, indName = "sample",
                                  geom = "bar",type = "viewSums") {
    args.aes <- args.non <- args <- list()
    args.aes$x <- if(!"x" %in% names(args.aes)) substitute(x)
    args.aes$y <- if(!"y" %in% names(args.aes)) substitute(y)
    mn <- mean(unlist(lapply(data, length)))
    if(missing(binwidth)) binwidth <- mn/nbin
    vs <- lapply(data, function(x) {
        Views(x, start = seq(from = 1, to = length(x), by = binwidth),
              width = binwidth)
    })
    x <- lapply(data, function(x) { 
        seq(from = 1, to = length(x), by = binwidth) + binwidth/2
    })
    y <- switch(type, viewMaxs = lapply(vs, viewMaxs),
                      viewMins = lapply(vs, viewMins),
                      viewSums = lapply(vs, viewSums),
                      viewMeans = lapply(vs, viewMeans))
    if(is.null(names(x)))
        nms <- rep(1:length(x), times = elementNROWS(x))
    else
        nms <- rep(names(x), times = elementNROWS(x))
    df <- data.frame(x = unlist(x), y = unlist(y), listName = nms)
    colnames(df) <- c("x", "y", indName)
    if(is.null(names(x)))
        levels(df[, indName]) <- 1:length(x)
    else
        levels(df[, indName]) <- unique(names(x))
    facets <- as.formula(paste(indName, "~ .", sep = ""))
    args$facets <- facets
    args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- do.call(facet_grid, args.facets)
    args.non$data <- df
    if(geom == "bar")
        p <- stat_geom_bar(args.aes, args.non)
    if(geom == "heatmap")
        p <- stat_geom_heatmap(args.aes, args.non, binwidth)
    p <- c(p, list(xlab("x")), list(ylab("y")))
    p <- c(list(p), list(facet))
    p <- ggbio:::setStat(p)
}

# wrapper to avoid unlist/list of xRle
call_stat_bin_RleList <- function(...) {
    stat_bin(xRleList, ...)
}

# wrapper to avoid unlist/list of xRle
call_make_stat_bin_RleList <- function(...) {
    make_stat_bin_RleList(xRleList, ...)
}

others <- list("indName", "binwidth", "nbin")
seed_values <- list(list(indName = "pair"),
                    list(binwidth = 10),
                    list(nbin = 10))

# for geom = c("bar", "heatmap")
td <- TestDetails("geom", geoms, "stat_bin", "RleList", call_stat_bin_RleList,
                  call_make_stat_bin_RleList, others, seed_values)
test_generator(td)
# for type = c("viewSums","viewMins", "viewMaxs", "viewMeans")
td <- TestDetails("type", types, "stat_bin", "RleList", call_stat_bin_RleList,
                  call_make_stat_bin_RleList, others, seed_values)
test_generator(td)
