context("stat_slice")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(type)                 (type = c("viewSums","viewMins","viewMaxs", "viewMeans"))
##                    * length(other_paramerts)      (na.rm, lower, upper, includeLower, includeUpper, rangesOnly)
##                    + length(geom)                 (geom = c(segment, rect, heatmap, bar))
##                    * length(other_paramerts)      (na.rm, lower, upper, includeLower, includeUpper, rangesOnly)
## Total test cases = 3 + (4 * 6) + (4 * 6) => 51

# simulate testing data
source('data.R')

source('test-generator.R')

types <- list("viewSums", "viewMins", "viewMaxs", "viewMeans")
geoms <- list("segment", "rect", "heatmap", "bar")
others <- list("na.rm", "lower", "upper", "includeLower",
               "includeUpper", "rangesOnly")

# Testing for Rle ------------------------------------------------------------

test_that("Test xlab parameter of stat_slice(Rle)", {
    test <- stat_slice(xRle, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("y"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_slice(Rle)", {
    test <- stat_slice(xRle, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_slice(Rle)", {
    test <- stat_slice(xRle, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x"), ylab("y"), labs(title = "Title"))
    expect_identical(test, expected)
})

get_coordinate <- function(type, vs, na.rm, islist = FALSE) {
   values <- switch(type,
            viewMaxs = list(x = viewWhichMaxs(vs, na.rm = na.rm), y = viewMaxs(vs, na.rm = na.rm)),
            viewMins = list(x = viewWhichMins(vs, na.rm = na.rm), y = viewMins(vs, na.rm = na.rm)),
            viewSums = list(x = start(vs) + width(vs)/2, y = viewSums(vs, na.rm = na.rm)),
            viewMeans = list(x = start(vs) + width(vs)/2, y = viewMeans(vs, na.rm = na.rm)))
    if (islist) {
        values[["xmin"]] <- start(vs)
        values[["xmax"]] <- end(vs)
    }
    values
}

map_coordinate_by_geom <- function(geom, args.aes, args.non, vs, islist = FALSE) {
    values <- list()
    if (islist) {
        values[["xmin"]] <- as.name("xmin")
        values[["xmax"]] <- as.name("xmax")
    } else {
        values[["xmin"]] <- start(vs)
        values[["xmax"]] <- end(vs)
    }

    if (geom == "segment") {
        args.aes$x <- as.name("x")
        args.aes$xend <- as.name("x")
        args.aes$y <- 0
        args.aes$yend <- as.name("y")
    }
    if (geom == "rect") {
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- 5
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    }
    if (geom == "heatmap") {
        args.non$geom <- "rect"
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- 5
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
        args.aes$color <- as.name("y")
        args.aes$fill <- as.name("y")
    }
    if (geom == "bar") {
        args.non$geom <- "rect"
        args.aes$xmin <- values[["xmin"]]
        args.aes$xmax <- values[["xmax"]]
        args.aes$ymin <- 0
        args.aes$ymax <- as.name("y")
        args.aes <- args.aes[!names(args.aes) %in% c("x", "y")]
    }
    if (geom %in% c("bar", "rect")) {
        if(!"color" %in% names(args.aes) &&
        !"color" %in% names(args.non) &&
        !"colour" %in% names(args.aes) &&
        !"colour" %in% names(args.non)) {
            args.non$color <- "grey20"
        }
    }
    list(args.aes = args.aes, args.non = args.non)
}

make_stat_slice_Rle <- function(data, ..., na.rm = FALSE, geom = "segment",
                            lower = -Inf, upper = Inf, includeLower = TRUE,
                            includeUpper=TRUE, rangesOnly = FALSE,
                            type = "viewSums") {
    args <- list(...)
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    if(!"x" %in% names(args.aes))
        args.aes$x <- substitute(x)
    if(!"y" %in% names(args.aes))
        args.aes$y <- substitute(y)
    args.non$geom <- geom
    vs <- slice(data, upper = upper, lower = lower, includeLower = includeLower,
                includeUpper = includeUpper, rangesOnly = rangesOnly)
    values <- get_coordinate(type, vs, na.rm)
    df <- data.frame(x = values[["x"]], y = values[["y"]])
    values <- map_coordinate_by_geom(geom, args.aes, args.non, vs)
    args.aes <- values[["args.aes"]]
    args.non <- values[["args.non"]]
    args.non$data <- df
    aes.args <- do.call(aes, args.aes)
    res.args <- c(list(aes.args), args.non)
    p <- ggbio:::do.ggcall(ggplot2::stat_identity, res.args)
    p <- c(p, list(xlab("x")), list(ylab("y")))
    attr(p, "isStat") <- TRUE
    p
}

# wrapper to avoid unlist/list of Rle
call_stat_slice_Rle <- function_wrapper(ggbio:::stat_slice, xRle)
call_make_stat_slice_Rle <- function_wrapper(make_stat_slice_Rle, xRle)

seed_values <- list(list(na.rm = TRUE),
                    list(lower = Inf),
                    list(upper = -Inf),
                    list(includeLower = FALSE),
                    list(includeUpper = FALSE),
                    list(rangesOnly = FALSE))

# for type = c("viewSums","viewMins","viewMaxs", "viewMeans")
td <- TestDetails(with_name = "type", with_values = types,
                  module_name = "stat_slice", generic_name = "Rle",
                  test_func = call_stat_slice_Rle,
                  expected_func = call_make_stat_slice_Rle,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

seed_values[["na.rm"]] <- FALSE
seed_values[["lower"]] <- -Inf
seed_values[["upper"]] <- Inf

# for geoms = c("segment", "rect", "heatmap", "bar")
td <- TestDetails(with_name = "geom", with_values = geoms,
                  module_name = "stat_slice", generic_name = "Rle",
                  test_func = call_stat_slice_Rle,
                  expected_func = call_make_stat_slice_Rle,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(type)                 (type = c("viewSums","viewMins","viewMaxs", "viewMeans"))
##                    * length(other_paramerts)      (indName, na.rm, lower, upper, includeLower, includeUpper, rangesOnly)
##                    + length(geom)                 (geom = c(segment, rect, heatmap, bar))
##                    * length(other_paramerts)      (indName, na.rm, lower, upper, includeLower, includeUpper, rangesOnly)
## Total test cases = 3 + (4 * 7) + (4 * 7)  => 59

# Testing for RleList --------------------------------------------------------

make_stat_slice_RleList <- function(data, ..., indName = "sample",
                                na.rm = FALSE, geom = "segment",
                                lower = -Inf, upper = Inf,
                                includeLower = TRUE, includeUpper = TRUE,
                                rangesOnly = FALSE, type = "viewSums") {
    args <- list(...)
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    if(!"x" %in% names(args.aes))
        args.aes$x <- substitute(x)
    if(!"y" %in% names(args.aes))
        args.aes$y <- substitute(y)
    args.non$geom <- geom
    vs <- slice(data, upper = upper, lower = lower, includeLower = includeLower,
                includeUpper = includeUpper, rangesOnly = rangesOnly)
    values <- get_coordinate(type, vs, na.rm, islist = TRUE)
    x <- values[["x"]]
    if (is.null(names(x)))
        nms <- rep(1:length(x), times = elementNROWS(x))
    else
        nms <- rep(names(x), times = elementNROWS(x))
    df <- data.frame(x = unlist(values[["x"]]), y = unlist(values[["y"]]), listName = nms,
                     xmin = unlist(values[["xmin"]]), xmax = unlist(values[["xmax"]]))
    colnames(df) <- c("x", "y", indName, "xmin", "xmax")
    if(is.null(names(x)))
        levels(df[, indName]) <- 1:length(x)
    else
        levels(df[, indName]) <- unique(names(x))
    facets <- as.formula(paste(indName, "~ .", sep = ""))
    args$facets <- facets
    args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- do.call(facet_grid, args.facets)
    values <- map_coordinate_by_geom(geom, args.aes, args.non, vs, islist = TRUE)
    args.aes <- values[["args.aes"]]
    args.non <- values[["args.non"]]
    args.non$data <- df
    aes.args <- do.call(aes, args.aes)
    res.args <- c(list(aes.args), args.non)
    p <- ggbio:::do.ggcall(ggplot2::stat_identity, res.args)
    p <- c(p, list(xlab("x")), list(ylab("y")))
    p <- c(list(p), list(facet))
    attr(p, "isStat") <- TRUE
    p
}

# wrapper to avoid unlist/list of RleList
call_stat_slice_RleList <- function_wrapper(ggbio:::stat_slice, xRleList)
call_make_stat_slice_RleList <- function_wrapper(make_stat_slice_RleList, xRleList)

others <- c(others, "indName")

seed_values <- list(list(na.rm = TRUE),
                    list(lower = Inf),
                    list(upper = -Inf),
                    list(includeLower = FALSE),
                    list(includeUpper = FALSE),
                    list(rangesOnly = FALSE),
                    list(indName = "pair"))

# for type = c("viewSums","viewMins","viewMaxs", "viewMeans")
td <- TestDetails(with_name = "type", with_values = types,
                  module_name = "stat_slice", generic_name = "RleList",
                  test_func = call_stat_slice_RleList,
                  expected_func = call_make_stat_slice_RleList,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

seed_values[["na.rm"]] <- FALSE
seed_values[["lower"]] <- -Inf
seed_values[["upper"]] <- Inf

# for geoms = c("segment", "rect", "heatmap", "bar")
td <- TestDetails(with_name = "geom", with_values = geoms,
                  module_name = "stat_slice", generic_name = "RleList",
                  test_func = call_stat_slice_RleList,
                  expected_func = call_make_stat_slice_RleList,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)
