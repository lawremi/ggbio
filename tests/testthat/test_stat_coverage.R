context("stat_coverage")
## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (xlim, facets, geom)
## Total test cases = 3 + 3 => 6

# simulate testing data
source('data.R')

source('test-generator.R')

# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of stat_coverage(GRanges)", {
    test <- stat_coverage(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("Coverage"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_coverage(GRanges)", {
    test <- stat_coverage(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_coverage(GRanges)", {
    test <- stat_coverage(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("Coverage"), labs(title = "Title"))
    expect_identical(test, expected)
})

transformGRL <- function(grl, geom, xlim, facets, allvars.extra) {
    lst <- lapply(grl, function(dt) {
        vals <- coverage(keepSeqlevels(dt, unique(as.character(seqnames(dt)))))
        if(any(is.na(seqlengths(dt)))) {
            seqs <- xlim[1]:max(end(dt))
            vals <- vals[[1]][seqs]
            vals <- as.numeric(vals)
            vals <- c(vals, rep(0, xlim[2]-max(end(dt))))
            seqs <- xlim[1]:xlim[2]
        } else {
            seqs <- xlim[1]:xlim[2]
            vals <- vals[[1]][seqs]
            vals <- as.numeric(vals)
        }
        if(geom == "area" | geom == "polygon") {
            seqs <- c(seqs, rev(seqs))
            vals <- c(vals, rep(0, length(vals)))
        }
        if(length(unique(values(dt)$.id.name))) {
            res <- data.frame(coverage = vals, seqs = seqs,
                              seqnames =
                              as.character(seqnames(dt))[1],
                              .id.name = unique(values(dt)$.id.name))
        } else {
            if("strand" %in% all.vars(facets)) {
                res <- data.frame(coverage = vals, seqs = seqs,
                            seqnames = as.character(seqnames(dt))[1],
                            strand = unique(as.character(strand(dt))))
            } else {
                res <- data.frame(coverage = vals, seqs = seqs,
                            seqnames = as.character(seqnames(dt))[1])
            }
        }
        for(v in allvars.extra) {
            res[,v] <- rep(unique(values(dt)[, v]), nrow(res))
        }
        res
    })
}

make_stat_coverage_GRanges <- function(data, ..., xlim, facets = NULL,
                                      geom = "area") {
    args <- list(...)
    args$facets <- facets
    args$geom <- geom
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    data <- keepSeqlevels(data, unique(as.character(seqnames(data))))
    args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- ggbio:::.buildFacetsFromArgs(data, args.facets)
    grl <- biovizBase::splitByFacets(data, facets)
    if (missing(xlim))
        xlim <- c(min(start(ranges(data))), max(end(ranges(data))))
    if(!length(facets))
        facets <- as.formula(~seqnames)
    facets <- biovizBase::strip_formula_dots(facets)
    allvars <- unique(c(all.vars(as.formula(facets)),
                        vapply(args.aes, quo_name, character(1L))))
    allvars.extra <- allvars[!allvars %in% c(".", "seqnames", "strand", "coverage")]
    lst <- transformGRL(grl, geom, xlim, facets, allvars.extra)
    res <- do.call(rbind, lst)
    if(!"y"  %in% names(args.aes))
        args.aes$y <- as.name("coverage")
    if(!"x"  %in% names(args.aes))
        args.aes$x <- as.name("seqs")
    aes <- do.call(aes, args.aes)
    if(geom  == "area" | geom == "polygon")
        args.non$geom <- "polygon"
    args.res <- c(list(data = res), list(aes), args.non)
    p <- ggbio:::do.ggcall(ggbio::stat_identity, args.res)
    p <- c(list(p) , list(facet))
    p <- c(p, list(xlab("")), list(ylab("Coverage")))
    attr(p, "isStat") <- TRUE
    p
}

# wrapper to avoid unlist/list of GRanges
call_stat_coverage_GRanges <- function_wrapper(ggbio:::stat_coverage, data)
call_make_stat_coverage_GRanges <- function_wrapper(make_stat_coverage_GRanges, data)

# "xlim" is not working properly
others <- list("facets", "geom")
seed_values <- list(list(facets = sample ~ seqnames),
                    list(geom = "polygon"))

td <- TestDetails(module_name = "stat_coverage", generic_name = "GRanges",
                  test_func = call_stat_coverage_GRanges,
                  expected_func = call_make_stat_coverage_GRanges,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)


## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(other_paramerts)      (xlim, facets, geom)
## Total test cases = 3 + 3 => 6

# Testing for GRangesList ----------------------------------------------------

test_that("Test xlab parameter of stat_coverage(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_coverage(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("Coverage"),
                     xlab("x-axis"), ylab("Coverage"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_coverage(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_coverage(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("Coverage"),
                     xlab("Genomic Coordinates"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_coverage(GRangesList)", {
    data <- GRangesList(data)
    test <- stat_coverage(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("Coverage"), xlab("Genomic Coordinates"),
                     ylab("Coverage"), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_coverage_GRangesList <- function(data, ..., xlim,facets = NULL, geom = NULL) {
    args <- list(...)
    args$facets <- facets
    args.aes <- biovizBase::parseArgsForAes(args)
    args.non <- biovizBase::parseArgsForNonAes(args)
    if(!"y" %in% names(args.aes))
      args.aes$y <- as.name("coverage")
    if(!is.null(geom))
      args.non$geom <- geom
    aes.res <- do.call(aes, args.aes)
    args.non$data <- biovizBase::flatGrl(data)
    p <- ggbio:::do.ggcall(stat_coverage, c(list(aes.res), args.non))
    p <- c(p, list(xlab("Genomic Coordinates")), list(ylab("Coverage")))
    attr(p, "isStat") <- TRUE
    p
}

call_stat_coverage_GRangesList <- function_wrapper(ggbio:::stat_coverage, GRangesList(data))
call_make_stat_coverage_GRangesList <- function_wrapper(make_stat_coverage_GRangesList, GRangesList(data))

# seems "xlim" is not implemented
others <- list("facets", "geom")
seed_values <- list(list(facets = sample ~ seqnames),
                    list(geom = "area"))

td <- TestDetails(module_name = "stat_coverage", generic_name = "GRangesList",
                  test_func = call_stat_coverage_GRangesList,
                  expected_func = call_make_stat_coverage_GRangesList,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
##                    + length(coord)                (coord = c("linear", "genome"))
##                    * length(other_paramerts)      (maxBinSize, xlim, which, facets, geom, space.skip)
##                    + length(method)               (method = c("estimate", "raw"))
##                    * length(other_paramerts)      (maxBinSize, xlim, which, facets, geom, space.skip)
## Total test cases = 3 + (2 * 6) + (2 * 6) => 27

# Testing for BamFile ----------------------------------------------------

test_that("Test xlab parameter of stat_coverage(BamFile)", {
    test <- stat_coverage(bamFile, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("Coverage"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_coverage(BamFile)", {
    test <- stat_coverage(bamFile, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Genomic Coordinates"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_coverage(BamFile)", {
    test <- stat_coverage(bamFile, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Genomic Coordinates"), ylab("Coverage"), labs(title = "Title"))
    expect_identical(test, expected)
})

make_stat_coverage_BamFile <- function(data, ..., maxBinSize = 2^14, xlim, which,
                                       facets = NULL, geom = "line", method = "estimate",
                                       space.skip = 0.1, coord = "linear") {
    if(missing(which)) {
        if(method != "estimate") {
            p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
                   list(ggplot2::xlim(c(0, 1))))
            return(p)
        } else {
            seq.nm <- names(scanBamHeader(data)$target)
        }
    } else {
        if(is(which, "GRanges")) {
            seq.nm <- unique(as.character(seqnames(which)))
        } else if(is(which, "character")) {
            seq.nm <- which
        } else {
            stop("which must be missing, GRanges or character(for seqnames)")
        }
    }
    args <- list(...)
    args$facets <- facets
    args.aes <- biovizBase::parseArgsForAes(args)
    if(!"y" %in% names(args.aes)) {
        args.aes$y <- as.name("score")
    } else {
        idx <- grepl("coverage", as.list(args.aes$y))
        if(any(idx)) {
            args.aes$y[[which(idx)]] <- as.name("score")
        }
    }
    if(!"x" %in% names(args.aes)) {
      args.aes$x <- as.name("midpoint")
    }
    args.non <- biovizBase::parseArgsForNonAes(args)
    args.non <- args.non[!names(args.non) %in% c("method", "maxBinSize", "data", "which")]
    args.non$geom <- geom
    res <- switch(method,
                estimate = {
                  message("Estimating coverage...")
                  res <- biovizBase::estimateCoverage(data, maxBinSize = maxBinSize)
                  if(!missing(which) && is(which, "GRanges"))
                      res <- subsetByOverlaps(res, which)
                  res
                },
                raw = {
                  if(missing(which))
                      stop("method 'raw' require which argument")
                  message("Parsing raw coverage...")
                  res <- biovizBase::crunch(data, which = which)
                })
    res.ori <- res
    if(method == "estimate") {
        message("Constructing graphics...")
        res <- res[seqnames(res) %in% seq.nm]
        args.facets <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
        facet <- ggbio:::.buildFacetsFromArgs(res, args.facets)
        res <- keepSeqlevels(res, unique(as.character(seqnames(res))))
        if(coord == "genome") {
            res <- biovizBase::transformToGenome(res, space.skip = space.skip)
            res.ori <- res <- biovizBase:::rescaleGr(res)
        }
        if(geom == "area") {
            grl <- biovizBase::splitByFacets(res, facets)
            res <- endoapply(grl, function(gr) {
                gr <- sort(gr)
                .gr1 <- gr[1]
                values(.gr1)$score <- 0
                .grn <- gr[length(gr)]
                values(.grn)$score <- 0
                c(gr,.gr1, .grn)
            })
            res <- unlist(res)
        }
        res <- biovizBase::mold(res)
        aes.res <- do.call(aes, args.aes)
        args.res <- c(list(data = res), list(aes.res), args.non)
        p <- c(list(ggbio:::do.ggcall(ggbio::stat_identity, args.res)), list(facet))
    }
    if(method == "raw") {
        p <- stat_coverage(res, ..., geom = geom, facets = facets)
    }
    p <- c(p, list(xlab("Genomic Coordinates")), list(ylab("Coverage")))
    if(biovizBase:::is_coord_genome(res.ori)) {
        ss <- biovizBase::getXScale(res.ori)
        p <- c(p, list(scale_x_continuous(breaks = ss$breaks, labels = ss$labels)))
    }
    if(coord == "genome") {
        facet <- facet_null()
        p <- c(p, list(facet))
    }
    attr(p, "isStat") <- TRUE
    p
}

call_stat_coverage_Bamfile <- function_wrapper(ggbio:::stat_coverage, bamFile)
call_make_stat_coverage_BamFile <- function_wrapper(make_stat_coverage_BamFile, bamFile)


others <- list("maxBinSize", "xlim", "which", "facets", "geom", "space.skip")
seed_values <- list(list(maxBinSize = 20000),
                    list(xlim = c(1, 5)),
                    list(which = GRanges("seq2", IRanges(1,100))),
                    list(facets = ~ seqnames),
                    list(geom = "point"),
                    list(space.skip = 0.2))

coords <- list("linear", "genome")
methods <- list("estimate", "raw")

# coord = c("linear", "genome")
td <- TestDetails(with_name = "coord", with_values = coords,
                  module_name = "stat_slice", generic_name = "BamFile",
                  test_func = call_stat_coverage_Bamfile,
                  expected_func = call_make_stat_coverage_BamFile,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)

# method = c("estimate", "raw")
td <- TestDetails(with_name = "method", with_values = methods,
                  module_name = "stat_slice", generic_name = "BamFile",
                  test_func = call_stat_coverage_Bamfile,
                  expected_func = call_make_stat_coverage_BamFile,
                  other_parameters = others, seed_values = seed_values)
test_generator(td)
