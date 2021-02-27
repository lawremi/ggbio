context("geom_alignment")
## Assuming 'xlab', 'ylab', and 'main' are independent.
## So we can say from above statement
## Total test cases =  3                              (for 'xlab', 'ylab', 'main')
##                     + length(range.geom)           (range.geom = c("rect", "arrowrect"))
##                     * length(other_parameters)     ('facets', 'rect.height', 'group.selfish')
##                     + length(gap.geom) - 1         (gap.geom = c("chevron", "arrow", "segment"))
##                     * length(other_parameters)     ('facets', 'rect.height', 'group.selfish')
## Total test cases = 3 + (2 * 3) + (2 * 3) => 15
##


## simulate testing data
source('data.R')

# Testing for GRanges ------------------------------------------------------------

test_that("Test xlab parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab(""))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of geom_alignment(GRanges)", {
    test <- geom_alignment(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab(""), ylab(""), labs(title = "Title"))
    expect_identical(test, expected)
})

make_GRanges_alignment <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    range.geom <- args_non$range.geom
    gap.geom <- args_non$gap.geom
    rect.height <- args_non$rect.height
    group.selfish <- args_non$group.selfish
    if (is.null(group.selfish))
        group.selfish <- TRUE

    # simulate expected facets
    facets_args <- biovizBase::subsetArgsByFormals(args, facet_grid, facet_wrap)
    # here relying on .buildFacetsFromArgs, So it needs to be tested separately
    facets <- ggbio:::.buildFacetsFromArgs(data, facets_args)

    if(is.null(rect.height))
        rect.height <- 0.4
    args_non$rect.height <- rect.height

    main_fun <- switch(range.geom,
                       rect = {
                           geom_rect
                       },
                       arrowrect = {
                           geom_arrowrect
                       })

    gap_fun <- switch(gap.geom,
                      chevron = {
                          geom_chevron
                      },
                      arrow = {
                          geom_arrow
                      },
                      segment = {
                          geom_segment
                      })

    # simulate expected data
    if (identical(stat, "stepping")) {
        grl <-  biovizBase::splitByFacets(data, args$facets)
        if("group" %in% names(args_aes)) {
            gpn <- quo_name(args_aes$group)
            res <- endoapply(grl, biovizBase::addStepping, group.name = quo_name(args_aes$group),
                             group.selfish = group.selfish,
                             extend.size = 0)
        } else {
            gpn <- "stepping"
            res <- endoapply(grl, biovizBase::addStepping, extend.size = 0)
        }
        res <- unlist(res)
        df <- biovizBase::mold(res)
        gps <- biovizBase::getGaps(res, group.name = gpn, args$facets)
        if (length(gps)) {
            gps <- GenomeInfoDb::keepSeqlevels(gps, names(seqlengths(res)))
            args_gaps <- args_aes[!names(args_aes) %in% c("x", "y", "xend", "yend",
                                                          "label.type", "label.size",
                                                          "label.color", "size",
                                                          "fill", "color", "colour")]
            args_gaps$y <- as.name("stepping")
            aes_lst <- do.call("aes", args_gaps)
            gps_lst <- c(list(aes_lst), list(data = gps, stat = "identity"))
            p <- list(ggbio:::do.ggcall(gap_fun, gps_lst))
        } else {
            p <- NULL
        }
        ## plot main
        args_aes$y <- as.name("stepping")
        args_aes <- args_aes[names(args_aes) != "size"]
        args_non$stat = "identity"
        aes <- do.call(aes, args_aes)
        args_res <- c(list(data = res), list(aes), args_non)

        p <- c(p, list(do.call(main_fun, args_res)))
        # p <- .changeStrandColor(p, args_aes)
        .df.lvs <- unique(df$stepping)
        .df.sub <- df[, c("stepping", gpn)]
        .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
        if(gpn != "stepping" & group.selfish)
            p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                               labels = as.character(.df.sub[, gpn]))))
        else
            p <- c(p, list(scale_y_continuous(breaks = NULL)))
    }
    p <- c(list(p), list(facets), list(xlab("")), list(ylab("")))
}

test_that("Test facets with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", rect.height = 0.5)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with range.geom = 'rect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "chevron",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with range.geom = 'arrowrect' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, range.geom = "arrowrect", group.selfish = FALSE)
    args <- list(stat = "stepping", range.geom = "arrowrect", gap.geom = "chevron",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, gap.geom = "arrow", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5, gap.geom = "arrow")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with gap.geom = 'arrow' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE, gap.geom = "arrow")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "arrow",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test facets with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, gap.geom = "segment", facets = sample ~ seqnames)
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 facets = sample ~ seqnames)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test rect.height with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, rect.height = 0.5, gap.geom = "segment")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 rect.height = 0.5)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test group.selfish with gap.geom = 'segment' of geom_alignment(GRanges)", {
    test <- geom_alignment(data, group.selfish = FALSE, gap.geom = "segment")
    args <- list(stat = "stepping", range.geom = "rect", gap.geom = "segment",
                 group.selfish = FALSE)
    expected <- make_GRanges_alignment(data, args)
    expect_equal(test, expected)
})

## Total test cases =  length(other_parameters)     ('columns', 'which', 'names.expr', 'truncate.gaps', 'truncate.fun', 'ratio')
## Total test cases = 6

# Testing for TxDbOREnsDb --------------------------------------------------------
make_TxDbOREnsDb_alignment <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    which <- args_non$which
    columns <- args_non$columns
    names.expr <- args_non$names.expr
    truncate.gaps <- args_non$truncate.gaps
    truncate.fun <- args_non$truncate.fun
    ratio <- args_non$ratio
    if (is.null(columns))
        columns <- c("tx_id", "tx_name", "gene_id")
    if (is.null(names.expr))
        names.expr <- "tx_name"
    if (is.null(truncate.gaps))
        truncate.gaps <- FALSE
    if (is.null(ratio))
        ratio <- 0.0025

    if (is(data, "EnsDb")) {
        columns <- sub(columns, pattern="tx_name",
                       replacement="gene_name", fixed=TRUE)
    }
    if (is.null(which)) {
        p <- c(list(geom_blank()), list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
        return(p)
    }
    gr <- biovizBase:::crunch(data, which, truncate.gaps = truncate.gaps,
                 truncate.fun = truncate.fun, ratio = ratio,
                 columns = columns)
    grl <- split(gr, gr$tx_id)
    ## getting label
    df <- biovizBase:::mold(gr)
    .df.sub <- do.call(rbind, lapply(grl, function(g) biovizBase:::mold(g)[1, columns]))
    rownames(.df.sub) <- NULL

    if(is.expression(names.expr)) {
        .labels <- eval(names.expr, .df.sub)
    } else if(is.character(names.expr)) {
        if(length(names.expr) == nrow(.df.sub) && !(names.expr %in% colnames(.df.sub))) {
            .labels <- names.expr
        } else {
            .labels <- ggbio:::sub_names(.df.sub, names.expr)
        }
    }else {
        .labels <- ggbio:::sub_names(.df.sub, names.expr)
    }
    names(grl) <- .labels

    p <- do.call(geom_alignment, c(list(data = grl), list(args_aes)))
}

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

test_that("Test columns of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, columns = "tx_id")
    args <- list(columns = "tx_id")
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

test_that("Test which of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, which = genesymbol["RBM17"])
    args <- list(which = genesymbol["RBM17"])
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

test_that("Test names.expr of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, names.expr = "tx_id")
    args <- list(names.expr = "tx_id")
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

test_that("Test truncate.gaps of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, truncate.gaps = TRUE)
    args <- list(truncate.gaps = TRUE)
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

test_that("Test truncate.fun of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, truncate.fun = biovizBase::shrinkageFun)
    args <- list(truncate.fun = biovizBase::shrinkageFun)
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

test_that("Test ratio of geom_alignment(TxDbOREnsDb)", {
    test <- geom_alignment(txdb, ratio = 0.5)
    args <- list(ratio = 0.5)
    expected <- make_TxDbOREnsDb_alignment(txdb, args)
    expect_equal(test, expected)
})

## Total test cases = length(other_parameters)     ('which', 'columns', 'names.expr', 'truncate.gaps', 'truncate.fun', 'ratio')
## Total test cases = 6

# Testing for OrganismDb ---------------------------------------------------------

make_OrganismDb_alignment <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)
    args_aes <- do.call(aes, args_aes)

    # mapping
    which <- args_non$which
    columns <- args_non$columns
    names.expr <- args_non$names.expr
    truncate.gaps <- args_non$truncate.gaps
    truncate.fun <- args_non$truncate.fun
    ratio <- args_non$ratio
    .cols <- c("TXNAME", "SYMBOL", "TXID", "GENEID")
    columns <- unique(c(.cols, columns))
    if (is.null(names.expr))
        names.expr <- "SYMBOL"
    if (is.null(truncate.gaps))
        truncate.gaps <- FALSE
    if (is.null(ratio))
        ratio <- 0.0025

    if(is.null(which)) {
        p <- c(list(geom_blank()), list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
        return(p)
    } else {
        which <- range(which, ignore.strand = TRUE)
    }
    txdb <- OrganismDbi:::.getTxDb(data)
    gr <- biovizBase:::crunch(txdb, which, truncate.gaps = truncate.gaps,
                              truncate.fun = truncate.fun, ratio = ratio,
                              columns = c("tx_id", "tx_name", "gene_id"))
    grl <- split(gr, gr$tx_id)
    ks <- names(grl)
    lbs <- select(data, ks, columns, "TXID")

    ## getting label
    df <- biovizBase:::mold(gr)
    values(gr) <- cbind(values(gr), lbs[match(gr$tx_id, lbs$TXID), ])
    grl <- split(gr, gr$tx_id)

    .df.sub <- do.call(rbind, lapply(grl, function(g){
        biovizBase:::mold(g)[1, c("tx_id", "tx_name", "gene_id")]
    }))
    rownames(.df.sub) <- NULL
    .df.new <- lbs[match(.df.sub$tx_id, lbs$TXID), ]
    .df.sub <- cbind(.df.sub, .df.new)

    if (is.expression(names.expr)) {
        .labels <- eval(names.expr, .df.sub)
    } else if (is.character(names.expr)) {
        if (length(names.expr) == nrow(.df.sub) &&
           !(names.expr %in% colnames(.df.sub))) {
            .labels <- names.expr
        } else {
            .labels <- ggbio:::sub_names(.df.sub, names.expr)
        }
    }else {
        .labels <- ggbio:::sub_names(.df.sub, names.expr)
    }

    names(grl) <- .labels
    p <- do.call(geom_alignment, c(list(data = grl, names.expr = names.expr), list(args_aes)))
}

library(Homo.sapiens)

test_that("Test columns of geom_alignment(OrganismDb)", {
    test <- geom_alignment(Homo.sapiens, columns = "TXNAME")
    args <- list(columns = "TXNAME")
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

test_that("Test which of geom_alignment(OrganismDb)", {
    which <- GRanges("chr11", IRanges(107899550,108291889),
                     SYMBOL = CharacterList("C11orf65"))
    test <- geom_alignment(Homo.sapiens, which = which)
    args <- list(which = which)
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

test_that("Test names.expr of geom_alignment(OrganismDb)", {
    test <- geom_alignment(Homo.sapiens, names.expr = "tx_id")
    args <- list(names.expr = "TXID")
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

test_that("Test truncate.gaps of geom_alignment(OrganismDb)", {
    test <- geom_alignment(Homo.sapiens, truncate.gaps = TRUE)
    args <- list(truncate.gaps = TRUE)
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

test_that("Test truncate.fun of geom_alignment(OrganismDb)", {
    test <- geom_alignment(Homo.sapiens, truncate.fun = biovizBase::shrinkageFun)
    args <- list(truncate.fun = biovizBase::shrinkageFun)
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

test_that("Test ratio of geom_alignment(OrganismDb)", {
    test <- geom_alignment(Homo.sapiens, ratio = 0.5)
    args <- list(ratio = 0.5)
    expected <- make_OrganismDb_alignment(Homo.sapiens, args)
    expect_equal(test, expected)
})

## Total test cases =  length(other_parameters)     ('which', 'what', 'facets')
## Total test cases = 3

# TODO:Testing for BamFile ------------------------------------------------------------

## Total test cases = 3                              (for 'xlab', 'ylab', 'main')
#                     +  length(stat)                (stat = c("identity", "reduce"))
##                    * length(other_parameters)     ('which', 'cds.rect.h', 'exon.rect.h', 'utr.rect.h',
##                                                    'geom', 'range.geom', 'gap.geom', 'utr.geom', 'names.expr', 'label',
##                                                    'label.color', 'label.size', 'arrow.rate', 'length')
## Total test cases = 3 + 2 * 14 => 31

# Testing for GRangesList --------------------------------------------------------

make_GRangesList_alignment <- function(data, args) {
    args_aes <- biovizBase::parseArgsForAes(args)
    args_non <- biovizBase::parseArgsForNonAes(args)

    # mapping
    stat <- args_non$stat
    which <- args_non$which
    cds.rect.h <- args_non$cds.rect.h
    exon.rect.h <- args_non$exon.rect.h
    utr.rect.h <- args_non$utr.rect.h
    geom <- args_non$geom
    range.geom <- args_non$range.geom
    gap.geom <- args_non$gap.geom
    utr.geom <- args_non$utr.geom
    names.expr <- args_non$names.expr
    label <- args_non$label
    label.color <- args_non$label.color
    label.size <- args_non$label.size
    arrow.rate <- args_non$arrow.rate
    length <- args_non$length

    if (is.null(cds.rect.h))
        cds.rect.h <- 0.25
    if (is.null(exon.rect.h))
        exon.rect.h <- cds.rect.h
    if (is.null(utr.rect.h))
        utr.rect.h <- cds.rect.h/2
    if (is.null(geom))
        geom <- "alignment"
    if (is.null(stat))
        stat <- "identity"
    if (is.null(range.geom))
        range.geom <- "rect"
    if (is.null(gap.geom))
        gap.geom <- "arrow"
    if (is.null(utr.geom))
        utr.geom <- "rect"
    if (is.null(label))
        label <- TRUE
    if (is.null(label.color))
        label.color <- "gray40"
    if (is.null(label.size))
        label.size <- 3
    if (is.null(arrow.rate))
        arrow.rate <- 0.015
    if (is.null(length))
        length <- unit(0.1, "cm")

    gap.fun <- ggbio:::getGeomFun(gap.geom)
    range.fun <- ggbio:::getGeomFun(range.geom)
    utr.fun <- ggbio:::getGeomFun(utr.geom)

    if("type" %in% names(args_aes)) {
        .type <- quo_name(args_aes$type)
    } else {
        .type <- NULL
    }
    if(!ggbio:::isGenemodel(data, type = .type)){
        gr <- biovizBase:::flatGrl(data)
        if(!"group" %in% names(args_aes))
            args_aes$group <- as.name("grl_name")
        if("type" %in% names(args_aes)){
            args_aes <- args_aes[names(args_aes) != "type"]
        }
        aes.res <- do.call(aes, args_aes)

        p <- do.call(geom_alignment, c(list(data = gr), list(aes.res)))
        return(p)
    }

    message("Constructing graphics...")
    .xlim <- NULL
    if (length(which) && is(which, "GRanges")) {
        which <- range(which, ignore.strand = TRUE)
        data <- subsetByOverlaps(data, which)
    }
    mids <- unlist(lapply(data, function(g) {
        r <- range(g, ignore.strand = TRUE)
        start(r) + width(r)/2
    }))
    gr <- stack(data, "..sample..")
    values(gr)$..inner.. <- rep(1:length(data), times = elementNROWS(data))

    if("type" %in% names(args_aes)){
        .type <- quo_name(args_aes$type)
        args_aes <- args_aes[names(args_aes) != "type"]
    } else {
        .type <- "type"
    }

    if (length(gr)) {
        if (length(which) && is(which, "GRanges")) {
            .xlim <- c(min(start(which)), max(end(which)))
        } else {
            .xlim <- c(min(start(gr)), max(end(gr)))
        }
        ## adding buffer between features to avoid overlap
        if ("extend.size" %in% names(args_non)) {
            es <- args_non$extend.size
        } else {
            es <- diff(.xlim)/1000 * 20
        }
        if (geom == "alignment" && stat == "reduce") {
            .gr.ori <- gr
            .gr <- biovizBase:::addStepping(gr, group.name = "..inner..", group.selfish = FALSE, extend.size = es)
            message("reduce alignemnts...")
            ## compute labels
            gr <- stack(endoapply(split(gr, values(gr)[[.type]]), function(g) {
                res <- reduce(g, ignore.strand = TRUE)
            }), .type)
            gr$stepping <- .lvs <- 1
        } else {
            gr <- biovizBase:::addStepping(gr, group.name = "..inner..", group.selfish = FALSE, extend.size = es)
        }
        df <- mold(gr)
        if (label) {
            if (stat == "reduce") {
                ## only transcripts with symbol or geneid
                cnms <- colnames(values(.gr.ori))
                idx <- cnms %in% c("SYMBOL", "GENEID", "gene_id")
                if(sum(idx)){
                    .df.sub <- do.call(rbind,lapply(split(.gr.ori,values(.gr.ori)[,which(idx)[1]]),
                                        function(g){
                                            if (length(g) == 0L)
                                                return(NULL)
                                            r <- range(g, ignore.strand = TRUE)
                                            mid <- start(r) + width(r)/2
                                            d <- values(g)[1, idx, drop = FALSE]
                                            d$midpoint <- mid
                                            d$stepping <- 1
                                            d
                               }))
                    .df.sub <- as.data.frame(.df.sub)
                    if (length(names.expr)) {
                        .labels <- ggbio:::sub_names(.df.sub, names.expr)
                        .df.sub$.labels <- .labels
                    } else {
                        label <- FALSE
                    }
                } else {
                    label <- FALSE
                }
            } else {
                column <- c("tx_id", "stepping", "..sample..", "..inner..")
                idx <- column %in% colnames(df)
                if (sum(idx)) {
                    column <- column[idx]
                    names.expr <- "..sample.."
                    .df.sub <- df[, column]
                    .df.sub <- .df.sub[!duplicated(.df.sub$..inner..),]
                    .df.sub$midpoint <- mids[.df.sub$..inner..]
                    .labels <- ggbio:::sub_names(.df.sub, names.expr)
                    .lvs <- max(.df.sub$stepping)
                    .df.lvs <- unique(df$stepping)
                    .df.sub$.labels <- .labels
                } else {
                    label <- FALSE
                }
         }
        }
        ## cds
        gr.cds <- gr[values(gr)[[.type]] %in% c("cds", "CDS")]
        ## exons
        gr.exons <- gr[values(gr)[[.type]] %in% c("exon", "EXON")]
        args.cds.non <- args_non
        args.cds.non$rect.height <- cds.rect.h
        args.exon.non <- args.cds.non
        args.exon.non$rect.height <- exon.rect.h
        args.exon <- args_aes[names(args_aes) != "y"]
        args.exon$y <- as.name("stepping")
        aes.res <- do.call(aes, args.exon)
        p <- NULL
        if (length(gr.cds) > 0L) {
            ## plot cds
            args.cds.res <- c(list(data = gr.cds),
                              list(aes.res),
                              args.cds.non,
                              list(stat = "identity"))
            p <- ggbio:::do.ggcall(range.fun, args.cds.res)
        }
        if (length(gr.exons) > 0L) {
            ## plot exons
            ## input gr.exons
            args.exon.res <- c(list(data = gr.exons),
                               list(aes.res),
                               args.exon.non,
                               list(stat = "identity"))
            p <- c(p, list(ggbio:::do.ggcall(range.fun, args.exon.res)))
        }
        ## utrs
        gr.utr <- gr[values(gr)[[.type]] == "utr"]
        args.utr <- args_aes[names(args_aes) != "y"]
        args.utr$y <- as.name("stepping")
        aes.res <- do.call(aes, args.utr)
        args.utr.non <- args.cds.non
        args.utr.non$rect.height <- utr.rect.h

        if (range.geom == "arrowrect" && utr.geom == range.geom) {
            if (!"arrow.head" %in% names(args.utr.non)) {
                args.utr.non$arrow.head <- 0.06
            }
            arrow.head.fix <- getArrowLen(gr.cds, arrow.head.rate = args.non$arrow.head)
            args.utr.non <- args.non[names(args.non) != "arrow.rate"]
            args.utr.non$arrow.head.fix <- arrow.head.fix
        }
        if (length(gr.utr)) {
            args.utr.res <- c(list(data = gr.utr),
                              list(aes.res),
                              args.utr.non,
                              list(stat = "identity"))
            p <- c(p, list(ggbio:::do.ggcall(utr.fun, args.utr.res)))
        }
        if (stat == "reduce") {
            .grl <- endoapply(data, function(g) {
                range(g, ignore.strand = TRUE)
            })
            grr <- reduce(unlist(.grl))
            .gr$..sample.. <- rep(subjectHits(findOverlaps(.grl, grr)),
                                  times = elementNROWS(data))
            exonic <- .gr[values(.gr)[[.type]] %in% c("utr", "cds", "exon")]
            df.gaps <- biovizBase:::getGaps(exonic, group.name = "..sample..")
            df.gaps$stepping <- 1
            ## let's figure out strand
            stds <- unique(as.character(strand(.gr)))
            if (length(stds) == 1) {
                strand(df.gaps) <- stds
            } else {
                strand(df.gaps) <- "*"
            }
          } else {
            exonic <- gr[values(gr)[[.type]] %in% c("utr", "cds", "exon")]
            df.gaps <- biovizBase:::getGaps(exonic, group.name = "..inner..")
          }

        args.aes.gaps <- args_aes[!(names(args_aes) %in% c("x", "y", "fill"))]
        aes.res <- do.call(aes, args.aes.gaps)

        if (!"arrow.rate" %in%  names(args_non)) {
            if(!is.list(which)) {
                arrow.rate <- 0.018 * diff(.xlim)/
                    (end(range(ranges(df.gaps))) - start(range(ranges(df.gaps))))
                args_non$arrow.rate <- arrow.rate
            }
        }

        aes.res$y <- as.name("stepping")
        args.non.arrow <- args_non
        args.non.arrow$length <- length
        args.gaps.res <- c(list(data = df.gaps),
                           list(aes.res),
                           args.non.arrow,
                           list(stat = "identity"))
        if (length(df.gaps)) {
            p <- c(p , list(ggbio:::do.ggcall(gap.fun, args.gaps.res)))
        }
        if (label) {
            aes.label <- do.call(aes, list(label = substitute(.labels),
                                           x = substitute(midpoint),
                                           y = substitute(stepping +
                                               cds.rect.h*1.2)))

            args.label.res <- args_non
            args.label.res$size <- label.size
            args.label.lst <- list(data = .df.sub,
                                   vjust = 0,
                                   aes.label,
                                   color = label.color,
                                   inherit.aes = FALSE)
            args.label.res[names(args.label.lst)] <- args.label.lst
            p <- c(p , list(ggbio:::do.ggcall(geom_text, args.label.res)))
            ggplot() + p
        }
        p <- c(p , list(scale_y_continuous(breaks = NULL)))
    } else {
        p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
    }
    if(missing(xlab)){
        xlab <- ""
    }
    xlab <- ggplot2::xlab(xlab)
    p <- c(p, list(xlab(xlab)))
    if(missing(ylab)){
        ylab <- ""
    }
    p <- c(p, list(ylab(ylab)))
    if(!missing(main))
        p <- c(p, labs(title = main))
    return(p)
}

# TODO : Fix xlab
# test_that("Test xlab parameter of geom_alignment(GRangesList)", {
#     data <- GRangesList(data)
#     test <- geom_alignment(data, xlab = "x-axis")
#     # select elements of 'labels' class from a 'test' list
#     test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
#     expected <- list(xlab("x-axis"), ylab(""))
#     expect_identical(test, expected)
# })

# TODO : Fix ylab
# test_that("Test ylab parameter of geom_alignment(GRangesList)", {
#     data <- GRangesList(data)
#     test <- geom_alignment(data, ylab = "y-axis")
#     test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
#     expected <- list(xlab(""), ylab("y-axis"))
#     expect_identical(test, expected)
# })

# TODO : Fix main
# test_that("Test main parameter of geom_alignment(GRangesList)", {
#     data <- GRangesList(data)
#     test <- geom_alignment(data, main = "Title")
#     test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
#     expected <- list(xlab(""), ylab(""), labs(title = "Title"))
#     expect_identical(test, expected)
# })

test_that("Test which parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    which <- GRanges("chr1", IRanges(249, 320))
    test <- geom_alignment(data, which = which)
    args <- list(which = which)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test cds.rect.h parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, cds.rect.h = 0.5)
    args <- list(cds.rect.h = 0.5)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test exon.rect.h parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, exon.rect.h = 0.5)
    args <- list(exon.rect.h = 0.5)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test utr.rect.h parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, utr.rect.h = 0.5)
    args <- list(utr.rect.h = 0.5)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test geom parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, geom = "identity")
    args <- list(geom = "identity")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test range.geom parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, range.geom = "arrow")
    args <- list(range.geom = "arrow")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test gap.geom parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, gap.geom = "rect")
    args <- list(gap.geom = "rect")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test utr.geom parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, utr.geom = "arrow")
    args <- list(utr.geom = "arrow")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test names.expr parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, names.expr = "sample")
    args <- list(names.expr= "sample")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label = FALSE)
    args <- list(label = FALSE)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label.color parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label.color = "gray41")
    args <- list(label.color = "gray41")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label.size parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label.size = 1)
    args <- list(label.size = 1)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.rate parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, arrow.rate = 0.5)
    args <- list(arrow.rate = 0.5)
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test length parameter with stat = 'identity' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, length = unit(0.5, "cm"))
    args <- list(length = unit(0.5, "cm"))
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test which parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    which <- GRanges("chr1", IRanges(249, 320))
    test <- geom_alignment(data, which = which, stat = "reduce")
    args <- list(which = which, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test cds.rect.h parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, cds.rect.h = 0.5, stat = "reduce")
    args <- list(cds.rect.h = 0.5, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test exon.rect.h parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, exon.rect.h = 0.5, stat = "reduce")
    args <- list(exon.rect.h = 0.5, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test utr.rect.h parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, utr.rect.h = 0.5, stat = "reduce")
    args <- list(utr.rect.h = 0.5, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test geom parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, geom = "identity", stat = "reduce")
    args <- list(geom = "identity", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test range.geom parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, range.geom = "arrow", stat = "reduce")
    args <- list(range.geom = "arrow", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test gap.geom parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, gap.geom = "rect", stat = "reduce")
    args <- list(gap.geom = "rect", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test utr.geom parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, utr.geom = "arrow", stat = "reduce")
    args <- list(utr.geom = "arrow", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test names.expr parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, names.expr = "sample", stat = "reduce")
    args <- list(names.expr= "sample", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label = FALSE, stat = "reduce")
    args <- list(label = FALSE, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label.color parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label.color = "gray41", stat = "reduce")
    args <- list(label.color = "gray41", stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test label.size parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, label.size = 1, stat = "reduce")
    args <- list(label.size = 1, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test arrow.rate parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, arrow.rate = 0.5, stat = "reduce")
    args <- list(arrow.rate = 0.5, stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})

test_that("Test length parameter with stat = 'reduce' of geom_alignment(GRangesList)", {
    data <- GRangesList(data)
    test <- geom_alignment(data, length = unit(0.5, "cm"), stat = "reduce")
    args <- list(length = unit(0.5, "cm"), stat = "reduce")
    expected <- make_GRangesList_alignment(data, args)
    expect_equal(test, expected)
})
