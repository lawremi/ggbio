context("autoplot")

# simulate testing data
source('data.R')


# Testing for GRanges --------------------------------------------------------

test_that("Test xlab parameter of autoplot(GRanges)", {
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(GRanges)", {
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(GRanges)", {
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

# coord

test_that("Test autplot(GRanges) with coord = 'genome'", {
    test <- autoplot(data, coord = "genome")
    test <- test@data
    x <- biovizBase::transformToGenome(data, space.skip = 0.1, chr.weight = NULL)
    expected <- biovizBase:::rescaleGr(x)
    expect_equal(test, expected)
})

test_that("Test autoplot(GRanges) with truncate.gaps = TRUE and coord = 'truncate_gaps'", {
    test <- autoplot(data, truncate.gaps = TRUE, coord = "truncate_gaps")
    test <- test@data
    grl <- split(data, seqnames(data))
    ratio <- 0.0025
    lst <- endoapply(grl, function(gr) {
                object.s <- reduce(gr, ignore.strand = TRUE)
                gps <- gaps(object.s, min(start(object.s)), max(end(object.s)))
                gps <- gps[strand(gps) == "*"]
                truncate.fun <- biovizBase::shrinkageFun(gps, biovizBase::maxGap(gps, ratio = ratio))
                res <- truncate.fun(gr)
                res
            })
    expected <- unlist(lst)
    expect_equal(test, expected)
})

# layout

test_that("Test autplot(GRanges) with layout = 'linear'", {
    test <- autoplot(data, layout = "linear")
    test@ggplot$plot_env <- NULL
    .xlim <- c(start(range(data, ignore.strand = TRUE)),
               end(range(data, ignore.strand = TRUE)))
    .fun <- ggbio:::getGeomFun("rect")
    args.non <- list(data = data)
    p <- list(ggbio:::do.ggcall(.fun, c(args.non)))
    p <- c(p, list(xlab("")))
    p <- do.call(ggplot, c(list(data = data))) + p
    scale_by_xlim <- ggbio:::scale_by_xlim
    getLimits<- ggbio:::getLimits
    expected <- p + scale_by_xlim(getLimits(p)$xlim)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test autplot(GRanges) with layout = 'karyogram'", {
    test <- autoplot(data, layout = "karyogram")
    test@ggplot$plot_env <- NULL
    plot <- plotStackedOverview(data, geom = "rect") + xlab("")
    p <- GGbio(plot, data = data)
    scale_by_xlim <- ggbio:::scale_by_xlim
    getLimits<- ggbio:::getLimits
    expected <- p + scale_by_xlim(getLimits(p)$xlim)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test autplot(GRanges) with layout = 'circle'", {
    test <- autoplot(data, layout = "circle")
    test@ggplot$plot_env <- NULL
    p <- ggplot(data) + circle(data, geom = "rect", space.skip = 0.1)
    scale_by_xlim <- ggbio:::scale_by_xlim
    getLimits<- ggbio:::getLimits
    expected <- p + scale_by_xlim(getLimits(p)$xlim)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for GRangesList ----------------------------------------------------

test_that("Test xlab parameter of autoplot(GRangesList)", {
    data <- GRangesList(data)
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(GRangesList)", {
    data <- GRangesList(data)
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(GRangesList)", {
    data <- GRangesList(data)
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'segment' of autoplot(GRangesList)", {
    data <- GRangesList(data)
    test <- autoplot(data, geom = "segment")
    test@ggplot$plot_env <- NULL
    args <- list(data = data, geom = "segment", group.selfish = FALSE,
                 aes(group = "grl_name"))
    gr <- biovizBase::flatGrl(data, "grl_name")
    args$object <- gr
    p <- do.call(autoplot, args)
    xlab <- ""
    expected <- p + ggplot2::xlab(xlab)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})


# Testing for IRanges --------------------------------------------------------

test_that("Test xlab parameter of autoplot(IRanges)", {
    data <- data@ranges
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(IRanges)", {
    data <- data@ranges
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(IRanges)", {
    data <- data@ranges
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test data representation of autoplot(IRanges)", {
    data <- data@ranges
    test <- autoplot(data)
    df <- values(data)
    values(data) <- NULL
    gr <- GRanges("chr_non", data)
    values(gr) <- df
    expected <- autoplot(gr) + ggplot2::xlab("")
    expect_equal(test, expected)
})

# Testing for GAlignments ----------------------------------------------------

test_that("Test xlab parameter of autoplot(GAlignments)", {
    data <- rtracklayer::import(ex1_file)
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(GAlignments)", {
    data <- rtracklayer::import(ex1_file)
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(GAlignments)", {
    data <- rtracklayer::import(ex1_file)
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'gapped.pair' parameter of autoplot(GAlignments)", {
    data <- rtracklayer::import(ex1_file)
    test <- autoplot(data, geom = "gapped.pair")
    test@ggplot$plot_env <- NULL
    test@cached_item <- list()
    test@cached_which <- list()
    aes.res <- aes()
    xlab <- ""
    facet <- ggbio:::.buildFacetsFromArgs(data, list())
    args.non <- list(object = grglist(data))
    p <- do.call(autoplot, c(list(aes.res), args.non))
    expected <- p + ggplot2::xlab(xlab) + facet
    expected@ggplot$plot_env <- NULL
    expected@cached_item <- list()
    expected@cached_which <- list()
    expect_equal(test, expected)
})

test_that("Test show.junction parameter of autoplot(GAlignments)", {
    data <- rtracklayer::import(ex1_file)
    test <- autoplot(data, show.junction = TRUE)
    test@ggplot$plot_env <- NULL
    test@cached_item <- list()
    test@cached_which <- list()
    aes.res <- aes()
    xlab <- ""
    facet <- ggbio:::.buildFacetsFromArgs(data, list())
    args.non <- list(object = grglist(data))
    p <- do.call(autoplot, c(list(aes.res), args.non))
    expected <- p + ggplot2::xlab(xlab) + facet
    expected@ggplot$plot_env <- NULL
    expected@cached_item <- list()
    expected@cached_which <- list()
    expect_equal(test, expected)
})

# Testing for BamFile --------------------------------------------------------

test_that("Test xlab parameter of autoplot(BamFile)", {
    data <- bamFile
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(BamFile)", {
    data <- bamFile
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(BamFile)", {
    data <- bamFile
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'gapped.pair' and method = 'estimate' parameter of autoplot(BamFile)", {
    data <- bamFile
    which <- GRanges("seq1", IRanges(1, 1000))
    test <- autoplot(data, geom = "gapped.pair", which = which,  method = "estimate")
    test@ggplot$plot_env <- NULL
    test@cached_item <- list()
    test@cached_which <- list()
    mc <- list(autoplot=list(as.name("autoplot"), object = data,
               which = which, geom = "gapped.pair", method = "estimate"))
    bf <- open(data)
    ga <- GenomicAlignments::readGAlignments(bf, param = ScanBamParam(which = which), use.names = TRUE)
    args <- list(object = ga)
    p <- do.call(autoplot, args)
    p <- p + facet_wrap(~seqnames) + ggplot2::xlab("")
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item <- list()
    expected@cached_which <- list()
    expect_equal(test, expected)
})

test_that("Test stat = 'coverage' and method = 'estimate' parameter of autoplot(BamFile)", {
    data <- bamFile
    which <- GRanges("seq1", IRanges(1, 1000))
    test <- autoplot(data, stat = "coverage", which = which,  method = "estimate")
    test@ggplot$plot_env <- NULL
    test@cached_item <- list()
    test@cached_which <- list()
    mc <- list(autoplot=list(as.name("autoplot"), object = data,
               which = which, stat = "coverage", method = "estimate"))
    xlab <- ""
    method <- "estimate"
    coord <- "linear"
    space.skip <- 0.1
    geom <- "line"
    bf <- open(data)
    seq.nm <- unique(as.character(seqnames(which)))
    p <- ggplot() + stat_coverage(bf, method = method, coord = coord,
         space.skip = space.skip, geom  =  geom, which = which) +
         facet_wrap(~seqnames) + ggplot2::xlab(xlab)
    p <- GGbio(p, data = data)
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item <- list()
    expected@cached_which <- list()
    expect_equal(test, expected)
})

# Testing for character ------------------------------------------------------

test_that("Test xlab parameter of autoplot(character)", {
    data <- bamFile$path
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(character)", {
    data <- system.file("tests", "test.bb", package="rtracklayer")
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(character)", {
    data <- system.file("tests", "test.bb", package="rtracklayer")
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test data representation with which parameter of autoplot(character)", {
    data <- bamFile$path
    which <- GRanges("seq1", IRanges(1, 1000))
    test <- autoplot(data, which = which)
    test@ggplot$plot_env <- NULL
    test@cached_item <- list()
    test@cached_which <- list()
    mc <- list(autoplot=list(as.name("autoplot"), object = data, which = which))
    obj <- BamFile(data)
    aes.res <- aes()
    args.non <- list(which = which, object = obj)
    p <- do.call(autoplot, c(list(aes.res), args.non))
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item <- list()
    expected@cached_which <- list()
    expect_equal(test, expected)
})

# Testing for TxDbOREnsDb ----------------------------------------------------

test_that("Test xlab parameter of autoplot(TxDbOREnsDb)", {
    data <- txdb
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(TxDbOREnsDb)", {
    data <- txdb
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(TxDbOREnsDb)", {
    data <- ensdb
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test data representation of autoplot(TxDbOREnsDb)", {
    # TxDb
    data <- txdb
    test <- autoplot(data)
    test@ggplot$plot_env <- NULL
    mc <- list(autoplot=list(as.name("autoplot"), object = data))
    args <- list(data = data, truncate.gaps = FALSE, truncate.fun = NULL,
                 ratio = 0.0025, geom = "alignment", stat = "identity",
                 names.expr = "tx_id", label = TRUE)
    args.res <- c(args, list(aes()))
    p <- ggplot() + do.call(geom_alignment, args.res)
    p <- p + ggplot2::xlab("") + ggplot2::ylab("")
    p <- GGbio(p, data = data)
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # EnsDb
    data <- ensdb
    test <- autoplot(data)
    test@ggplot$plot_env <- NULL
    mc <- list(autoplot=list(as.name("autoplot"), object = data))
    args <- list(data = data, truncate.gaps = FALSE, truncate.fun = NULL,
                 ratio = 0.0025, geom = "alignment", stat = "identity",
                 names.expr = "tx_id", label = TRUE)
    args.res <- c(args, list(aes()))
    p <- ggplot() + do.call(geom_alignment, args.res)
    p <- p + ggplot2::xlab("") + ggplot2::ylab("")
    p <- GGbio(p, data = data)
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for OrganismDb -----------------------------------------------------

test_that("Test xlab parameter of autoplot(OrganismDb)", {
    data <- Homo.sapiens
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(OrganismDb)", {
    data <- Homo.sapiens
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(OrganismDb)", {
    data <- Homo.sapiens
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test data representation with which parameter of autoplot(OrganismDb)", {
    data <- Homo.sapiens
    which <- GRanges(c("chr1"), IRanges(1, 100000))
    test <- autoplot(data, which = which)
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- list(autoplot=list(as.name("autoplot"), object = data, which = which))
    columns <- c("TXNAME", "SYMBOL", "TXID", "GENEID")
    which <- range(which, ignore.strand = TRUE)
    args <- list(data = data, truncate.gaps = FALSE, truncate.fun = NULL,
                 ratio = 0.0025, geom = "alignment", stat = "identity",
                 names.expr = "SYMBOL", label = TRUE, label.color = "gray40",
                 columns = columns, which = which)
    args.res <- c(args, list(aes()))
    p <- ggbio() + do.call(geom_alignment, args.res)
    p <- p + ggplot2::xlab("") + ggplot2::ylab("")
    p <- ggbio:::cacheSet(p, which)
    p@fetchable <- TRUE
    p@cmd <- mc
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for TabixFile ------------------------------------------------------

test_that("Test data representation of autoplot(TabixFile)", {
    data <- tbx
    which <- GRanges(c("chr1", "chr2"), IRanges(c(1, 1), width=100000))
    test <- autoplot(data, which)
    mc <- c(list(as.name("autoplot")), object = data, which = which)
    data <- import(data, which=which)
    p <- autoplot(data)
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expect_equal(test, expected)
})

# Testing for BSgenome -------------------------------------------------------

data(genesymbol, package = "biovizBase")

test_that("Test xlab parameter of autoplot(BSgenome)", {
    data <- bsgenome
    test <- autoplot(data, xlab = "x-axis", which = genesymbol["NAT1"])
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(BSgenome)", {
    data <- bsgenome
    test <- autoplot(data, ylab = "y-axis", which = genesymbol["NAT1"])
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(BSgenome)", {
    data <- bsgenome
    test <- autoplot(data, main = "title", which = genesymbol["NAT1"])
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'text' parameter of autoplot(BSgenome)", {
    data <- bsgenome
    which <- genesymbol["NAT1"]
    test <- autoplot(data, which = which, geom = "text")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which, geom = "text")
    geom <- "text"
    .xlim <- c(min(start(which)), max(end(which)))
    which <- biovizBase:::rectifySeqlevelsStyle(which, data)
    seqlevelsStyle(which) <- "UCSC"
    which_backup <- which

    seqs <- getSeq(data, which, as.character = TRUE)
    seqs <- safeExplode(seqs)
    xs <- seq(start(which), length.out = width(which))
    df <- data.frame(x = xs, seqs = seqs)
    p <- ggplot(data = df)

    baseColor <- getOption("biovizBase")$DNABasesColor
    fc <- baseColor[df$seqs]

    # isDNABaseColor = TRUE
    xlab <- ylab <- ""
    aes <- aes(x = x, y = 0, label = seqs, color = seqs)
    args <- c(list(aes), size = 4)
    p <- p + ggbio:::do.ggcall(geom_text, args) +
         scale_color_manual(values = baseColor) + xlab(xlab) +
         ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # isDNABaseColor = FALSE
    which <- which_backup
    test <- autoplot(data, which = which, geom = "text", color = "white")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which,
            color = "white", geom = "text")
    aes <- aes(x = x, y = 0, label = seqs)
    args <- c(list(aes), color = "white")
    p <- ggplot(data = df)
    p <- p + ggbio:::do.ggcall(geom_text, args) + xlab(xlab) +
         ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'segment' parameter of autoplot(BSgenome)", {
    data <- bsgenome
    which <- genesymbol["NAT1"]
    test <- autoplot(data, which = which, geom = "segment")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which, geom = "segment")
    geom <- "segment"
    .xlim <- c(min(start(which)), max(end(which)))
    which <- biovizBase:::rectifySeqlevelsStyle(which, data)
    seqlevelsStyle(which) <- "UCSC"
    which_backup <- which

    seqs <- getSeq(data, which, as.character = TRUE)
    seqs <- safeExplode(seqs)
    xs <- seq(start(which), length.out = width(which))
    df <- data.frame(x = xs, seqs = seqs)
    p <- ggplot(data = df)

    baseColor <- getOption("biovizBase")$DNABasesColor
    fc <- baseColor[df$seqs]

    # isDNABaseColor = TRUE
    xlab <- ylab <- ""
    args <- list(x = as.name("x"), y = -1, xend = as.name("x"),
                 yend = 1, color = as.name("seqs"))
    args <- list(do.call(aes, args))
    p <- p + ggbio:::do.ggcall(ggplot2::geom_segment, args) +
         scale_color_manual(values = baseColor) +
         scale_y_continuous(limits = c(-10, 10))+ xlab(xlab) +
         ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # isDNABaseColor = FALSE
    which <- which_backup
    test <- autoplot(data, which = which, geom = "segment", color = "white")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which,
            color = "white", geom = "segment")
    args <- list(x = as.name("x"), y = -1, xend = as.name("x"), yend = 1)
    args <- c(list(do.call(aes, args)), color = "white")
    p <- ggplot(data = df)
    p <- p + ggbio:::do.ggcall(ggplot2::geom_segment, args) +
         scale_y_continuous(limits = c(-10, 10)) +
         xlab(xlab) + ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'point' parameter of autoplot(BSgenome)", {
    data <- bsgenome
    which <- genesymbol["NAT1"]
    test <- autoplot(data, which = which, geom = "point")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which, geom = "point")
    geom <- "point"
    .xlim <- c(min(start(which)), max(end(which)))
    which <- biovizBase:::rectifySeqlevelsStyle(which, data)
    seqlevelsStyle(which) <- "UCSC"
    which_backup <- which

    seqs <- getSeq(data, which, as.character = TRUE)
    seqs <- safeExplode(seqs)
    xs <- seq(start(which), length.out = width(which))
    df <- data.frame(x = xs, seqs = seqs)
    p <- ggplot(data = df)

    baseColor <- getOption("biovizBase")$DNABasesColor
    fc <- baseColor[df$seqs]

    # isDNABaseColor = TRUE
    xlab <- ylab <- ""
    args <- list(aes(x = x, y = 0, color = seqs))
    p <- p + ggbio:::do.ggcall(geom_point, args) +
         scale_color_manual(values = baseColor) + xlab(xlab) +
         ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # isDNABaseColor = FALSE
    which <- which_backup
    test <- autoplot(data, which = which, geom = "point", color = "white")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which,
            color = "white", geom = "point")
    args <- c(list(aes(x = x, y = 0)), color = "white")
    p <- ggplot(data = df)
    p <- p + ggbio:::do.ggcall(geom_point, args) +
         xlab(xlab) + ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'rect' parameter of autoplot(BSgenome)", {
    data <- bsgenome
    which <- genesymbol["NAT1"]
    test <- autoplot(data, which = which, geom = "rect")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which, geom = "rect")
    geom <- "rect"
    .xlim <- c(min(start(which)), max(end(which)))
    which <- biovizBase:::rectifySeqlevelsStyle(which, data)
    seqlevelsStyle(which) <- "UCSC"
    which_backup <- which

    seqs <- getSeq(data, which, as.character = TRUE)
    seqs <- safeExplode(seqs)
    xs <- seq(start(which), length.out = width(which))
    df <- data.frame(x = xs, seqs = seqs)
    p <- ggplot(data = df)

    baseColor <- getOption("biovizBase")$DNABasesColor
    fc <- baseColor[df$seqs]

    # isDNABaseColor = TRUE
    xlab <- ylab <- ""
    args <- list(xmin = as.name("x"), ymin = -1, xmax = substitute(x + 0.9),
                 ymax = 1, color = as.name("seqs"), fill = as.name("seqs"))
    args <- list(do.call(aes, args))
    p <- p + ggbio:::do.ggcall(ggplot2::geom_rect, args) +
         scale_y_continuous(limits = c(-10, 10)) +
         scale_color_manual(values = baseColor) +
         scale_fill_manual(values = baseColor) + xlab(xlab) +
         ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    # isDNABaseColor = FALSE
    which <- which_backup
    test <- autoplot(data, which = which, geom = "rect", color = "white")
    test@ggplot$plot_env <- NULL
    test@cached_item[[1]]@ggplot$plot_env <- NULL
    mc <- c(list(as.name("autoplot")), object = data, which = which,
            color = "white", geom = "rect")
    args <- list(xmin = as.name("x"), ymin = -1,
                 xmax = substitute(x + 0.9), ymax = 1)
    args <- c(list(do.call(aes, args)), color = "white")
    p <- ggplot(data = df)
    p <- p + ggbio:::do.ggcall(ggplot2::geom_rect, args) +
         scale_y_continuous(limits = c(-10, 10)) +
         xlab(xlab) + ylab(ylab) + scale_y_continuous(breaks = NULL)
    p <- GGbio(p, data = data)
    p <- ggbio:::cacheSet(p, which)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p
    expected@ggplot$plot_env <- NULL
    expected@cached_item[[1]]@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for Rle ------------------------------------------------------------

test_that("Test xlab parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test stat = 'identity' parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, stat = "identity")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL)
    plot <- ggplot() + ggbio:::do.ggcall(stat_identity, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test stat = 'bin' parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, stat = "bin")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL, nbin = 30)
    plot <- ggplot() + ggbio:::do.ggcall(stat_bin, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test stat = 'slice' parameter of autoplot(Rle)", {
    data <- xRle
    test <- autoplot(data, stat = "slice")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL)
    plot <- ggplot() + ggbio:::do.ggcall(stat_slice, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})


# Testing for RleList --------------------------------------------------------

test_that("Test xlab parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test stat = 'identity' parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, stat = "identity")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL)
    plot <- ggplot() + ggbio:::do.ggcall(stat_identity, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test stat = 'bin' parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, stat = "bin")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL, nbin = 30)
    plot <- ggplot() + ggbio:::do.ggcall(stat_bin, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test stat = 'slice' parameter of autoplot(RleList)", {
    data <- xRleList
    test <- autoplot(data, stat = "slice")
    test@ggplot$plot_env <- NULL
    xlab <- ""
    args <- list(data = data, geom = NULL)
    plot <- ggplot() + ggbio:::do.ggcall(stat_slice, args) + ggplot2::xlab(xlab)
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for ExpressionSet --------------------------------------------------

test_that("Test type = 'scatterplot.matrix' parameter of autoplot(ExpressionSet)", {
   data <- expSet
   expect_error(test <- autoplot(data, type = "scatterplot.matrix"))
})

test_that("Test type = 'heatmap' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    test <- autoplot(data, type = "heatmap")
    test@ggplot$plot_env <- NULL
    df.exp <- exprs(data)
    df <- as.data.frame(df.exp)

    colnames(df.exp) <- rownames(pData(data))
    p <- autoplot(df.exp) + ylab("Features") + xlab("Samples")
    expected <- GGbio(p, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    colnames(df.exp) <- rownames(pData(data))
    p <- autoplot(t(df.exp)) + xlab("Features") + ylab("Samples")
    pd <- pData(data)
    s <- list(theme(axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()),
              theme(legend.position = "top",
                    plot.margin = unit(c(1, 0.2, 0.5, 0.2), "lines")),
              guides(fill = guide_legend(bycol = TRUE, byrow = FALSE, ncol =  1,
                     title.theme = element_blank())))
    N <- ncol(pd)
    hts <- rep(1/N, N)
    hts <- c(hts, 4.5)
    l <- lapply(1:N, function(i) {
        autoplot(as.matrix(pd[, i, drop  = FALSE])) + s
    })
    ry <- c(rep(TRUE, N), FALSE)
    l <- c(l, list(p))
    expected <- grid::grid.draw(do.call(alignPlots, c(l, list(vertical = FALSE,
                                remove.y.axis = ry, widths = hts))))
    expect_equal(autoplot(data, type = "heatmap", pheno.plot = TRUE), expected)
})

test_that("Test type = 'pcp' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    test <- autoplot(data, type = "pcp")
    df.exp <- exprs(data)
    df <- as.data.frame(df.exp)
    p <- ggbio:::.ggpcp(df) + geom_line() + xlab("Sample Name")
    expected <- GGbio(p, data = data)
    expect_equal(test, expected)
})

test_that("Test type = 'boxplot' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    test <- autoplot(data, type = "boxplot")
    df.exp <- exprs(data)
    df <- as.data.frame(df.exp)
    p <- ggbio:::.ggpcp(df) + geom_boxplot(aes(group=variable)) + xlab("Sample Name")
    expected <- GGbio(p, data = data)
    expect_equal(test, expected)
})

test_that("Test type = 'MA' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    expect_error(test <- autoplot(data, type = "MA"))
})

test_that("Test type = 'mean-sd' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    test <- autoplot(data, type = "mean-sd")
    test@ggplot$plot_env <- NULL
    df.exp <- exprs(data)
    df <- as.data.frame(df.exp)
    require("vsn")
    res <- vsn::meanSdPlot(data, ranks = TRUE, plot = FALSE)
    xlabs <- "rank(mean)"
    p <- qplot(x = res$px, y = res$py, geom = "point") +
        geom_point(aes(x = res[[1]], y = res$sd), color = "red") +
        xlab(xlabs) + ylab("sd")
    expected <- GGbio(p, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test type = 'none' parameter of autoplot(ExpressionSet)", {
    data <- expSet
    test <- autoplot(data, type = "none")
    test@ggplot$plot_env <- NULL
    df.exp <- exprs(data)
    df <- as.data.frame(df.exp)
    df.l <- biovizBase::mold(data)
    p <- qplot(data = df.l)
    expected <- GGbio(p, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for VRanges --------------------------------------------------------

test_that("Test xlab parameter of autoplot(VRanges)", {
    data <- vr
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(VRanges)", {
    data <- vr
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(VRanges)", {
    data <- vr
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'rect' parameter of autoplot(VRanges)", {
    data <- vr
    test <- autoplot(data, geom = "rect")
    test@ggplot$plot_env <- NULL
    .xlim <- c(min(start(data)), max(end(data)))
    mc <- c(list(as.name("autoplot")), object = data, geom="rect")

    md <- biovizBase::mold(data)
    indel.col <- "gray30"
    geom <- "rect"
    baseColor <- getOption("biovizBase")$DNABasesColor
    fc1 <- baseColor[md$alt]
    fc1[is.na(fc1)] <- indel.col
    fc2 <- baseColor[md$ref]
    fc2[is.na(fc2)] <- indel.col
    md.rect <- md
    indel <- width(data) > 1 | width(alt(data)) > 1
    md.rect$ref[indel] <- "Indel"
    md.rect$alt[indel] <- "Indel"
    md.rect$ref <- factor(md.rect$ref, levels = c("A", "C", "G", "T", "Indel"))
    md.rect$alt <- factor(md.rect$alt, levels = c("A", "C", "G", "T", "Indel"))
    xlab <- ylab <- ""

    p <- ggplot(md.rect) + ggplot2::geom_segment(aes(x = midpoint-0.5, y = 1.25,
                xend = midpoint-0.5, yend = 1.35, color = ref)) +
            ggplot2::geom_rect(aes(xmin = midpoint - 0.5, ymin = 1.25, xmax = midpoint +
                0.5, ymax = 1.35, fill = ref), color = NA) +
            ggplot2::geom_segment(aes(x = midpoint, y = 1.65, xend = midpoint,
                yend = 1.75, color = alt)) +
            ggplot2::geom_rect(aes(xmin = midpoint - 0.5, ymin = 1.65, xmax = midpoint + 0.5,
                ymax = 1.75, fill = alt), color = NA) +
            scale_fill_manual(values = c(baseColor, "Indel" = indel.col)) +
            scale_color_manual(values = c(baseColor, "Indel" = indel.col)) +
            ggplot2::geom_segment(data = md, aes(x = midpoint, xend = midpoint), y = 1.4,
                yend = 1.6, arrow = arrow(length = unit(0.3,"strwidth", "A"))) +
            facet_grid (sampleNames ~ .) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    p <- GGbio(p, data = data)
    p <- p + xlim(.xlim)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    p <- p + ylim(1, 2)
    expected <- p + theme(axis.text.y=element_blank(),
                axis.ticks=element_blank())
    expected$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'none' parameter of autoplot(VRanges)", {
    data <- vr
    test <- autoplot(data, geom = "none")
    test@ggplot$plot_env <- NULL
    .xlim <- c(min(start(data)), max(end(data)))
    mc <- c(list(as.name("autoplot")), object = data, geom="none")
    md <- biovizBase::mold(data)
    indel.col <- "gray30"
    geom <- "none"
    baseColor <- getOption("biovizBase")$DNABasesColor
    fc1 <- baseColor[md$alt]
    fc1[is.na(fc1)] <- indel.col
    fc2 <- baseColor[md$ref]
    fc2[is.na(fc2)] <- indel.col
     xlab <- ylab <- ""
    p <- ggplot() + annotate("text", x = mean(.xlim), y = 0,
         label = "zoom in to show data", color = "gray 60") + xlim(.xlim) +
         ggplot2::ylab("") + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    p <- GGbio(p, data = data)
    p <- p + xlim(.xlim)
    attr(p, "geom") <- geom
    p@fetchable <- TRUE
    p@cmd <- list(mc)
    expected <- p + theme(axis.text.y=element_blank(),
                axis.ticks=element_blank())
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for VCF ------------------------------------------------------------

test_that("Test xlab parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, xlab = "x-axis")
    test <- test@ggplot$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, ylab = "y-axis")
    test <- test@ggplot$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, main = "title")
    test <- test@ggplot$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test type = 'default' parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, type = "default")
    vr <- as(data, "VRanges")
    xlab <- ""
    ylab <- ""
    expected <- autoplot(vr) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    expect_equal(test, expected)
})

# test_that("Test type = 'geno' parameter of autoplot(VCF)", {
# })

test_that("Test type = 'info' parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, type = "info")
    test@ggplot$plot_env <- NULL
    header <- metadata(data)[["header"]]
    class <- vapply(info(data)@listData, class, character(1L))
    idx <- which(class %in% c("numeric", "integer", "character", "factor"))
    temp <- granges(data)
    values(temp) <- info(data)
    df <-  biovizBase::mold(temp)
    header_rnames <- rownames(info(header))
    aes <- aes(y = END, x = start)
    plot <- ggplot(data = df) + ggbio:::do.ggcall(ggplot2::geom_bar,
            c(list(stat = "identity"), list(aes, color = "black"))) +
            xlab("") + ylab("")
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test type = 'fixed' parameter of autoplot(VCF)", {
    data <- vcf
    test <- autoplot(data, type = "fixed")
    test@ggplot$plot_env <- NULL
    header <- metadata(data)[["header"]]

    fix <- VariantAnnotation::fixed(data)
    fix <- fix[, !colnames(fix) %in% c("ALT", "REF")]
    temp <- granges(data)
    values(temp) <- fix
    fix <- temp
    values(fix)$ALT <- as.character(unlist(alt(data)))
    values(fix)$REF <- as.character(ref(data))

    fix2 <- fix
    type2 <- vector("character", length  = length(fix))
    idx <- width(values(fix)$ALT) > 1
    type2[idx] <- "I"
    type2[!idx] <- as.character(values(fix[!idx])$ALT)
    values(fix)$type <- type2
    isDNABaseColor <- FALSE
    baseColor <- getOption("biovizBase")$DNABasesColor
    .i <- "black"
    names(.i) <- "I"
    baseColor <- c(baseColor, .i)
    ir <- IRanges(start = start(fix), width = width(values(fix)$ALT))
    width(ir[idx,]) <- 1
    steps <- disjointBins(ir)
    values(fix)$stepping <- steps
    values(fix)$value <- values(fix)$ALT
    values(fix)$group <- "ALT"
    fix2 <- biovizBase::addStepping(fix2)
    idx <- width(values(fix2)$REF) > 1

    ir <- IRanges(start = start(fix), width = width(values(fix2)$REF))
    width(ir[idx,]) <- 1
    steps <- disjointBins(ir)
    values(fix2)$stepping <- steps
    type2 <- vector("character", length  = length(fix2))
    type2[idx] <- "I"
    type2[!idx] <- as.character(values(fix[!idx])$REF)
    values(fix2)$type <- type2
    values(fix2)$value <- values(fix2)$REF
    values(fix2)$group <- "REF"
    .nms <- colnames(values(fix))
    fix <- c(fix, fix2[, .nms])

    facet <- facet_grid(group ~ ., scales = "free_y")
    df <- biovizBase::mold(fix)
    aes <- aes(x = start, label = type, color = type, y = stepping)
    plot <- ggplot() + geom_text(data = df, aes) +
         scale_color_manual(values = baseColor, guide="none") +
         scale_y_continuous(breaks=NULL, labels=NULL) + facet +
         xlab("") + ylab("")
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)


    test <- autoplot(data, type = "fixed", full.string = TRUE)
    test@ggplot$plot_env <- NULL
    df <- biovizBase::mold(fix)
    df$type <- factor(df$type, levels = c(names(baseColor)))
    aes <- aes(x = start, label = value, color = type, y = stepping)
    plot <- ggplot() + geom_text(data = df, aes) +
         scale_color_manual(values = baseColor, guide="none") +
         scale_y_continuous(breaks=NULL, labels=NULL) + facet +
         xlab("") + ylab("")
    expected <- GGbio(plot, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for matrix ---------------------------------------------------------

test_that("Test xlab parameter of autoplot(matrix)", {
    data <- volMatrix
    test <- autoplot(data, xlab = "x-axis")
    test <- test$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(matrix)", {
    data <- volMatrix
    test <- autoplot(data, ylab = "y-axis")
    test <- test$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(matrix)", {
    data <- volMatrix
    test <- autoplot(data, main = "title")
    test <- test$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'raster' parameter of autoplot(matrix)", {
    data <- volMatrix
    colnames(data) <- c(1:ncol(data))
    rownames(data) <- c(1:nrow(data))
    test <- autoplot(data, geom = "raster")
    test$plot_env <- NULL
    df <- biovizBase::mold(data)
    y.lab <- rownames(data)
    y <- seq_len(nrow(data))
    x.lab <- colnames(data)
    x <- seq_len(ncol(data))
    aes <- aes(x = x, y = y, fill = value, width = 1, height = 1)
    expected <- ggplot(data = df) + ggbio:::do.ggcall(geom_raster, list(aes)) +
         theme_noexpand() + scale_y_continuous(breaks = y, labels = y.lab, expand = c(0, 0)) +
         scale_x_continuous(breaks = x, labels = x.lab, expand = c(0, 0)) +
         xlab("") + ylab("") + theme(axis.text.x=element_text(angle = 0, hjust = 0.5))
    expected$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'tile' parameter of autoplot(matrix)", {
    data <- volMatrix
    colnames(data) <- c(1:ncol(data))
    rownames(data) <- c(1:nrow(data))
    test <- autoplot(data, geom = "tile")
    test$plot_env <- NULL
    df <- biovizBase::mold(data)
    y.lab <- rownames(data)
    y <- seq_len(nrow(data))
    x.lab <- colnames(data)
    x <- seq_len(ncol(data))
    aes <- aes(x = x, y = y, fill = value, width = 1, height = 1)
    expected <- ggplot(data = df) + ggbio:::do.ggcall(geom_tile, list(aes)) +
         theme_noexpand() + scale_y_continuous(breaks = y, labels = y.lab, expand = c(0, 0)) +
         scale_x_continuous(breaks = x, labels = x.lab, expand = c(0, 0)) +
         xlab("") + ylab("") + theme(axis.text.x=element_text(angle = 0, hjust = 0.5))
    expected$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for Views ----------------------------------------------------------

test_that("Test xlab parameter of autoplot(Views)", {
    data <- views
    test <- autoplot(data, xlab = "x-axis")
    test <- test$labels$x
    expected <- "x-axis"
    expect_identical(test, expected)
})

test_that("Test ylab parameter of autoplot(Views)", {
    data <- views
    test <- autoplot(data, ylab = "y-axis")
    test <- test$labels$y
    expected <- "y-axis"
    expect_identical(test, expected)
})

test_that("Test main parameter of autoplot(Views)", {
    data <- views
    test <- autoplot(data, main = "title")
    test <- test$labels$title
    expected <- "title"
    expect_identical(test, expected)
})

test_that("Test geom = 'raster' parameter of autoplot(Views)", {
    data <- views
    y.lab <- c("A", "B", "C", "D")
    names(data) <- y.lab
    test <- autoplot(data, geom = "raster")
    test@ggplot$plot_env <- NULL
    y <- seq_len(length(data))
    xlab <- ylab <- ""
    hjust <- axis.text.angle <- 0
    plot <- function(...) {
        p <- ggplot(data, aes(group = row, x = x, y = y, fill = value)) + geom_raster(...)
    }
    expected <- plot() + theme_noexpand() + scale_y_continuous(breaks = y, labels = y.lab , expand = c(0, 0)) +
                ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                theme(axis.text.x = element_text(angle = axis.text.angle, hjust = hjust))
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'tile' parameter of autoplot(Views)", {
    data <- views
    y.lab <- c("A", "B", "C", "D")
    names(data) <- y.lab
    test <- autoplot(data, geom = "tile")
    test@ggplot$plot_env <- NULL
    y <- seq_len(length(data))
    xlab <- ylab <- ""
    hjust <- axis.text.angle <- 0
    plot <- function(...) {
        p <- ggplot(data, aes(group = row, x = x, y = y, fill = value)) + geom_tile(...)
    }
    expected <- plot() + theme_noexpand() + scale_y_continuous(breaks = y, labels = y.lab , expand = c(0, 0)) +
                ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                theme(axis.text.x = element_text(angle = axis.text.angle, hjust = hjust))
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test geom = 'line' parameter of autoplot(Views)", {
    data <- views
    y.lab <- c("A", "B", "C", "D")
    names(data) <- y.lab
    test <- autoplot(data, geom = "line")
    test@ggplot$plot_env <- NULL
    y <- seq_len(length(data))
    xlab <- ylab <- ""
    hjust <- axis.text.angle <- 0
    facets <- row ~ .
    plot <- function(...) {
        p <- ggplot(data, aes(group = row, x = x, y = value)) + geom_line(...)
    }
    expected <- plot() + facet_grid(facets) + theme_pack_panels() +
                ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                theme(axis.text.x = element_text(angle = axis.text.angle, hjust = hjust))
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for Seqinfo --------------------------------------------------------

test_that("Test ideogram = TRUE of autoplot(Seqinfo)", {
    test <- autoplot(sq["chr1"], ideogram = TRUE)
    transformedSeq <- biovizBase:::.transformSeqinfo(sq["chr1"])
    plot <- plotIdeogram(transformedSeq, as.character(seqnames(transformedSeq)))
    expected <- plot
    expect_equal(test, expected)
})

test_that("Test data representation of autoplot(Seqinfo)", {
    test <- autoplot(sq)
    test@ggplot$plot_env <- NULL
    transformedSeq <- biovizBase:::.transformSeqinfo(sq)
    plot <- ggplot() + layout_karyogram(transformedSeq, geom = NULL)
    expected <- GGbio(plot, data = sq)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

# Testing for RangedSummarizedExperiment -------------------------------------

test_that("Test type = 'heatmap' parameter of autoplot(RangedSummarizedExperiment)", {
    data <- rse
    test <- autoplot(data, type = "heatmap")
    test@ggplot$plot_env <- NULL
    ays <- assays(data)
    res <- ays[[1]]
    colnames(res) <- colnames(data)

    p <- autoplot(res) + ylab("Features") + xlab("Samples")
    expected <- GGbio(p, data = data)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)

    p <- autoplot(t(res)) + xlab("") + ylab("Samples")
    pd <- colData(data)
    s <- list(theme(axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()) ,
              theme(legend.position = "top",
                    plot.margin = unit(c(1, 0.2, 0.5, 0.2), "lines")),
              guides(fill = guide_legend(bycol = TRUE,
                     byrow = FALSE, ncol =  1, title.theme = element_blank())))

    N <- ncol(pd)
    hts <- rep(1/N, N)
    hts <- c(hts, 4.5)
    l <- lapply(1:N, function(i) {
        autoplot(as.matrix(pd[, i, drop  = FALSE])) + s
    })
    ry <- c(rep(TRUE, N), FALSE)
    l <- c(l, list(p))
    expected <- grid::grid.draw(do.call(alignPlots, c(l, list(vertical = FALSE, 
                                remove.y.axis = ry, widths = hts))))
    expect_equal(autoplot(data, type = "heatmap", pheno.plot = TRUE), expected)
})

test_that("Test type = 'pcp' parameter of autoplot(RangedSummarizedExperiment)", {
    data <- rse
    test <- autoplot(data, type = "pcp")
    ays <- assays(data)
    res <- ays[[1]]
    df <- as.data.frame(res)
    p <- ggbio:::.ggpcp(df) + geom_line() + xlab("Sample Name")
    expected <- GGbio(p, data = data)
    expect_equal(test, expected)
})

test_that("Test type = 'boxplot' parameter of autoplot(RangedSummarizedExperiment)", {
    data <- rse
    test <- autoplot(data, type = "boxplot")
    ays <- assays(data)
    res <- ays[[1]]
    df <- as.data.frame(res)
    p <- ggbio:::.ggpcp(df) + geom_boxplot(aes(group=variable)) + xlab("Sample Name")
    expected <- GGbio(p, data = data)
    expect_equal(test, expected)
})

test_that("Test type = 'link' parameter of autoplot(RangedSummarizedExperiment)", {
    data <- rse
    expect_error(autoplot(data, type = "link"))
})

test_that("Test type = 'scatterplot.matrix' parameter of autoplot(RangedSummarizedExperiment)", {
    data <- rse
    expect_error(autoplot(data, type = "scatterplot.matrix"))
})
