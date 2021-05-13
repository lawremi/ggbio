context("plotSpliceSum")

source('data.R')

test_that("Test plotSpliceSum(c('character', 'GRangesList')", {
    bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
    data(genesymbol, package = "biovizBase")
    exons <- exonsBy(txdb, by = "tx")
    exons.rbm17 <- subsetByOverlaps(exons, genesymbol["RBM17"])
    seqlevels_in_bam <- seqlevels(BamFile(bamfile))
    exons.rbm17 <- keepSeqlevels(exons.rbm17, seqlevels_in_bam)
    test <- plotSpliceSum(bamfile, exons.rbm17)
    test@ggplot$plot_env <- NULL
    freq <- biovizBase:::spliceSummary(bamfile, exons.rbm17, weighted = TRUE)
    expected <- autoplot(exons.rbm17, freq = freq)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})

test_that("Test plotSpliceSum(c('character', 'TxDb')", {
    bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
    seqlevels_in_bam <- seqlevels(BamFile(bamfile))
    txdb <- keepSeqlevels(txdb, seqlevels_in_bam)
    test <- plotSpliceSum(bamfile, txdb, which = genesymbol["RBM17"])
    test@ggplot$plot_env <- NULL
    exons <- exonsBy(txdb, by = "tx")
    exons <- subsetByOverlaps(exons, genesymbol["RBM17"])
    freq <- biovizBase:::spliceSummary(bamfile, exons, weighted = TRUE)
    expected <- autoplot(exons, freq = freq)
    expected@ggplot$plot_env <- NULL
    expect_equal(test, expected)
})
