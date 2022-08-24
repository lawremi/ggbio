context("stat_mismatch")

source('data.R')

# Testing for GRanges --------------------------------------------------------

test_that("Test error message for invalid type of data of stat_mismatch(GRanges)", {
    expect_error(stat_mismatch(data))
})

data(genesymbol, package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
data <- biovizBase::pileupAsGRanges(bamfile, region = genesymbol["RBM17"])
data <- biovizBase::pileupGRangesAsVariantTable(data, Hsapiens)

test_that("Test xlab parameter of stat_mismatch(GRanges)", {
    test <- stat_mismatch(data, xlab = "x-axis")
    # select elements of 'labels' class from a 'test' list
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("x-axis"), ylab("Counts"))
    expect_identical(test, expected)
})

test_that("Test ylab parameter of stat_mismatch(GRanges)", {
    test <- stat_mismatch(data, ylab = "y-axis")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Genomic Coordinates"), ylab("y-axis"))
    expect_identical(test, expected)
})

test_that("Test main parameter of stat_mismatch(GRanges)", {
    test <- stat_mismatch(data, main = "Title")
    test <- test[vapply(test, function(x) identical(class(x), "labels"), logical(1L))]
    expected <- list(xlab("Genomic Coordinates"), ylab("Counts"), labs(title = "Title"))

})

make_stat_mismatch_GRanges <- function(data, geom) {
    df <- mold(data)
    df.unmatch <- df[!df$match, ]
    pos <- min(df$start):max(df$end)
    idx <- ! (pos %in% df$start)
    df.bg <- df[,c("seqnames", "start", "end", "width", "strand", "depth")]
    df.bg.extra <- data.frame(seqnames = unique(as.character(df.bg$seqnames)),
                              start = pos[idx], end = pos[idx],
                              width = 1, strand = "*", depth = 0)
    df.bg <- rbind(df.bg, df.bg.extra)
    df.bg <- df.bg[order(df.bg$start),]
    df.bg <- rbind(df.bg[1,], df.bg)
    df.bg <- rbind(df.bg, df.bg[nrow(df.bg),])
    df.bg[c(1, nrow(df.bg)),]$depth <- 0
    idx <- order(df.unmatch$start, df.unmatch$read)
    x <- df.unmatch[idx,]
    eds <- unlist(by(df.unmatch$count, df.unmatch$start, cumsum))
    eds <- as.numeric(eds)
    sts <- unlist(by(df.unmatch$count, df.unmatch$start, function(x) {
        N <- length(x)
        c(0,cumsum(x)[-N])
    }))
    sts <- as.numeric(sts)
    df.unmatch$eds <- eds
    df.unmatch$sts <- sts
    idx <- order(df.bg$start)
    df.bg <- df.bg[idx,]
    DNABasesColor <- getBioColor("DNA_BASES_N")
    aes.res <- aes(x = start, y = depth)
    args.non <- list(fill = I("gray70"))
    args.res <- c(list(data = df.bg), list(aes.res), args.non)
    p <- list(ggbio:::do.ggcall(ggplot2::geom_polygon, args.res))
    if(geom == "segment") {
        aes <- aes(x = start, y = sts, xend = start, yend = eds, color = read)
        p <- c(p, list(ggplot2::geom_segment(data = df.unmatch, aes)),
               list(scale_color_manual(values = DNABasesColor)))
    }
    if(geom == "bar") {
        aes <- aes(xmin = start - 0.5, ymin = sts, xmax = start + 0.5, ymax = eds,
                   color = read, fill = read)
        p <- c(p, list(ggplot2::geom_rect(data = df.unmatch, aes)),
               list(scale_color_manual(values = DNABasesColor)),
               list(scale_fill_manual(values = DNABasesColor)))
    }
    p <- c(p, list(xlab("Genomic Coordinates")), list(ylab("Counts")))
    attr(p, "isStat") <- TRUE
    p
}

test_that("Test geom = 'segment' of stat_mismatch(GRanges) ", {
    test <- stat_mismatch(data, geom = "segment")
    expected <- make_stat_mismatch_GRanges(data, "segment")
    expect_equal(test, expected)
})

test_that("Test geom = 'bar' of stat_mismatch(GRanges) ", {
    test <- stat_mismatch(data, geom = "bar")
    expected <- make_stat_mismatch_GRanges(data, "bar")
    expect_equal(test, expected)
})

# Testing for BamFile --------------------------------------------------------
