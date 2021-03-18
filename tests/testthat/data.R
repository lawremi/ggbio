require(GenomicRanges)
N <- 100
data <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"),
              size = N, replace = TRUE),
              IRanges(start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"),
              size = N, replace = TRUE),
              pair = sample(letters, size = N, replace = TRUE))

lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)
require(Rsamtools)
bamFile <- system.file("extdata", "ex1.bam", package="Rsamtools")
bamFile <- BamFile(bamFile)
