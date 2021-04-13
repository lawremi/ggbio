# GRanges
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

# Rle and RleList
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)
require(Rsamtools)
ex1_file <- system.file("extdata", "ex1.bam", package="Rsamtools")

# BamFile
bamFile <- BamFile(ex1_file)

# TxDb
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

# EnsDb
library(EnsDb.Hsapiens.v75)
ensdb <- EnsDb.Hsapiens.v75

# OrganismDb
library(Homo.sapiens)

# TabixFile
fl <- system.file("extdata", "example.gtf.gz", package="Rsamtools")
tbx <- TabixFile(fl)

# BSgenome
library(BSgenome.Hsapiens.UCSC.hg19)
bsgenome <- BSgenome.Hsapiens.UCSC.hg19

# ExpressionSet
data(sample.ExpressionSet)
expSet <- sample.ExpressionSet

# VRanges
library(VariantAnnotation)
vr <- VRanges(seqnames = c("chr1", "chr2"),
              ranges = IRanges(c(1, 10), c(5, 20)),
              ref = c("T", "A"), alt = c("C", "T"),
              refDepth = c(5, 10), altDepth = c(7, 6),
              totalDepth = c(12, 17), sampleNames = letters[1:2],
              hardFilters =
                FilterRules(list(coverage = function(x) totalDepth > 10)),
              softFilterMatrix =
                FilterMatrix(matrix = cbind(depth = c(TRUE, FALSE)),
                             FilterRules(depth = function(x) altDepth(x) > 6)),
              tumorSpecific = c(FALSE, TRUE))

# VCG
vcf <- as(vr, "VCF")

# matrix
volMatrix <- volcano[20:70, 20:60] - 150

# Views
views <- Views(xRle, start=4:1, end=4:7)

# Seqinfo
data(hg19Ideogram, package = "biovizBase")
sq <- seqinfo(hg19Ideogram)

# RangedSummarizedExperiment
nrows <- 200; ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                     IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                     strand=sample(c("+", "-"), 200, TRUE),
                     feature_id=sprintf("ID%03d", 1:200))
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                     row.names=LETTERS[1:6])
rse <- SummarizedExperiment(assays=SimpleList(counts=counts),
                            rowRanges=rowRanges, colData=colData)
