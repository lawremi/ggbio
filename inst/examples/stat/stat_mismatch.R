require(ggbio)
require(BSgenome.Hsapiens.UCSC.hg19)
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## pgr <- pileupAsGRanges(bamfile, region = genesymbol["RBM17"])
pgr <- pileupAsGRanges(bamfile, region = genesymbol["ALDOA"])
pgr.match <- pileupGRangesAsVariantTable(pgr, genome = Hsapiens)

ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE)
ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE)
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE, geom = "bar")

## ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE) +
##   coord_cartesian(xlim = c(6134000, 6135000),wise = TRUE) + theme_bw()

bf <- BamFile(bamfile)
ggplot() + stat_mismatch(bf, which = genesymbol["RBM17"],
                         bsgenome = Hsapiens,show.coverage = TRUE) +
  coord_cartesian(xlim = c(6134000, 6135000), wise = TRUE) + theme_bw()

