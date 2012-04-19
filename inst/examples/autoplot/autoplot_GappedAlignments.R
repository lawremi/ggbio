## @knitr load
library(ggbio)
library(Rsamtools)

## @knitr read 
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE)

## @knitr exp
p1 <- autoplot(ga)
p2 <- autoplot(ga, geom = "rect")
p3 <- autoplot(ga, geom = "line", stat = "coverage")
tracks(p1, p2, p3)

## @knitr NULL
library(ggbio)
library(Rsamtools)

## @knitr read 
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE)


grl <- grglist(ga)
autoplot(grl, geom = "rect", type = "sashimi")
Rprof()
autoplot(grl)
grl
Rprof(NULL)
summaryRprof()
autoplot(unlist(grl), geom = "rect")
autoplot(grl, gap.geom = "segment")

autoplot(unlist(grl), stat = "coverage", geom = "area")

reads <- GappedAlignments(
  names = c("a","b","c","d","e","f","g"),
  seqnames = Rle(c(rep(c("chr1", "chr2"), 3), "chr1")),
  pos = as.integer(c(1400, 2700, 3400, 7100, 4000, 3100, 5200)),
  cigar = c("500M", "100M", "300M", "500M", "300M", "50M200N50M", "50M150N50M"),
  strand = strand(rep.int("+", 7L)))

autoplot(ga, aes(color = strand, fill = strand))
autoplot(reads)
autoplot(grglist(reads))
autoplot(grglist(reads), group.selfish = TRUE)

autoplot(grglist(reads), geom = "rect")
autoplot(grglist(reads), gap.geom = "segment")
reads <- ga
autoplot(reads)
autoplot(reads, group.selfish = TRUE)
autoplot(reads, group.selfish = TRUE, gap.geom = "segment")
autoplot(reads, geom = "rect")
autoplot(reads, stat = "coverage")
autoplot(reads, stat = "coverage", geom = "area")
grl
genome(grl) <- "gen"
getXLab(grl)
getXLab(ga)
