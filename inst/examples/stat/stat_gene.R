## @knitr load
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], fill = "gray40")
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], stat = "reduce")
tracks(p1, p2, heights = c(3, 1))
library(biovizBase)
p3 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"],
                           truncate.gaps = TRUE)
p3
autoplot(txdb, which = genesymbol["RBM17"],
                           truncate.gaps = TRUE)

## @knitr NULLL
p1 <- ggplot() + stat_gene(txdb, which = GRanges("chr1", IRanges(1, 100)), fill = "gray40", stat = "reduce")
p2 <- ggplot() + stat_gene(txdb, which = GRanges("chr1", IRanges(1, 100)), fill = "gray40")
