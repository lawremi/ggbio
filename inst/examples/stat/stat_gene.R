## @knitr load
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], fill = "gray40", geom = "gene")
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
tracks(p1, p2, heights = c(3, 1))

