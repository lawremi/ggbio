## @knitr load
## ==========================================================
## Load packages
## ==========================================================
## Load gene features for human
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
## ==========================================================
## Create tracks
## ==========================================================
## create two tracks
## full gene model
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "gene")
## reduced gene model
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
## @knitr align.plots
## ==========================================================
## align.plots
## ==========================================================

