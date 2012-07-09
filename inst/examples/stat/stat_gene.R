## @knitr load
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
## RBM17
library(grid)
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"])
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"], names.expr = "tx_name::gene_id")
p1
p1 + scale_x_sequnit("bp")
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"], arrow.rate = 0.03,
                           length = unit(0.2, "cm"))
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"], arrow.rate = 0.03,
                           length = unit(0.2, "cm"), gap.geom = "chevron")
p1
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
p1
p1 <- ggplot() + stat_reduce(txdb, which = GRanges("chr1", IRanges(1, 100)))
library(grid)
p1
convertX(unit(1, "strwidth", "Hello"), "npc")
convertX(strwidth))

