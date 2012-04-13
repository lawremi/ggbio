## @knitr load
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
p1 <- autoplot(txdb, which = genesymbol["ALDOA"])
p2 <- autoplot(txdb, which = genesymbol["ALDOA"], stat = "reduce")
tracks(p1, p2, heights = c(3, 1))


## @knitr id
library(org.Hs.eg.db)
aldoa_eg <- org.Hs.egSYMBOL2EG$ALDOA
p3 <- autoplot(txdb, which = list(gene_id = aldoa_eg))
p3

