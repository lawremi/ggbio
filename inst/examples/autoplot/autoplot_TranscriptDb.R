## @knitr load
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr tracks
p1 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"])

p1
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
tracks(p1, p2, heights = c(3, 1))


## by id
library(org.Hs.eg.db)
aldoa_eg <- org.Hs.egSYMBOL2EG$ALDOA
res <- biovizBase:::fetch(txdb, which = list(gene_id = aldoa_eg))
res <- exons(txdb, val = list(gene_id = aldoa_eg), columns = c("exon_id", "tx_id"))
res
res2 <- fortify(res)
fortify(res[,idx])
p1 <- ggplot() + stat_gene(txdb, which = list(gene_id = aldoa_eg))
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["ALDOA"])
p3 <- autoplot(txdb, which = list(gene_id = aldoa_eg))
p4 <- autoplot(txdb, which = genesymbol["ALDOA"])
myfun <- function(data, ...){
  ggplot() + stat_gene(data, ...)
}
p5 <- myfun(txdb, which = list(gene_id = aldoa_eg))
tracks(p1, p2, p3, p4, p5)

