## @knitr load
library(ggbio)
data(genesymbol, package = "biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

## @knitr theme:default
p <- autoplot(txdb, which = genesymbol["RBM17"])
p

## @knitr theme:alignment
p + theme_alignment(border = TRUE, grid = FALSE, label = TRUE) 




