library(BSgenome.Hsapiens.UCSC.hg19)
gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
p1 <- autoplot(Hsapiens, which = gr, geom = "text")
library("TxDb.Mmusculus.UCSC.mm9.knownGene")
txdb <- TxDb.Mmusculus.UCSC.mm9.knownGene
p2
tracks(p1, p2,)

