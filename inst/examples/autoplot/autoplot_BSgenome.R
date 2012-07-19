library(BSgenome.Hsapiens.UCSC.hg19)
autoplot(Hsapiens, which = resize(genesymbol["ALDOA"], width = 50))
autoplot(Hsapiens, which = genesymbol["ALDOA"], geom = "rect")

## FIXME:
## autoplot(Hsapiens, which = genesymbol["ALDOA"], geom = "segment")

