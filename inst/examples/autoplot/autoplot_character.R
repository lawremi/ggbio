## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr bed
library(rtracklayer)
test_path <- system.file("tests", package = "rtracklayer")
test_bed <- file.path(test_path, "test.bed")
wh <- GRanges("chr7", IRanges(127472000, 127473000))
autoplot(test_bed)
autoplot(test_bed, aes(fill = score), geom  = "rect")
autoplot(test_bed, aes(fill = name))
autoplot(test_bed, aes(fill = name), which = wh)

