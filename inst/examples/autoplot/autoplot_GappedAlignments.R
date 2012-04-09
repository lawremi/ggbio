## @knitr load
library(ggbio)
library(Rsamtools)

## @knitr read 
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE)

## @knitr exp
p1 <- autoplot(ga)
p2 <- autoplot(ga, show.junction = TRUE)
p3 <- autoplot(ga, geom = "rect")
p4 <- autoplot(ga, geom = "line", stat = "coverage")
tracks(p1, p2, p3, p4)


