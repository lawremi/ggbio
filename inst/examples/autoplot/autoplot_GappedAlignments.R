library(ggbio)
library(Rsamtools)
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE) 
p1 <- autoplot(ga)
p2 <- autoplot(ga, show.junction = TRUE)
p2
p3 <- autoplot(ga, geom = "rect")
p3 <- autoplot(ga, geom = "line", stat = "coverage")
p3
grid.arrange(p1, p2, p3, ncol = 1)
