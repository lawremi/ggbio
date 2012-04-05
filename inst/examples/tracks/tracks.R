## @knitr load
## ==========================================================
## Load packages
## ==========================================================
## Load gene features for human
library(ggbio)
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
## building tracks
obj <- tracks(p1, p2, heights = c(3, 1))
## showing 
obj


## @knitr align.plots
## ==========================================================
## align.plots
## ==========================================================
align.plots(p1, p2)

## @knitr reset
## ==========================================================
## test reset/backup
## ==========================================================
## create tracks
obj <- tracks(p1, p2, heights = c(3, 1))
## show it
obj
## three ways to change x limits, IRanges/GRanges/numeric
xlim(obj) <- IRanges(start = 6145000, end = 6150000)
xlim(obj) <- GRanges("chr1", c(start = 6145000, end = 6150000))
xlim(obj) <- c(6145000, 6150000)
## show it
obj
## reset to original setting
obj <- reset(obj)
## get back
obj
## we could save a statue of the tracks to backup and then
## reset will get that copy back
xlim(obj) <- c(6145000, 6150000)
obj <- backup(obj)
obj@xlim <- c(6135000, 6150000)
obj
obj <- reset(obj)
obj

## @knitr utils
## ==========================================================
## utils
## ==========================================================
## summary information about a track
summary(obj)
## update a x limits on the fly, this is useful when you try to
## keep the view open and tweak with limits on the fly.
update(obj, xlim  = c(6130000, 6150000))

## @knitr opts
## ==========================================================
## options
## ==========================================================
## To make it easy, you could just apply any *options* by using "+"
## and this will apply it to every plot in the track.
obj + theme_bw() 

