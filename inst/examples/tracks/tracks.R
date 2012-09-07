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

## @knitr NULL
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, nocl = 1)))
library(ggbio)
p1 <- qplot(data = mtcars, x = mpg, y = wt, ylab = "ylab", facets = gear ~ .)
p2 <- qplot(data = mtcars, x = wt, y = mpg, geom = "line", ylab = "")
tracks(list(a = p1, b = p2))
tracks(list(a = p1, p2))
tracks(p1, p2)
tracks("DNA seq" = p1, "RNA-seq" = p2)
png("~/Desktop/testTrack.png", height = 700, width = 700)
tracks(list("Rna sesq" = p1, "DNA-seq" = p2), label.width = unit(3, "line"),
       heights = c(1, 3), label.text.cex = 1.5, label.text.color = "white" )
dev.off()
list(list(a = 2, b = 2))[[1]][[1]]
list(2, 2)[[1]]

## todo axis/
library(ggbio)
p1 <- qplot(data = mtcars, x = mpg, y = cyl) + ylab("") +  scale_y_continuous(breaks = NULL)
p2 <- qplot(data = mtcars, x = cyl, y = mpg, color = mpg)
p3 <- qplot(data = mtcars, x = mpg, y = cyl, facets = cyl ~ .)
p3
library(ggplot2)

library(ggplot2)
traceback()

alignPlots(p1, p2, p3)
alignPlots(a = p1, c = p2, d = p3)
tracks(a = p1, b = p2, c = p3)
alignPlots(p2, p3)
alignPlots(p1, p2)
grid.draw(g1)
print(g1)
grid.layout(g1)

a <- gtable(unit(1:3, c("cm")), unit(5, "cm"))
a
gtable_show_layout(a)

                                        # Add a grob:
rect <- rectGrob(gp = gpar(fill = "black"))
a <- gtable_add_grob(a, rect, 1, 1)
a
plot(a)

