## TODO:
## 1. Txdb
## 2. Solve the seqinfo issues


library(ggbio)
library(ggplot2)
p <- ggplot(data = mtcars)
class(p)
p  <- p + geom_point(aes(x = mpg, y = wt), color = "red")
p
N <- 100
library(GenomicRanges)
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                start = sample(1:300, size = N, replace = TRUE),
                width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

seqlengths(gr) <- c(400, 500, 700)
values(gr)$to.gr <- gr[sample(1:length(gr), size = length(gr))]
grr <- GRanges(c("chr1", "chr1", "chr2"), IRanges(1, 50))

autoplot(gr, facets = grr)
ggbio() + geom_rect(gr)

ggplot() + circle(gr, geom = "ideo", fill = "gray70") +
     circle(gr, geom = "bar", aes(fill = score, y = score)) + 
     circle(gr, geom = "point", color = "red", grid = TRUE, aes(y = score)) + 
     circle(gr, geom = "link", linked.to = "to.gr", r = 0, )


## doesn't pass gr to the ggplot
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
  layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6,
                trackWidth = 1)

p <- ggplot(gr) + layout_circle() + geom_bar(aes(fill = score, y = score))
p
library(grid)
p <- ggplot() + geom_rect(gr)
p
genPlots(list(p, p, gr, txdb))
tracks(p)
p + xlim(100, 200)
p <- ggplot(gr) + geom_rect(which = gr)


tracks(p = p, p2 = p) + xlim(1, 3)

gr1 <- GRanges("chr1", IRanges(1, 3))
gr2 <- GRanges("chr1", IRanges(c(2, 4, 1), c(3, 5, 2)))
countOverlaps(gr2, gr1, type = "within")
cached_item(p1)

## test cache ability
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
gr1 <- genesymbol["ALDOA"]
gr1
gr2 <- GRanges("chr16", IRanges(30074491, 30075733))
gr3 <- GRanges("chr12", IRanges(30074491, 30081733))

p1 <- autoplot(txdb, which = genesymbol["ALDOA"])
p1
## this should work
myfunc <- function(x, ...){
  autoplot(x, ...)
}
ylab = "asdfasdf"
p1 <- myfunc(txdb, xlab = "lalaal", ylab = ylab)
p.tx <- autoplot(txdb)
p <- p.tx + xlim(gr1)
p
p + xlim(gr2)
p + xlim(gr3)

## fixme:
## Need to add a marker called null graphics need to sync xlim wiht
tracks(gr1, p1)
tracks(txdb, xlim = gr1)
library(grid)
getGrFromXlim(xlim(gr1))
p1

tracks(p1)
library(grid)
p1
p1 + xlim(gr1)
p1
p3 <- p1 + xlim(gr3)
p3
class( xlim(gr3))
ggbio:::cached(p1)
res <- xlim(gr2)
res <- xlim_car(res)

## test bamfile
fl <- "~/Datas/seqs/ENCODE/cshl/wgEncodeCshlLongRnaSeqGm12878CellPapAlnRep1.bam"
fl1 <- "~/Datas/seqs/ENCODE/cshl/wgEncodeCshlLongRnaSeqK562CellPapAlnRep1.bam"

library(Rsamtools)
bf <- BamFile(fl)
p <- autoplot(bf)
p <- p + xlim(gr1)
p
p + xlim(gr2)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(Rsamtools)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
fl <- "~/Datas/seqs/ENCODE/cshl/wgEncodeCshlLongRnaSeqGm12878CellPapAlnRep1.bam"
bf <- BamFile(fl)
tks <- tracks(coverage = bf, model = txdb, xlim = gr1)
tks
ggsave("~/Desktop/tks.jpg")
bfl <- BamFileList(c(fl, fl1))

autoplot(bfl)


## subset tracks
p <- ggplot(data = mtcars)
p1  <- p + geom_point(aes(x = mpg, y = wt), color = "red")
p2  <- p + geom_point(aes(x = mpg, y = wt), color = "blue")
p3  <- p + geom_point(aes(x = mpg, y = wt), color = "green")

tks <- tracks(p1 = p1, p2 = p2, p3 = p3)
names(tks@grobs[c(1, 3)])
tk <- tks[c(1, 3)]
tk
names(tk@grobs)
tk <- tks[1:2]
names(tk@grobs)
tk@label


## TODO: make a better looking txbd
library(ggbio)
p <- qplot(data = mtcars, x = mpg,  y = wt, facets = cyl ~ .)
p1 <- qplot(data = mtcars, x = mpg,  y = wt)
tracks(p1 = p, p2 = p1)


## 
library(ggbio)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
data(genesymbol, package = "biovizBase")
p <- autoplot(txdb, which = genesymbol["BRCA1"])
data(genesymbol, package = "biovizBase")
p <- autoplot(txdb, which = genesymbol["BRCA1"])
autoplot(keepSeqlevels(genesymbol[1:100], "chr1"))
class(p)
is

## 



