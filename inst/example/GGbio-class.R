## TODO:
## 1. Txdb
## 2. Solve the seqinfo issues

plot(1:10)
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



library(ggbio)
library(ggplot2)
library(GenomicRanges)

gr = GRanges("1",
    IRanges(1:5, 1:5))

set.seed(1)
gr$e = runif(5)
gr$l = runif(5, -1, 0)
gr$u = runif(5, 1, 2)

p = autoplot(gr, geom = "pointrange", aes_string(y = "e", ymin = "l", ymax = "u"))

t = tracks(p, p, p)
t
t = tracks(p, p, p, title = "title")
t = tracks(p, p, p, title = "")
t = tracks(p1 = p, p2 = p, p3 = p, title = "title", xlab = "xlab")
print(t)

df1 <- data.frame(time = 1:100, score = sin((1:100)/20)10)
p1 <- qplot(data = df1, x = time, y = score, geom = "line")
df2 <- data.frame(time = 30:120, score = sin((30:120)/20)10, value = rnorm(120-30 + 1))
p2 <- ggplot(data = df2, aes(x = time, y = score)) +
  geom_line() + geom_point(size = 4, aes(color = value))

plot two tracks with a label - this looks OK

tracks (p1, p2, main="myTitle")

plot two labelled tracks - this look OK

tracks (p1=p1, p2=p2)

adding title to the plot with labelled tracks messes up alignment of the labels with the plot

tracks (p1=p1, p2=p2, main="myTitle")

## support VRanges
# construction
library(ggbio)
library(GenomicRanges)
library(VariantAnnotation)
vr <- VRanges(seqnames = c("chr1", "chr2"),
              ranges = IRanges(c(1, 10), c(5, 20)),
              ref = c("T", "A"), alt = c("C", "T"),
              refDepth = c(5, 10), altDepth = c(7, 6),
              totalDepth = c(12, 17), sampleNames = letters[1:2],
              hardFilters =
                FilterRules(list(coverage = function(x) totalDepth > 10)),
              softFilterMatrix =
                FilterMatrix(matrix = cbind(depth = c(TRUE, FALSE)),
                             FilterRules(depth = function(x) altDepth(x) > 6)),
              tumorSpecific = c(FALSE, TRUE))

## simple accessors
vr
ref(vr)
as(vr, "data.frame")
library(biovizBase)
mold(vr)
altDepth(vr)
vr$tumorSpecific
called(vr)

data("genesymbol", package = "biovizBase")
genesymbol["BRCA1"]
param <- ScanVcfParam(which = GRanges("17", IRanges(41196313, 41277500)))
vcf <- readVcf("/Users/tengfei/Documents/Data/sbgtest/1000G_phase1.snps.high_confidence.b37.vcf.gz",
               genome = "hg19", param = param)
vcf
vr <- as(vcf, "VRanges")
library(biovizBase)
md <- mold(vr[1:10])
head(md)
library(grid) # needed for arrow function
library(gridExtra)
head(md)
p <- ggplot(data.frame(x = range(md$midpoint),
                  y = c(1, 2))) + geom_blank( aes(x = x, y = y)) +
  ggplot2::geom_segment(data = md, aes(x = midpoint, xend = midpoint), y = 1.4,
               yend = 1.6, arrow = arrow(length = unit(0.2,"strwidth", "A"))) +
  annotate("text", x = md$midpoint, y = 1.75, label = md$alt) +
  annotate("text", x = md$midpoint, y = 1.25, label = md$ref) +
  theme_alignment()  +
  theme(aspect.ratio = 1/10)
p
class(p)


library(ggbio)
tracks(list(p))
tracks(p = p)
ggbio:::.supportedPlots
extends(class(p), 'gg')
tracks(p = ggbio(p))

library(ggbio)
library(IRanges)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), seq(10, 0.001, length = 500))
xVector <- dnorm(1:5e3, mean = 1e3, sd = 200)
xRle <- Rle(xVector)
v1 <- Views(xRle, start = sample(.4e3:.6e3, size = 50, replace = FALSE), width =1000)
autoplot(v1)

## let's add  more ideogram data
library(biovizBase)
?getIdeogram
hg19 <- getIdeogram(genome = "hg19")
hg19

## ggbio

p <- plotGrandLinear(gr.snp, aes(y = pvalue), color = c("#7fc97f", "#fdc086"))
vline.df <- p@ggpplot$data
vline.df <- do.call(rbind, by(vline.df, vline.df$seqnames, function(dd){
  data.frame(start = min(dd$start),
             end = max(dd$end))
}))
## compute gap
gap <- (vline.df$start[-1] + vline.df$end[-nrow(vline.df)])/2
p + geom_vline(xintercept = gap, alpha = 0.5, color = 'gray70') + theme(panel.grid = element_blank())
theme_gray()

library(ggplot2)
library(proto)




qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
  geom_text2(colour = "red", fc = c("black", "red"))
library(grid)

library(BiocManager)
BiocManager::install("FDb.UCSC.snp137common.hg19")
## load the library
library(FDb.UCSC.snp137common.hg19)
## list the contents that are loaded into memory
ls(’package:FDb.UCSC.snp137common.hg19’)
## show the db object that is loaded by calling its name
FDb.UCSC.snp137common.hg19
## extract features for use in annotating data
snp137common <- features(FDb.UCSC.snp137common.hg19)
met <- metadata(FDb.UCSC.snp137common.hg19) ## need to fetch genome


library(GenomicRanges)
snp.gr <- GRanges("chr17", IRanges(41224057,41244883))
data(genesymbol)
gr <- GRanges("chr17", IRanges(41196312, 41277500))
gr

library(VariantAnnotation)
fl = "/Users/tengfei/Documents/Data/sbgtest/17-1409-CEU.vcf.gz"
vcf.brca1 <- readVcf(fl, genome = "hg19", param = ScanVcfParam(which = gr))

vcf.brca1


library(ggbio)
autoplot(vcf.brca1)
?readVcf

writeVcf(vcf.brca1, "/Users/tengfei/Documents/Data/sbgtest/17-1409-CEU-brca1.vcf.gz", index = TRUE)




library(biovizBase)

genesymbol

library(ggbio)
library(biovizBase)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
data(genesymbol, package = "biovizBase")
wh <- genesymbol[c("BRCA1", "NBR1")]
temp <- crunch(txdb, which = wh)
temp2 <- crunch(txdb, which = wh)
temp3 <- temp2
names(values(temp3))[4] <- "model"
temp2
temp.l <- split(temp2, temp2$tx_name)
temp.l


autoplot(temp.l, geom = "alignment")
ggbio() + geom_alignment(temp.l, aes(type = type))
ggbio() + geom_alignment(temp.l[1:3])
ggbio() + geom_alignment(temp.l)
p2 <- ggbio() + geom_alignment(temp.l, stat = "reduce")
p1 <- ggbio() + geom_alignment(temp.l, aes(type = model))
p1
names(temp.l)
height(p1) <- 20
height(p2) <- 1
tracks(p1, p2) ## to make it look perfect
height(p2)
names(temp.l)
wh

ggplot() + geom_alignment(txdb, which = wh)
ggplot(txdb) + geom_alignment(which = wh)
ggplot(txdb) + geom_alignment(which = wh, names.expr = "tx_id(gene_id)")
ggplot(txdb) + geom_alignment(which = wh, names.expr = "tx_id:::gene_id")
ggplot() + geom_alignment(data = txdb, which = wh, names.expr = "gene_id")
ggplot(txdb) + geom_alignment(which = wh)

autoplot(txdb, which = wh)
autoplot(txdb, which = wh, names.expr = "tx_id:::gene_id")
autoplot(txdb, which = wh, names.expr = "tx_id:::gene_id", stat = "reduce")


library(Homo.sapiens)
library(ggbio)
data(genesymbol, package = "biovizBase")
wh <- genesymbol[c("BRCA1", "NBR1")]

ggplot() + geom_alignment(Homo.sapiens, which = wh)
ggplot() + geom_alignment(Homo.sapiens, which = wh, names.expr = "SYMBOL(TXNAME)")
ggplot() + geom_alignment(Homo.sapiens, which = wh, stat = "reduce")

ggplot() + geom_alignment(Homo.sapiens, which = wh, names.expr = "SYMBOL(GENEID)",
                          stat = "reduce")


autoplot(Homo.sapiens, which  = wh)
p <- autoplot(Homo.sapiens, which  = wh, label.color = "gray40", color = "brown",
          fill = "brown")
p <- autoplot(Homo.sapiens, which = wh, stat = "reduce")
autoplot(Homo.sapiens, which  = wh, label.color = "gray40", gap.geom = "")

p + zoom() ## fix me, arrow cannot be re-drawned
p + prevView()
p + nextView()



