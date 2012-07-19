### R code from vignette source 'autoplot.Rnw'

###################################################
### code chunk number 1: load
###################################################
library(ggbio)


###################################################
### code chunk number 2: simul
###################################################
set.seed(1)
N <- 1000
library(GenomicRanges)
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

idx <- sample(1:length(gr), size = 50)


###################################################
### code chunk number 3: default
###################################################
autoplot(gr[idx])


###################################################
### code chunk number 4: bar-default-pre
###################################################
set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b2 <- GRanges(seqnames = "chr2", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b <- c(gr.b, gr.b2)
head(gr.b)


###################################################
### code chunk number 5: bar-default
###################################################
p1 <- autoplot(gr.b, geom = "bar")
## use value to fill the bar
p2 <- autoplot(gr.b, geom = "bar", aes(fill = value))
tracks(default = p1, fill = p2)


###################################################
### code chunk number 6: autoplot.Rnw:236-237
###################################################
autoplot(gr[idx], geom = "arch", aes(color = value), facets = sample ~ seqnames)


###################################################
### code chunk number 7: gr-group
###################################################
gra <- GRanges("chr1", IRanges(c(1,7,20), end = c(4,9,30)), group = c("a", "a", "b"))
## if you desn't specify group, then group based on stepping levels, and gaps are computed without
## considering extra group method
p1 <- autoplot(gra, aes(fill = group), geom = "alignment")
## when use group method, gaps only computed for grouped intervals.
## default is group.selfish = TRUE, each group keep one row.
## in this way, group labels could be shown as y axis.
p2 <- autoplot(gra, aes(fill = group, group = group), geom = "alignment")
## group.selfish = FALSE, save space
p3 <- autoplot(gra, aes(fill = group, group = group), geom = "alignment", group.selfish = FALSE)
tracks('non-group' = p1,'group.selfish = TRUE' = p2 , 'group.selfish = FALSE' = p3)


###################################################
### code chunk number 8: gr-facet-strand
###################################################
autoplot(gr, stat = "coverage", geom = "area", 
         facets = strand ~ seqnames, aes(fill = strand))


###################################################
### code chunk number 9: gr-autoplot-circle
###################################################
autoplot(gr[idx], layout = 'circle') 


###################################################
### code chunk number 10: gr-circle
###################################################
seqlengths(gr) <- c(400, 500, 700)
values(gr)$to.gr <- gr[sample(1:length(gr), size = length(gr))]
idx <- sample(1:length(gr), size = 50)
gr <- gr[idx]
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, 
                aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
  layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6, trackWidth = 1)


###################################################
### code chunk number 11: seqinfo-src
###################################################
data(hg19Ideogram, package = "biovizBase")
sq <- seqinfo(hg19Ideogram)
sq


###################################################
### code chunk number 12: seqinfo
###################################################
autoplot(sq[paste0("chr", c(1:22, "X"))])


###################################################
### code chunk number 13: ir-load
###################################################
set.seed(1)
N <- 100
ir <-  IRanges(start = sample(1:300, size = N, replace = TRUE),
               width = sample(70:75, size = N,replace = TRUE))
## add meta data 
df <- DataFrame(value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))
values(ir) <- df
ir


###################################################
### code chunk number 14: ir-exp
###################################################
p1 <- autoplot(ir)
p2 <- autoplot(ir, aes(fill = pair)) + opts(legend.position = "none")
p3 <- autoplot(ir, stat = "coverage", geom = "line", facets = sample ~. )
p4 <- autoplot(ir, stat = "reduce")
tracks(p1, p2, p3, p4)


###################################################
### code chunk number 15: grl-simul
###################################################
set.seed(1)
N <- 100
## ======================================================================
##  simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(30:40, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))


grl <- split(gr, values(gr)$pair)


###################################################
### code chunk number 16: grl-exp
###################################################
## default gap.geom is 'chevron'
p1 <- autoplot(grl, group.selfish = TRUE)
p2 <- autoplot(grl, group.selfish = TRUE, main.geom = "arrowrect", gap.geom = "segment")
tracks(p1, p2)


###################################################
### code chunk number 17: grl-name
###################################################
autoplot(grl, aes(fill = ..grl_name..))
## equal to 
## autoplot(grl, aes(fill = grl_name))


###################################################
### code chunk number 18: rle-simul
###################################################
library(IRanges)
library(ggbio)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))

## @knitr create
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRle


###################################################
### code chunk number 19: rle-bin
###################################################
p1 <- autoplot(xRle)
p2 <- autoplot(xRle, nbin = 80)
p3 <- autoplot(xRle, geom = "heatmap", nbin = 200)
tracks('nbin = 30' = p1, "nbin = 80" = p2, "nbin = 200(heatmap)" = p3)


###################################################
### code chunk number 20: rle-id
###################################################
p1 <- autoplot(xRle, stat = "identity")
p2 <- autoplot(xRle, stat = "identity", geom = "point", color = "red")
tracks('line' = p1, "point" = p2)


###################################################
### code chunk number 21: rle-slice
###################################################
p1 <- autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5)
p2 <- autoplot(xRle, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
tracks('bar' = p1, "heatmap" = p2)


###################################################
### code chunk number 22: rlel-simul
###################################################
xRleList <- RleList(xRle, 2L * xRle)
xRleList


###################################################
### code chunk number 23: rlel-bin
###################################################
p1 <- autoplot(xRleList)
p2 <- autoplot(xRleList, nbin = 80)
p3 <- autoplot(xRleList, geom = "heatmap", nbin = 200)
tracks('nbin = 30' = p1, "nbin = 80" = p2, "nbin = 200(heatmap)" = p3)


###################################################
### code chunk number 24: rlel-id
###################################################
p1 <- autoplot(xRleList, stat = "identity")
p2 <- autoplot(xRleList, stat = "identity", geom = "point", color = "red")
tracks('line' = p1, "point" = p2)


###################################################
### code chunk number 25: rlel-slice
###################################################
p1 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5)
p2 <- autoplot(xRleList, type = "viewMaxs", stat = "slice", lower = 5, geom = "heatmap")
tracks('bar' = p1, "heatmap" = p2)


###################################################
### code chunk number 26: txdb
###################################################
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene


###################################################
### code chunk number 27: txdb-visual
###################################################
p1 <- autoplot(txdb, which = genesymbol["ALDOA"], names.expr = "tx_name:::gene_id")
p2 <- autoplot(txdb, which = genesymbol["ALDOA"], stat = "reduce", color = "brown", 
               fill = "brown")
tracks(full = p1, reduce = p2, heights = c(5, 1)) + ylab("")


###################################################
### code chunk number 28: ga-load
###################################################
library(Rsamtools)
data("genesymbol", package = "biovizBase")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE)


###################################################
### code chunk number 29: ga-exp
###################################################
p1 <- autoplot(ga)
p2 <- autoplot(ga, geom = "rect")
p3 <- autoplot(ga, geom = "line", stat = "coverage")
tracks(default = p1, rect = p2, coverage = p3)


###################################################
### code chunk number 30: bf-load (eval = FALSE)
###################################################
## library(Rsamtools)
## bamfile <- "./wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
## bf <- BamFile(bamfile)


###################################################
### code chunk number 31: bf-est-cov (eval = FALSE)
###################################################
## autoplot(bamfile)
## autoplot(bamfile, which = c("chr1", "chr2"))
## autoplot(bf)
## autoplot(bf, which = c("chr1", "chr2"))
## 
## data(genesymbol, package = "biovizBase")
## autoplot(bamfile,  method = "raw", which = genesymbol["ALDOA"])
## 
## library(BSgenome.Hsapiens.UCSC.hg19)
## autoplot(bf, stat = "mismatch", which = genesymbol["ALDOA"], bsgenome = Hsapiens)


###################################################
### code chunk number 32: char-bam (eval = FALSE)
###################################################
## bamfile <- "./wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
## autoplot(bamfile)


###################################################
### code chunk number 33: char-gr
###################################################
library(rtracklayer)
test_path <- system.file("tests", package = "rtracklayer")
test_bed <- file.path(test_path, "test.bed")
autoplot(test_bed, aes(fill = name))


###################################################
### code chunk number 34: matrix-default
###################################################
autoplot(volcano)


###################################################
### code chunk number 35: matrix-default-scale
###################################################
autoplot(volcano-150)+scale_fill_fold_change()


###################################################
### code chunk number 36: matrix-default-gene
###################################################
colnames(volcano) <- sort(sample(1:300, size = ncol(volcano), replace = FALSE))
autoplot(volcano-150, genomic.pos = TRUE)+scale_fill_fold_change()


###################################################
### code chunk number 37: autoplot.Rnw:832-838
###################################################
library(Biobase)
data(sample.ExpressionSet)
sample.ExpressionSet
set.seed(1)
idx <- sample(seq_len(dim(sample.ExpressionSet)[1]), size = 50)
eset <- sample.ExpressionSet[idx,]


###################################################
### code chunk number 38: eset-default
###################################################
p1 <- autoplot(eset)
p1


###################################################
### code chunk number 39: eset-default-scale
###################################################
p2 <- p1 + scale_fill_fold_change()
p2


###################################################
### code chunk number 40: eset-default-pcp
###################################################
autoplot(eset, type = "pcp")


###################################################
### code chunk number 41: eset-default-boxplot
###################################################
autoplot(eset, type = "boxplot")


###################################################
### code chunk number 42: eset-default-sm
###################################################
autoplot(eset[, 1:7], type = "scatterplot.matrix")


###################################################
### code chunk number 43: eset-default-ms
###################################################
autoplot(eset, type = "mean-sd")


###################################################
### code chunk number 44: eset-default-volcano
###################################################
autoplot(eset, type = "volcano", fac = pData(sample.ExpressionSet)$type)


###################################################
### code chunk number 45: sset
###################################################
nrows <- 200; ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
rowData <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
                   IRanges(floor(runif(200, 1e5, 1e6)), width=100),
                   strand=sample(c("+", "-"), 200, TRUE))
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                     row.names=LETTERS[1:6])
sset <- SummarizedExperiment(assays=SimpleList(counts=counts),
                             rowData=rowData, colData=colData)






###################################################
### code chunk number 46: sset-heatmap
###################################################
autoplot(sset) + scale_fill_fold_change()


###################################################
### code chunk number 47: sset-pcp
###################################################
autoplot(sset, type = "pcp")


###################################################
### code chunk number 48: sset-boxplot
###################################################
autoplot(sset, type = "boxplot")


###################################################
### code chunk number 49: sset-sm
###################################################
autoplot(sset, type = "scatterplot.matrix")


###################################################
### code chunk number 50: vcf
###################################################
library(VariantAnnotation)
vcffile <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(vcffile, "hg19")
hdr <- exptData(vcf)[["header"]]



## autoplot(vcf, type = "fixed") + coord_cartesian(xlim = c(50310860, 50310890))




###################################################
### code chunk number 51: va-default
###################################################
autoplot(vcf)


###################################################
### code chunk number 52: va-info
###################################################
autoplot(vcf, type = "info", aes(y  = THETA))


###################################################
### code chunk number 53: va-fixed
###################################################
autoplot(vcf, type = "fixed")


###################################################
### code chunk number 54: va-fs
###################################################
np1 <- autoplot(vcf, type = "fixed") + xlim(50310860, 50310890) 
p2 <- autoplot(vcf, type = "fixed", full.string = TRUE) + xlim(50310860, 50310890)
tracks("full.string = FALSE" = p1, "full.string = TRUE" = p2)+
  scale_y_continuous(breaks = NULL, limits = c(0, 3))


###################################################
### code chunk number 55: va-rsw
###################################################
p3 <- autoplot(vcf, type = "fixed", ref.show = FALSE) + xlim(50310860, 50310890) +
    scale_y_continuous(breaks = NULL, limits = c(0, 2))
p3


###################################################
### code chunk number 56: bs-v
###################################################
library(BSgenome.Hsapiens.UCSC.hg19)
data(genesymbol, package = "biovizBase")
p1 <- autoplot(Hsapiens, which = resize(genesymbol["ALDOA"], width = 50))
p2 <- autoplot(Hsapiens, which = resize(genesymbol["ALDOA"], width = 50), geom = "rect")
tracks(text = p1, rect = p2)


###################################################
### code chunk number 57: sessionInfo
###################################################
sessionInfo()


