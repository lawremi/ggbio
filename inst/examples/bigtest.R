## TODO:
## 1. tracks margin fix
## 2. exons labels for plotRangesLinkedToData
library(ggbio)
library(GenomicRanges)

##  GRanges
set.seed(1)
N <- 1000

gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              group = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))


autoplot(gr)
autoplot(gr, geom = "rectangle")
autoplot(gr, geom = "segment")

autoplot(gr, geom = "segment", y = value)

## alignment is with chevron
autoplot(gr[1:50], geom = "alignment")
autoplot(gr[1:50], geom = "rectangle")
autoplot(gr[1:50], geom = "rectangle", group = pair) #works
autoplot(gr[1:50], geom = "alignment", group = pair, fill = pair) #werro
tracks(p1, p2)
gr[1:50]
## TODO: fix this
gr.test <- GRanges("chr1", IRanges(c(1, 9, 20, 50), width = 15), group = c("a", "a", "b","b"))
autoplot(gr.test, geom = "alignment", group = group)
autoplot(gr.test, geom = "rectangle", group = group)
autoplot(gr.test)

autoplot(gr[1:50], geom = "alignment", group = pair) #erro
autoplot(gr[1:50], geom = "alignment", group = pair) #error

autoplot(gr, geom = "line", y = value)
autoplot(gr, geom = "point", y = value)
autoplot(gr, geom = "line", stat = "coverage")
autoplot(gr, geom = "area", stat = "coverage")



gr.sub <- gr[seqnames(gr) == "chr1"] #or 
p1 <- autoplot(gr.sub, geom = "rectangle") + opts(title = "full")
p2 <- autoplot(gr.sub, geom = "point", y = value) + opts(title = "point")
p3 <- autoplot(gr.sub, geom = "line", y = value) + opts(title = "line")
p4 <- autoplot(gr.sub, geom = "line", stat = "coverage") + opts(title = "coverage.line")   
p5 <- autoplot(gr.sub, geom = "area", stat = "coverage") + opts(title = "coverage.area")   
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

autoplot(gr, ncol = 2)
## faceting, use facets not facet
autoplot(gr, facets = group ~ seqnames)
autoplot(gr, geom = "segment", facets = group ~ seqnames)
autoplot(gr, geom = "line", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "point", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "line", stat = "coverage", facets = group ~ seqnames)
autoplot(gr, geom = "area", stat = "coverage", facets = group ~ seqnames)

autoplot(gr[seqnames(gr) == "chr1"], facets = group ~ seqnames)

## facet gr
gr.region <- GRanges(c("chr1", "chr2", "chr3"), 
                     IRanges(c(100, 200, 250), 
                             width = 70))
## facet_grid
autoplot(gr, facets = gr.region)
## facet_wrap
autoplot(gr, facets = gr.region, nrow = 2) + 
  scale_y_continuous(limits = c(0, 90))

## TO BE FIXED
## checvron
gr2 <- GRanges("chr1", IRanges(c(100, 200, 300), width = 50))
p <- qplot(gr2)
gr.gaps <- gaps(gr2)[-1]
values(gr.gaps)$score <- c(1, 100)
gr.gaps
p1 <- p + geom_chevron(gr.gaps)
p2 <- p + geom_chevron(gr.gaps, aes(size = score), offset = "score",
                 chevron.height = c(0.1, 0.2))
p3 <- p + geom_chevron(gr.gaps, offset = -0.1)
tracks(p1, p2, p3)

## arch
df <- data.frame(x = seq(1, 100, by = 2),
                 y = 1:50,
                 xend = seq(1, 100, by = 2) + rnorm(50, 10),
                 yend =0 , size = rnorm(50))

ggplot() + geom_arch(data = df, aes(x = x, xend = xend, height = size, y = y, size = size))
ggplot() + geom_arch(data = df, aes(x = x, xend = xend, height = size, size = size))
ggplot() + geom_arch(data = df, aes(x = x, xend = xend, size = size))


library(GenomicRanges)
gr <- GRanges("chr1", IRanges(start = seq(1, 100, by = 2),
                        width = 20), size = rnorm(50))

ggplot() + geom_arch(data = gr, aes(height = size))
ggplot() + geom_arch(data = gr, aes(size = size))
ggplot() + geom_arch(data = gr, aes(height = size), rect.height = 5)



gr1 <- GRanges("chr1", IRanges(c(1, 10), width = 5), value = 1:2)
gr2 <- GRanges("chr1", IRanges(c(30, 40), width = 5), value = 3:4)
grl <- GRangesList(gr1, gr2)
values(grl)$size <- c(5, 10)

ggplot() + geom_arch(data = grl, aes(size = size), rect.height = 5)
ggplot() + geom_arch(data = grl, aes(height = value, size = size, color = value), rect.height = 5)

## GRangesList
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
exons.tx <- exonsBy(txdb, by = "tx")
exons.rbm17 <- subsetByOverlaps(exons.tx, genesymbol["RBM17"])
values(exons.rbm17)$freqs <- c(100, 200, 300)
p.splice <- autoplot(exons.rbm17, fill = freqs, ylab = "")
print(p.splice)

## Tengfei is up to here
## IRanges not check yet
ir <- ranges(gr[seqnames(gr) == "chr1"])[1:40]
Df <- values(gr[seqnames(gr) == "chr1"][1:40])
values(ir) <- Df
values(ir)
p1 <- autoplot(ir) + opts(title = "full")
p2 <- autoplot(ir, geom = "segment")+ opts(title = "segment")
p3 <- autoplot(ir, geom = "coverage.line")+ opts(title = "coverage.line")
p4 <- autoplot(ir, geom = "coverage.area")+ opts(title = "coverage.area")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

## Rle
library(IRanges)
library(ggbio)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)


autoplot(xRle)
autoplot(xRle, geom = "line")
autoplot(xRle, geom = "segment")
autoplot(xRle, type = "viewMaxs", lower = 5)
autoplot(xRle, type = "viewMins", lower = 5)
autoplot(xRle, type = "viewMeans", lower = 5)
autoplot(xRle, type = "viewMeans", lower = 5, color = I("red"))
autoplot(xRle, type = "viewSums", lower = 5)
autoplot(xRle, type = "viewMaxs", lower = 5, geom = "line")
autoplot(xRle, type = "viewMaxs", lower = 5, geom = "segment")



autoplot(xRleList)
autoplot(xRleList, geom = "segment")
autoplot(xRleList, geom = "line")
autoplot(xRleList, type = "viewMaxs", lower = 5)
autoplot(xRleList, type = "viewMaxs", lower = 5, geom = "line")
autoplot(xRleList, type = "viewSums", lower = 5, geom = "segment",
      facetByRow = FALSE, color = I("red"), size = I(5))
autoplot(xRle, size = y)
autoplot(xRle, type = "viewSums", lower = 5)

autoplot(xRle, type = "viewSums", lower = 5, size = I(10), color = I("red"),
      alpha = y)

## GappedAlignments
library(Rsamtools)
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE) 
p1 <- autoplot(ga)
p2 <- autoplot(ga, show.junction = TRUE)
p3 <- autoplot(ga, geom = "full")
grid.arrange(p1, p2, p3, ncol = 1)


## bamfile
library(BSgenome.Hsapiens.UCSC.hg19)
library(ggbio)
data(genesymbol)
## autoplot(bamfile, which = genesymbol["RBM17"]) # fail
p1 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "gapped.pair")
p2 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "gapped.pair",
            show.junction = TRUE) 
p3 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "full")
p4 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "coverage.line") 
p5 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "coverage.area")
p6 <- autoplot(bamfile, which = genesymbol["RBM17"], bsgenome = Hsapiens,
      geom = "mismatch.summary") 
tracks(p1, p2, p3, p4, p5, p6)

## use plotMismatchSum directly
pmis1 <- plotMismatchSum(test.match2, show.coverage = FALSE)
pmis2 <- plotMismatchSum(test.match, show.coverage = TRUE)


## TranscirptDb(Done)
## remove
p.full <- autoplot(txdb, geom = "gene", which = genesymbol["RBM17"])
p.dense <- autoplot(txdb, geom = "reduced_gene", which = genesymbol["RBM17"])
tracks(p.full, p.dense, theme = theme_bw(), heights = c(2, 1))


## BSgenome for reference genome
gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
p1 <- autoplot(Hsapiens, which = gr, geom = "text") +
  theme_bw()
p2 <- autoplot(Hsapiens, which = gr, geom = "point") +
    theme_bw()
p3 <- autoplot(Hsapiens, which = gr, geom = "segment") +
    theme_bw()
p4 <- autoplot(Hsapiens, which = gr, geom = "rectangle") +
    theme_bw()
tracks(p1, p2, p3, p4)


## stacked
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
p <- plotStackedOverview(new.ideo, cytoband = TRUE)
print(p)
## fixing order
new.ideo <- sort(new.ideo)
p <- plotStackedOverview(new.ideo, cytoband = FALSE)
print(p)

data(darned_hg19_subset500)
## rename 
old.chrs <- seqnames(seqinfo(darned_hg19_subset500))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs

new.darned <- renameSeqlevels(darned_hg19_subset500, new.chrs)
values(new.darned)$value <- rnorm(length(new.darned))
p + layout_stacked(new.darned)
p + layout_stacked(new.darned, aes(y = value), geom = "area")
p + layout_stacked(new.darned, aes(y = value), geom = "line")

p <- plotStackedOverview(new.ideo, cytoband = FALSE)
p <- p + layout_stacked(new.darned, aes(color = exReg))
print(p)

p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1")
print(p)

p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1",
                zoom.region = c(1e8, 1.5e8))
print(p)
## ========================================
## Grand Linear
## ========================================
library(ggbio)
data(hg19Ideogram, package = "biovizBase")
data(hg19IdeogramCyto, package = "biovizBase")
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))

chrs
seqlths <- seqlengths(hg19Ideogram)[chrs]
set.seed(1)
nchr <- length(chrs)
nsnps <- 1000

gr.snp <- GRanges(rep(chrs,each=nsnps),
             IRanges(start = do.call(c, lapply(chrs, function(chr){
               N <- seqlths[chr]
               runif(nsnps,1,N)
             })), width = 1),
             SNP=sapply(1:(nchr*nsnps), function(x) paste("rs",x,sep='')),
             pvalue =  -log10(runif(nchr*nsnps)),
                  group = sample(c("Normal", "Tumor"), size = nchr*nsnps,
                    replace = TRUE)
             )

## seqlengths(gr.snp) <- rep(1e9, 24)      #
nms <- paste("chr", c(1:22, "X","Y"), sep = "")
gr.snp <- keepSeqlevels(gr.snp, nms)
## default is two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point")
plotGrandLinear(gr.snp, y = pvalue, geom = "point", color.type = "identity",
                color = I("black"), size = I(1))
## remove ticks and laebels
plotGrandLinear(gr.snp, y = pvalue, geom = "point") +
            opts(axis.text.x = theme_blank(),
                 axis.ticks=theme_blank())


## how to change labels and rotate it 
ticks <- biovizBase:::transformGRangesToDfWithTicks(gr.snp)$ticks
names(ticks) <- paste("chr", names(ticks), sep = "")
plotGrandLinear(gr.snp, y = pvalue, geom = "point") +
  scale_x_continuous(breaks = ticks, labels = names(ticks))+
   opts(axis.text.x=theme_text(angle=-90, hjust=0)) 

## facet by samples, comparison across groups
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facets = group ~ .,  color.type = "twocolor")
library(biovizBase)
library(ggplot2)
## change two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facets = group ~ .,  color.type = "twocolor",
                two.color = c("red", "blue"))
## geom line
plotGrandLinear(gr.snp, y = pvalue,
                geom = "line", facet = group ~ .)  




## add size and change color
plotGrandLinear(gr.snp, y = pvalue, size = pvalue,geom = "point",
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, size = I(0.05),
                geom = "point", facet = group ~ .)

plotGrandLinear(gr.snp, y = pvalue, color = group, geom = "point",
                facet = group ~ .,
                color.type = "identity")

plotGrandLinear(gr.snp, y = pvalue, color = I("blue"), geom = "point", facet = group ~ .,
                color.type = "identity")


## facet by seqnames, slower
plotGrandLinear(gr.snp, y = pvalue,geom = "point", 
                facet = group ~ seqnames, scales = "free", space = "free")


## splicesum
p1 <- plotSpliceSum(bamfile, exons.rbm17)
p2 <- plotSpliceSum(bamfile, exons.rbm17, weighted = FALSE, offset = 0.01)
p3 <- plotSpliceSum(bamfile, txdb, which = genesymbol["RBM17"],
                   show.label = TRUE,
                   label.type = "count")

## mismatch
gr <- GRanges("chr10", IRanges(6134000, 6135000))
test <- pileupAsGRanges(bamfile, region = gr)
test.match <- pileupGRangesAsVariantTable(test, Hsapiens)
## use plotMismatchSum directly
pmis1 <- plotMismatchSum(test.match, show.coverage = FALSE)
pmis2 <- plotMismatchSum(test.match, show.coverage = TRUE)
grid.arrange(pmis1, pmis2, ncol = 1)
## we can also use generic autoplot function
## use autoplot generic function
p <- autoplot(test.match, geom = "mismatch.summary")
library(Rsamtools)
## for character
p <- autoplot(bamfile, which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = TRUE)
## for BamFile
p <- autoplot(BamFile(bamfile), which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = TRUE)

## plot linked data
model <- exonsBy(txdb, by = "tx")
exons <- exons(txdb)
exon17 <- subsetByOverlaps(exons, genesymbol["RBM17"])
model17 <- subsetByOverlaps(model, genesymbol["RBM17"])
## reduce to make sure there is no overlap
## just for example
exon.new <- reduce(exon17)
## simulated data
values(exon.new)$sample1 <- rnorm(length(exon.new), 10, 3)
values(exon.new)$sample2 <- rnorm(length(exon.new), 10, 10)
values(exon.new)$score <- rnorm(length(exon.new))
p <- autoplot(exons.rbm17)
plotRangesLinkedToData(exon.new, stat.col = c("sample1", "sample2"))
library(biovizBase)
plotRangesLinkedToData(exon.new, stat.col = 1:2)
plotRangesLinkedToData(exon.new, stat.col = 1:2, annotation = list(p))
theme_bw

p <- qplot(data = mtcars, x = mpg, y = cyl)
tracks(p + theme_grey(), p + theme_bw())
## fragment Length
library(ggbio)
data(genesymbol)
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
## model <- exonsBy(txdb, by = "tx")
## model.new <- subsetByOverlaps(model, genesymbol["RBM17"])
exons.rbm17 <- subsetByOverlaps(exons(txdb), genesymbol["RBM17"])
exons.new <- reduce(exons.rbm17)
library(gridExtra)
plotFragLength(bamfile, exons.new, geom = "line")
plotFragLength(bamfile, exons.new, geom = c("point","segment"))
plotFragLength(bamfile, exons.new, geom = c("point","segment"), annotation = FALSE)
plotFragLength(bamfile, exons.new, geom = c("point","segment"), type = "cut",
               gap.ratio = 0.001)

length(GRangesList(exons.new, exons.new))
library(ggbio)
data(genesymbol)
## M's question, combine a coverage plot with fragment length
p <- plotFragLength(bamfile, exons.new, geom = c("point","segment"),
                    annotation = FALSE)

bamfile <- "~/Datas/seqs/rna-seq/genentech/SRC587239.101201_SN152_0410_B_FC810WK_IX_PE.003.s_5.gsnap.merged.concordant_uniq.bam"
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- Hsapiens_UCSC_hg19_knownGene_TxDb
exons.rbm17 <- subsetByOverlaps(exons(txdb), genesymbol["RBM17"])
exons.new <- reduce(exons.rbm17)
## maybe it's not a best way to get coverage, just example
## to get a coverage plot
library(Rsamtools)
res <- readBamGappedAlignments(bamfile,
                               param =
                               ScanBamParam(which = genesymbol["RBM17"], 
                                 flag = scanBamFlag(isProperPair = TRUE,
                                   isFirstMateRead = TRUE)))
gr <- as(res, "GRanges")
## use coverage geom, need to set theme_bw() to make sure they aligned well
p.cov <- autoplot(gr, geom = "coverage.line") + theme_bw()
p.frag <- plotFragLength(bamfile, exons.new, geom = c("point","segment"),
                         annotation = FALSE)
## p.model <- autoplot(exons.new) should support show.gaps/chevron
## want to show chevron...need a GRangesList to represent model

## need to fix/update this..
grl <- GRangesList(exons.new)
names(grl) <- "rbm17" # this is tedious
p.model <- autoplot(grl)
p.model
## annotation = FALSE, return a ggplot object, but you need to embed you
## model track yourself
## this need some conventient way I guess
p.frag <- plotFragLength(bamfile, exons.new, geom = c("point","segment"),
                         annotation = FALSE)
exons.new
p.frag + scale_x_continuous(limits = c(61))
tracks(p.frag, p.model, heights = c(400, 400, 100))
## compare grid.arrange
library(gridExtra)
grid.arrange(p.frag, p.cov, p.model, heights = c(400, 400, 300))



## for jennifer
## for now, store whatever information into GRangesList
## suppose we have exons
## [1]---[2]---[3]---[4]
## We have splicing [1]--[2]
## [2]--[3],..., we put whatever we found as GRangesList, each entry
## contains one splicing form
library(GenomicRanges)
gr <- GRanges("chr1", IRanges(c(1, 20, 50, 60),width = c(5, 10, 6, 7)))
## values(gr) <- data.frame(pvalue = rnorm(6))
combs <- combn(1:4,2)

grl <- do.call("GRangesList", apply(combs, 2, function(x){
  res <- gr[x]
  values(res) <- data.frame(pvalue = runif(2))
  res
}))

values(grl) <- data.frame(counts = sample(1:100, size = 6),
                                   score  = rnorm(6))
## so you could work on this grl, which contains 6 conbination of 4 exons
## use values to get the counts for each case
grl
## get first combination
grl[[1]]  # '[[' return a GRanges
grl[1] #'[' still a list 
values(grl[1])$counts # counts for first  combination
## notice the elementMetadata( or values) is not the same for list and element
values(grl)
class(grl[1])
values(grl[1])
start(grl)
end(grl)
width(grl)
## this is for GRanges inside the list
class(grl[[1]])
values(grl[[1]])
start(grl[[1]])
end(grl[[1]])
width(grl[[1]])

##  circular
## it's dangerous that some track with seqlengths, while others doesn't
library(GenomicRanges)
library(ggbio)

gr <- GRanges(rep(c("chr1", "chr2", "chr3"), each = 200),
              IRanges(c(sample(size = 200, 1:200, replace = TRUE),
                        sample(size = 200, 1:500, replace = TRUE),
                        sample(size = 200, 1:1000, replace = TRUE)), width = 1))
seqlengths(gr) <- c(200, 500, 1000)
values(gr)$size <- rnorm(200 * 3)

gr3 <- GRanges(rep(c("chr1", "chr2", "chr3"), each = 200),
              IRanges(c(sample(size = 200, 1:200, replace = TRUE),
                        sample(size = 200, 1:500, replace = TRUE),
                        sample(size = 200, 1:1000, replace = TRUE)), width = 10))

seqlengths(gr3) <- c(210, 510, 1010)
values(gr3)$size <- rnorm(200 * 3)

data(hg19Ideogram, package = "biovizBase")

library(scales)
## make a link
gr1 <- GRanges(rep(c("chr1", "chr2", "chr3"), each = 100),
               IRanges(c(sample(size = 100, 1:200, replace = TRUE),
                        sample(size = 100, 1:500, replace = TRUE),
                        sample(size = 100, 1:1000, replace = TRUE)), width = 1))
values(gr1)$size <- rnorm(100 * 3)
seqlengths(gr1) <- c(200, 500, 1000)
gr2 <- GRanges(rep(c("chr1", "chr2", "chr3"), each = 100),
               IRanges(c(sample(size = 100, 1:200, replace = TRUE),
                        sample(size = 100, 1:500, replace = TRUE),
                        sample(size = 100, 1:1000, replace = TRUE)), width = 1))
gr2 <- gr2[sample(1:length(gr2), size = 300)]
values(gr1)$to.gr <- gr2

library(Hmisc)
library(ggplot2)
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames))+ theme_null2()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line")+ theme_null2()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line",
                         direction = "anticlockwise")+ theme_null2()


ggplot() + layout_circle(gr3, geom = "segment")+ theme_null2()
ggplot() + layout_circle(gr3, geom = "rectangle", aes(fill = "black"))+ theme_null2()
ggplot() + layout_circle(gr3, aes(y = "size", color = seqnames), geom = "segment")+ theme_null2()
ggplot() + layout_circle(gr1, radius = 10, linked.to = "to.gr",geom = "link")+ theme_null2()

ggplot() + layout_circle(gr1, aes(color = size),radius = 10, linked.to = "to.gr",geom = "link")+
  theme_null2()

ggplot() + layout_circle(gr1, aes(color = size, alpha = size),radius = 10, linked.to = "to.gr",geom = "link")+
  theme_null2()


  layout_circle(gr, radius = 20, aes(y = "size", color = seqnames), geom = "line")

xy <- bezier(x = c(0, 5, 10), y = c(0, 0, 10))
plot(xy$x, xy$y)
library(ggplot2)
head(mtcars)
autoplot(data = mtcars, x = mpg, y = cyl, alpha = qsec)

## test figures from Michael
## example(GRanges)
## autoplot(gr)
library(ggbio)
data(cstest, package = "chipseq")
library(GenomicRanges)
## library("org.Mm.eg.db")
## orgs <- org.Mm.eg.db
## syms <- genSymbols(orgs)
## syms
## from the plot you sent me I got a region overlapped with your plot, it's gene Rrp1
grs <- GRanges("chr10",IRanges(start = 77863000, end = 77878000))
## sym.cur <- subsetByOverlaps(syms, grs)
## sym.cur
library("TxDb.Mmusculus.UCSC.mm9.knownGene")
txdb <- TxDb.Mmusculus.UCSC.mm9.knownGene
test <- exonsBy(txdb, by = "tx")
## grl.Rrp1 <- subsetByOverlaps(test, sym.cur)
## grs
cstest.sub <- endoapply(cstest, function(x){
  subsetByOverlaps(x, grs)
})
cstest.sub <- keepSeqlevels(cstest.sub, "chr10")
gr <- unlist(cstest.sub)
gr <- resize(gr, width = 300, fix = "start")
values(gr)$group <- rep(names(cstest.sub), times = elementLengths(cstest.sub))
library(ggbio)
p1 <- autoplot(gr, facets = group ~ seqnames, stat = "coverage", geom = "line")
p2 <- autoplot(gr, facets = group ~ seqnames, stat = "coverage", geom = "area", fill = group)
tracks(p1, p2)
## autoplot(gr, facets = group ~ seqnames, stat = "coverage", geom = "line",
##          xlim = c(77873000, 77878000))
## autoplot(gr, facets = group ~ seqnames, stat = "coverage", geom = "area")

p4 <- autoplot(txdb, geom = "gene", which = grs)
p5 <- autoplot(txdb, geom = "reduced_gene", which = grs, ylab = "")
p6 <- autoplot(txdb, geom = "gene", which = GRanges("chr10", IRanges(77874000, 77878000)))
tracks(p4, p6)
tracks(p1, p5, heights = c(2, 1))
tracks(p2, p5, heights = c(2, 1))
png("~/Desktop/ctest3.png", 800, 800)
tracks(p1 + theme_bw(), p5 + theme_alignment(), heights = c(3, 1))
dev.off()
png("~/Desktop/ctest4.png", 800, 800)
tracks(p2, p5, heights = c(3, 1),xlim = c(77870000, 77878000))
dev.off()
p5
p5 + scale_x_continuous(limits = c(77870000, 77878000))
p5 + coord_cartesian() + scale_x_continuous(limits = c(77870000, 77878000))
p5 + xlim(77870000, 77878000)
p5 + coord_cartesian(xlim=c(77870000, 77878000), wise = TRUE)
png("~/Desktop/ctest4-zoom.png", 800, 800)
tracks(p1 + theme_bw(), p5 + theme_alignment(), heights = c(3,1), xlim = c(77870000, 77878000))
dev.off()

png("~/Desktop/ctest6-zoom.png", 800, 800)
tracks(p2, p5, heights = c(3,1), xlim = c(77870000, 77878000))
dev.off()

tracks(p4, p5, theme = theme_bw(), heights = c(2, 1))

p1 <- qplot(data = mtcars, x = mpg, y = cyl)
p2 <- qplot(data = mtcars, x = mpg, y = cyl, geom = "point")
tracks(p1, p2)
ggbio:::align.plots(p1, p2)

x = 1:10000
y = rnorm(10000)
df <- data.frame(x = x, y = y)
qplot(data = df,x = x, y = y, geom = "histogram")


"hello" * 4
library(ggplot2)
x = 1:1e5
y = rnorm(length(x))
qplot(x = x, y = y, geom = "area")
qplot(x = x, y = y, geom = "polygon")
## polygon is fast


## Michael's code
library(chipseq)
data(cstest)
genome(cstest) <- "mm9" # should already be set on the cstest dataset
allFragments <- resize(stack(cstest), 120)
library(ggbio)
roiGR <- GRanges("chr10", IRanges(77875000, 77876500))
p1 <- autoplot(subsetByOverlaps(allFragments, roiGR), 
               facets = sample ~ seqnames,
               stat = "coverage", geom = "area")
p1
library(TxDb.Mmusculus.UCSC.mm9.knownGene)
## names.expr accept expression now
p2 <- autoplot(TxDb.Mmusculus.UCSC.mm9.knownGene, ylab = "",
               which = roiGR, names.expr = expression(paste(gene_id,"::",tx_id,sep = "")))
p2
## default name, tx_name(gene_id), and use range of which
p2 <- autoplot(TxDb.Mmusculus.UCSC.mm9.knownGene, ylab = "",
               which = roiGR)
p2
## NOTE: use widest track as limits if xlim is not specified
tracks(p1, p2, heights = c(2, 1))
## now xlim accept IRanges, GRanges, numeric
tracks(p1, p2, heights = c(2, 1), xlim = roiGR)
tracks(p1, p2, heights = c(2, 1), xlim = ranges(roiGR))
tracks(p1, p2, heights = c(2, 1), xlim = c(start(roiGR), end(roiGR)))

## xlim overide which
p4 <- autoplot(TxDb.Mmusculus.UCSC.mm9.knownGene, ylab = "",
               which = roiGR, xlim = c(start(roiGR)-1000, end(roiGR)))
p4
tracks(p1, p2, heights = c(2, 1))
## NOTE: theme_alignment cannot remove only y ticks, you could add following code
p2 + theme_alignment() + scale_y_continuous(breaks = NA)

