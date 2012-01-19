## TODO:
## 1. tracks margin fix
## 2. exons labels for plotRangesLinkedToData
library(ggbio)

##  GRanges
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
autoplot(gr[1:50], geom = "alignment", group = pair) #error
autoplot(gr, geom = "line", y = value)
autoplot(gr, geom = "point", y = value)
autoplot(gr, geom = "line", stat = "coverage")
autoplot(gr, geom = "polygon", stat = "coverage")



gr.sub <- gr[seqnames(gr) == "chr1"] #or 
p1 <- autoplot(gr.sub, geom = "rectangle") + opts(title = "full")
p2 <- autoplot(gr.sub, geom = "point", y = value) + opts(title = "point")
p3 <- autoplot(gr.sub, geom = "line", y = value) + opts(title = "line")
p4 <- autoplot(gr.sub, geom = "line", stat = "coverage") + opts(title = "coverage.line")   
p5 <- autoplot(gr.sub, geom = "polygon", stat = "coverage") + opts(title = "coverage.polygon")   
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

autoplot(gr, ncol = 2)
## faceting, use facets not facet
autoplot(gr, facets = group ~ seqnames)
autoplot(gr, geom = "segment", facets = group ~ seqnames)
autoplot(gr, geom = "line", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "point", y = value, facets = group ~ seqnames)
autoplot(gr, geom = "line", stat = "coverage", facets = group ~ seqnames)
autoplot(gr, geom = "polygon", stat = "coverage", facets = group ~ seqnames)

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
p4 <- autoplot(ir, geom = "coverage.polygon")+ opts(title = "coverage.polygon")
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
p5 <- autoplot(bamfile, which = genesymbol["RBM17"], geom = "coverage.polygon")
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
data(hg19IdeogramCyto)
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
p <- plotOverview(new.ideo, cytoband = TRUE)
print(p)

p <- plotOverview(new.ideo, cytoband = FALSE)
print(p)

data(darned_hg19_subset500)
## rename 
old.chrs <- seqnames(seqinfo(darned_hg19_subset500))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.darned <- renameSeqlevels(darned_hg19_subset500, new.chrs)
p <- p + geom_hotregion(new.darned)
print(p)


p <- plotOverview(new.ideo, cytoband = FALSE)
p <- p + geom_hotregion(new.darned, aes(color = exReg))
print(p)

p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1")
print(p)

p <- plotSingleChrom(hg19IdeogramCyto, subchr = "chr1",
                zoom.region = c(1e8, 1.5e8))
print(p)

## Grand Linear
data(hg19Ideogram)
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
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

## processing the name, to make it shorter
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
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
                facet = group ~ .,  color.type = "twocolor")

## change two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facet = group ~ .,  color.type = "twocolor",
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
library(ggplot2)
library(Hmisc)
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames))+ blank_opts()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line")+ blank_opts()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line",
                         direction = "anticlockwise")+ blank_opts()

ggplot() + layout_circle(gr1, radius = 10, linked.to = "to.gr",geom = "link")+ blank_opts()

ggplot() + layout_circle(gr1, aes(color = size),radius = 10, linked.to = "to.gr",geom = "link")+ blank_opts()

  layout_circle(gr, radius = 20, aes(y = "size", color = seqnames), geom = "line")

xy <- bezier(x = c(0, 5, 10), y = c(0, 0, 10))
plot(xy$x, xy$y)
library(ggplot2)
head(mtcars)
autoplot(data = mtcars, x = mpg, y = cyl, alpha = qsec)

## test figures from Michael
library(ggbio)
data(cstest, package = "chipseq")
grs <- GRanges("chr10",IRanges(start = 77873000, end = 77878000))
library("TxDb.Mmusculus.UCSC.mm9.knownGene")
txdb <- TxDb.Mmusculus.UCSC.mm9.knownGene
test <- exonsBy(txdb, by = "tx")
test2 <- subsetByOverlaps(test, grs)
grs2 <- range(unlist(test2))
grs3 <- resize(grs2, width = width(grs2) + 3000, fix = "end")
## cstest.sub <- subsetByOverlaps(cstest,grs2)
cstest.sub <- endoapply(cstest, function(x){
  subsetByOverlaps(x, grs3)
})
cstest.sub <- keepSeqlevels(cstest.sub, "chr10")
gr <- unlist(cstest.sub)
values(gr)$group <- rep(names(cstest.sub), times = elementLengths(cstest.sub))
gr <- resize(gr, width = 300, fix = "start")
## p1 <- autoplot(gr, facets = group ~ seqnames, stat = "coverage", geom = "line")
## p2 <- autoplot(test2)
## p2 is equal to p3
p4 <- autoplot(txdb, geom = "gene", which = grs3)
p5 <- autoplot(txdb, geom = "reduced_gene", which = grs3)
tracks(p4, p5, theme = theme_bw(), heights = c(2, 1))


