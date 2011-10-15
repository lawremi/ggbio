library(ggbio)

## overide qplot
qplot(data = mtcars, mpg, cyl)
qplot(1:3)
qplot(volcano)
qplot(c(1, 2.2, 3.3))
ggplot2::qplot(1:3)
ggplot2::qplot(c(1, 2.2, 3.3))
ggplot2::qplot(volcano)


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

qplot(gr)
qplot(gr, geom = "full")
qplot(gr, geom = "segment")
qplot(gr, geom = "line", y = value)
qplot(gr, geom = "point", y = value)
qplot(gr, geom = "coverage.line")
qplot(gr, geom = "coverage.polygon")

gr.sub <- gr[seqnames(gr) == "chr1"] #or 
p1 <- qplot(gr.sub, geom = "full") + opts(title = "full")
p2 <- qplot(gr.sub, geom = "point", y = value) + opts(title = "point")
p3 <- qplot(gr.sub, geom = "line", y = value) + opts(title = "line")
p4 <- qplot(gr.sub, geom = "coverage.line") + opts(title = "coverage.line")   
p5 <- qplot(gr.sub, geom = "coverage.polygon") + opts(title = "coverage.polygon")   
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

qplot(gr, ncol = 2)
## faceting, use facets not facet
qplot(gr, facets = group ~ seqnames)
qplot(gr, geom = "segment", facets = group ~ seqnames)
qplot(gr, geom = "line", y = value, facets = group ~ seqnames)
qplot(gr, geom = "point", y = value, facets = group ~ seqnames)
qplot(gr, geom = "coverage.line", facets = group ~ seqnames)
qplot(gr, geom = "coverage.polygon", facets = group ~ seqnames)

## facet gr
gr.region <- GRanges(c("chr1", "chr2", "chr3"), 
                     IRanges(c(100, 200, 250), 
                             width = 70))
## facet_grid
qplot(gr, facet_gr = gr.region)
## facet_wrap
qplot(gr, facet_gr = gr.region, nrow = 2) + 
  scale_y_continuous(limits = c(0, 90))


## checvron
gr <- GRanges("chr1", IRanges(c(100, 200, 300), width = 50))
p <- qplot(gr)
gr.gaps <- gaps(gr)[-1]
values(gr.gaps)$score <- c(1, 100)
p1 <- p + geom_chevron(gr.gaps)
p2 <- p + geom_chevron(gr.gaps, aes(size = score), offset = "score",
                 chevron.height = c(0.1, 0.2))
p3 <- p + geom_chevron(gr.gaps, offset = -0.1)
tracks(p1, p2, p3)

## GRangesList
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol)
library(ggbio)
txdb <- Hsapiens_UCSC_hg19_knownGene_TxDb
exons.tx <- exonsBy(txdb, by = "tx")
exons.rbm17 <- subsetByOverlaps(exons.tx, genesymbol["RBM17"])
nms <- names(exons.rbm17)
freqs <- c(100, 200, 300)
names(freqs) <- nms
p.splice1 <- qplot(exons.rbm17)
## when turning on frequency 
p.splice <- qplot(exons.rbm17, freq = freqs, show.label = TRUE, label.type = "count",
      scale.size = c(1, 5), label.size = 3)
p.splice2 <- qplot(exons.rbm17, freq = freqs, show.label = TRUE, offset = 0.05,
                   label.type = "count")
print(p.splice1)
print(p.splice2)

ir <- IRanges(c(10, 20, 30) ,width  = 15)
qplot(ir)
ir <- ranges(gr[seqnames(gr) == "chr1"])[1:40]
p1 <- qplot(ir) + opts(title = "full")
p2 <- qplot(ir, geom = "segment")+ opts(title = "segment")
p3 <- qplot(ir, geom = "coverage.line")+ opts(title = "coverage.line")
p4 <- qplot(ir, geom = "coverage.polygon")+ opts(title = "coverage.polygon")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

library(IRanges)
set.seed(1)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length = 500), 
            seq(10, 0.001, length = 500))
xVector <- rpois(1e4, lambda)
xRle <- Rle(xVector)
xRleList <- RleList(xRle, 2L * xRle)


qplot(xRle)
qplot(xRle, geom = "line")
qplot(xRle, geom = "segment")
qplot(xRle, type = "viewMaxs", lower = 5)
qplot(xRle, type = "viewMins", lower = 5)
qplot(xRle, type = "viewMeans", lower = 5)
qplot(xRle, type = "viewSums", lower = 5)

qplot(xRleList)
qplot(xRleList, geom = "segment")
qplot(xRleList, geom = "line")
qplot(xRleList, type = "viewMaxs", lower = 5)
qplot(xRleList, type = "viewMaxs", lower = 5, geom = "line")
qplot(xRleList, type = "viewSums", lower = 5, geom = "segment",
      facetByRow = FALSE, color = I("red"), size = I(5))
qplot(xRle, size = y)
qplot(xRle, type = "viewSums", lower = 5)

qplot(xRle, type = "viewSums", lower = 5, size = I(10), color = I("red"),
      alpha = y)

## GappedAlignments
library(Rsamtools)
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
## need to set use.names = TRUE
ga <- readBamGappedAlignments(bamfile,
                              param = ScanBamParam(which = genesymbol["RBM17"]),
                              use.names = TRUE) 
p1 <- qplot(ga)
p2 <- qplot(ga, show.junction = TRUE)
p3 <- qplot(ga, geom = "full")
grid.arrange(p1, p2, p3, ncol = 1)


## bamfile
library(BSgenome.Hsapiens.UCSC.hg19)
library(ggbio)
data(genesymbol)
## qplot(bamfile, which = genesymbol["RBM17"]) # fail
p1 <- qplot(bamfile, which = genesymbol["RBM17"], geom = "gapped.pair")
p2 <- qplot(bamfile, which = genesymbol["RBM17"], geom = "gapped.pair",
            show.junction = TRUE) 
p3 <- qplot(bamfile, which = genesymbol["RBM17"], geom = "full")
p4 <- qplot(bamfile, which = genesymbol["RBM17"], geom = "coverage.line") 
p5 <- qplot(bamfile, which = genesymbol["RBM17"], geom = "coverage.polygon")
p6 <- qplot(bamfile, which = genesymbol["RBM17"], bsgenome = Hsapiens,
      geom = "mismatch.summary") 
tracks(p1, p2, p3, p4, p5, p6)

## use plotMismatchSum directly
pmis1 <- plotMismatchSum(test.match2, show.coverage = FALSE)
pmis2 <- plotMismatchSum(test.match, show.coverage = TRUE)


## TranscirptDb
p.full <- qplot(txdb, geom = "full", which = genesymbol["RBM17"])
p.single <- qplot(txdb, geom = "single", which = genesymbol["RBM17"])
p.tx <- qplot(txdb, geom = "tx", which = genesymbol["RBM17"])

tracks(p.full, p.single, p.tx, heights = c(400, 100, 400))


## BSgenome
gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
p1 <- qplot(Hsapiens, name = gr, geom = "text") +
  theme_bw()
p2 <- qplot(Hsapiens, name = gr, geom = "point") +
    theme_bw()
p3 <- qplot(Hsapiens, name = gr, geom = "segment") +
    theme_bw()
p4 <- qplot(Hsapiens, name = gr, geom = "rectangle") +
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
## we can also use generic qplot function
## use qplot generic function
p <- qplot(test.match, geom = "mismatch.summary")
library(Rsamtools)
## for character
p <- qplot(bamfile, which = gr, bsgenome = Hsapiens,
      geom = "mismatch.summary", show.coverage = TRUE)
## for BamFile
p <- qplot(BamFile(bamfile), which = gr, bsgenome = Hsapiens,
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
p <- qplot(exons.rbm17)
plotRangesLinkedToData(exon.new, stat.col = c("sample1", "sample2"))

plotRangesLinkedToData(exon.new, stat.col = 1:2)
plotRangesLinkedToData(exon.new, stat.col = 1:2, annotation = list(p))

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
p.cov <- qplot(gr, geom = "coverage.line") + theme_bw()
p.frag <- plotFragLength(bamfile, exons.new, geom = c("point","segment"),
                         annotation = FALSE)
## p.model <- qplot(exons.new) should support show.gaps/chevron
## want to show chevron...need a GRangesList to represent model

## need to fix/update this..
grl <- GRangesList(exons.new)
names(grl) <- "rbm17" # this is tedious
p.model <- qplot(grl)
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
