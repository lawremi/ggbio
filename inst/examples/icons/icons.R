library(ggbio)
## geom icons
library(GenomicRanges)
grs <- GRanges("chr1", IRanges(start = c(1, 30), width = c(10, 30)),
               strand = c("+", "-"), score = c(1, 3))


## geom
setwd("~/Codes/svnrepos/devel/ggbio/inst/examples/icons/")
w <- 10
h <- 5
dev.fun <- function(file, w, h,ext = ".pdf" ){
  nms <- paste(file, ext, sep = "")
  pdf(nms, w, h)
}
dev.fun("geom_rect", w, h)
ggplot() + geom_rect(grs)+ theme_null()
dev.off()
dev.fun("geom_segment", w, h)
ggplot() + geom_segment(grs)+ theme_null()
dev.off()
dev.fun("geom_chevron", w, h)
ggplot() + geom_chevron(grs, size = 2)+ theme_null()
dev.off()
dev.fun("geom_arch", w, h)
ggplot() + geom_arch(grs, size = 2)+ theme_null()
dev.off()
dev.fun("geom_arrow", w, h)
ggplot() + geom_arrow(grs, length = unit(1, "cm"))+ theme_null()
dev.off()

dev.fun("geom_arrowrect", w, h)
ggplot() + geom_arrowrect(grs ,arrow.head = 0.2)+ theme_null()
dev.off()
dev.fun("geom_bar", w, h)
ggplot() + geom_bar(grs)+ theme_null()
dev.off()
dev.fun("geom_alignment", w, h)
ggplot() + geom_alignment(grs)+ theme_null()
dev.off()

## stat
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
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

gr1 <- gr[seqnames(gr) == "chr1"]
dev.fun("stat_coverage", w, h)
ggplot() + stat_coverage(gr1, geom = "area") + theme_null()
dev.off()
dev.fun("stat_aggregate", w, h)
ggplot() + stat_aggregate(gr1, y = "score", window = 40) + theme_null()
dev.off()
## gene
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
p2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"])
dev.fun("stat_gene", w, h)
p2 + theme_null()
dev.off()

dev.fun("stat_identity", w, h)
ggplot() + stat_identity(gr1[1:6], aes(y = score), geom = "rect") + theme_null()
dev.off()
## mismatch
## simulated
df <- data.frame(x = c(rep(3, 10), rep(5, 20)),
                 type = c(rep("A", 5), rep("C", 5), rep("T", 6), rep("G", 14)))
cols <- strandColor <- getOption("biovizBase")$DNABasesColor
dev.fun("stat_mismatch", w, h)
ggplot() +   geom_bar(data = df, aes(x = factor(x), fill = type)) +
  scale_fill_manual(values = cols) +
 theme_null() + opts(legend.position = "none")
dev.off()

## stepping
grss <-  GRanges("chr1", IRanges(start = c(1, 10), width = 20),
               strand = "-", score = c(1, 3))

dev.fun("stat_stepping", w, h)
ggplot() + stat_stepping(grss) + theme_null()
dev.off()

## table
dev.fun("stat_table", w, h)
grs2 <-  GRanges("chr1", IRanges(start = c(1, rep(30, 10)), width = c(10, rep(30, 10))))
ggplot() + stat_table(grs2, aes(y = score, fill = score), stat = "identity", geom = "rect") + theme_null() + opts(legend.position = "none")
dev.off()
## default
dev.fun("coord_linear", w, h)
autoplot(gr2[1:50])
dev.off()
## coord_genome
gr2 <- gr[seqnames(gr) != "chr3"]
gr2 <- keepSeqlevels(gr2, paste("chr", 1:2, sep = ""))
seqlengths(gr2) <- c(400, 500)
gr2
dev.fun("coord_genome", w, h)
plotGrandLinear(gr, aes(y = score), color = c("red", "blue", "orange"), size = 4) + 
  opts(legend.position = "none")
dev.off()
## coord_truncate
dev.fun("coord_truncate_gaps", w, h)
autoplot(grs, coord = "truncate_gaps", ratio = 0)
dev.off()

## layout karyogram
data("hg19Ideogram", package = "biovizBase")
data("hg19IdeogramCyto", package = "biovizBase")

seqlengths(hg19IdeogramCyto) <- seqlengths(hg19Ideogram)[names(seqlengths(hg19IdeogramCyto))]
dev.fun("layout_karyogram", w, h)
df <- keepSeqlevels(hg19IdeogramCyto, c(paste("chr", c(1,2 ), sep = "")))
df2 <- df[sample(1:length(df), size = 10)]
width(df2) <- 1e6
ggplot() + layout_karyogram(df, cytoband = FALSE) + layout_karyogram(df2, geom = "rect",
                                  fill = "steelblue") +
  theme_null() + opts(legend.position = "none")

dev.off()

## layout circle
dev.fun("layout_circle", w, h)
grc <- GRanges(c("chr1", "chr2", "chr3"),
               IRanges(start = 1,
                       width = 100))
seqlengths(grc) <- c(100, 200, 300)

autoplot(grc, layout = "circle", geom = "ideo")
dev.off()
## facetting
autoplot(gr)



dev.fun("facet", w, h)
gr2 <- gr[seqnames(gr) != "chr2"]
values(gr2)$.new <- interaction(values(gr2)$sample, as.character(seqnames(gr2)))
autoplot(gr2, facets = sample ~ seqnames, stat = "coverage", geom = "area", aes(fill = .new)) +
  theme_bw()
dev.off()

dev.fun("facet_gr", w, h)
gr2 <- gr[seqnames(gr) != "chr2"]
autoplot(gr2,  stat = "coverage", geom = "area", aes(fill = seqnames)) +  theme_bw() + opts(legend.position = "none")
dev.off()



## autoplot,GRanges
## autoplot,GRangesList
gr1 <- GRanges("1", IRanges(c(1, 20), width = 10))
gr2 <- GRanges("1", IRanges(c(5, 25), width = 10))
grl <- GRangesList(gr1, gr2)
dev.fun("grl", w, h)
autoplot(grl) + theme_null()
dev.off()
## autoplot,IRanges
## autoplot,BamFile
## autoplot,Rle
## autoplot,TranscriptDb
## chracter




