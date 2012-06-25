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
dev.fun("geom_arrowrect", w, h)
library(grid)
ggplot() + geom_arrowrect(grs, arrow.head = 0.2)+ theme_null()
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

p2.2 <- ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], stat = "reduce")
setwd("~/Desktop/")
dev.fun("stat_reduce", w, h)
p2.2 + theme_null()
dev.off()

dev.fun("stat_identity", w, h)
ggplot() + stat_identity(gr1[1:6], aes(y = score), geom = "rect") + theme_null()
dev.off()
## mismatch
## simulated
x0 <- seq(-5, 5, length = 1000)
y0 <- -x0^2 + 25
df0 <- data.frame(x0 = c(min(x0), x0, max(x0)), y0 = c(0, y0, 0))
df <- data.frame(x = c(0), y  = c(15))
dev.fun("stat_mismatch", w, h)
ggplot() +  geom_area(data = df0, aes(x = x0, y = y0), fill = "gray70") + 
  geom_bar(data = df, aes(x = x, y = y), stat = "identity", fill = "red") +
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
grs2 <-  GRanges("chr1", IRanges(start = c(1, rep(15, 4)), width = c(10, rep(10, 4))))
p1 <- autoplot(grs2)+ theme_null()
p2 <- ggplot() + stat_table(grs2) + scale_fill_continuous(space = "Lab") + theme_null() + opts(legend.position = "none")
tracks(p1, p2, heights = c(3, 1))
dev.off()
## default
grco <- GRanges(c("1", "1", "2", "2"),
                IRanges(c(1, 5, 1, 5), width = 10))
dev.fun("coord_linear", w, h)
autoplot(grco) + theme_alignment(grid = FALSE)+
  opts(strip.background = theme_rect(fill = 'black')) + opts(axis.text.x = theme_blank(),
                                       axis.ticks = theme_blank()) + xlab("") + ylim(c(0, 3))

dev.off()
## coord_genome
dev.fun("coord_genome", w, h)
autoplot(grco, coord = "genome") + theme_alignment(grid = FALSE) +
    opts(strip.background = theme_rect(fill = 'black')) + opts(axis.text.x = theme_blank(),
                                       axis.ticks = theme_blank()) + xlab("") + ylim(c(0, 3))

dev.off()
## coord_truncate
grs <- GRanges("1", IRanges(c(1, 100), width = 20))
dev.fun("coord_truncate_gaps", w, h)
p1 <- autoplot(grs)
p2 <- autoplot(grs, coord = "truncate_gaps", ratio = 0.2, truncate.gaps = TRUE)
tracks(p1, p2) + theme_null()
dev.off()


## layout default
grc <- GRanges(c("chr1", "chr2", "chr3"),
               IRanges(start = c(30, 50, 70),
                       width = 5))

seqlengths(grc) <- c(100, 200, 300)

dev.fun("layout_default", w, h)
autoplot(grc)+  theme_alignment(grid = FALSE)  +
  opts(strip.background = theme_rect(fill = 'black')) + opts(axis.text.x = theme_blank(),
                                       axis.ticks = theme_blank()) + xlab("") + ylim(c(-1, 3))

dev.off()
## layout karyogram
dev.fun("layout_karyogram", w, h)
autoplot(grc, layout = "karyogram")
dev.off()
## layout circle
dev.fun("layout_circle", w, h)
grc <- GRanges(c("chr1", "chr2", "chr3"),
               IRanges(start = 1,
                       width = 100))
seqlengths(grc) <- c(100, 200, 300)
grc
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




## gappedalignment
gr <- GRanges("1", IRanges(c(1, 10), width = 5))
gr2 <- GRanges("1", IRanges(1, width = 5))
grl <- GRangesList(gr, gr2)
dev.fun("autoplot_GappedAlignment", w, h)
autoplot(grl, gap.geom = "segment") + theme_null()
dev.off()
