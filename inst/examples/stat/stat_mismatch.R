## @knitr load
library(ggbio)
library(BSgenome.Hsapiens.UCSC.hg19)
data("genesymbol", package = "biovizBase")

## @knitr load_bam
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
library(Rsamtools)
bf <- BamFile(bamfile)

## @knitr BamFile
ggplot() + stat_mismatch(bf, which = genesymbol["RBM17"],
                         bsgenome = Hsapiens,show.coverage = TRUE) +
  coord_cartesian(xlim = c(6134000, 6135000), wise = TRUE) + theme_bw()

## @knitr pag
library(biovizBase)
pgr <- pileupAsGRanges(bamfile, region = genesymbol["RBM17"])
pgr.match <- pileupGRangesAsVariantTable(pgr, genome = Hsapiens)

## @knitr pag_v
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE)
ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE)
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE, geom = "bar")

## @knitr NULLL
ggplot() + stat_mismatch(bf, which = GRanges("chr1", IRanges(1, 2)),
                         bsgenome = Hsapiens,show.coverage = TRUE)

ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE) +
  stat_mismatch(GRanges(), xlab = "xlab", ylab = "ylab", main = "main")

## ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE) +
##   coord_cartesian(xlim = c(6134000, 6135000),wise = TRUE) + theme_bw()
library(ggbio)
library(Rsamtools)
bamfile <- "~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam"
bf <- BamFile(bamfile)
data(genesymbol, package = "biovizBase")

rng.ori <- genesymbol["PYGB"]
rng <- GRanges("20", ranges(rng.ori))
rng
## red vcf
library(VariantAnnotation)
svp_all <- ScanVcfParam(which=rng)
vcf <- readVcf("~/Datas/1000genome/ALL.chr20.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz", genome = "hg19", svp_all)
p.v <- autoplot(vcf, type = "fixed") + coord_cartesian(ylim = c(0.6, 1.4), wise = TRUE) +
  scale_y_continuous(breaks = NULL)+
  opts(legend.position = "none")

ggplot() + stat_mismatch(bf, which = rng,
                         bsgenome = Hsapiens,show.coverage = TRUE) 
coord_cartesian(xlim = c(6134000, 6135000), wise = TRUE) + theme_bw()


##
gr.t <- GRanges("chr16", IRanges(30080000, 30080000 + 2000))
p0 <- ggplot() + stat_mismatch(bf, which = gr.t,
                         bsgenome = Hsapiens,show.coverage = TRUE,
                               geom = "bar") + ylab("Coverage") +
   ## coord_cartesian(ylim = c(0, 200), wise = TRUE) +
  theme_bw()
library(biovizBase)

pgr <- pileupAsGRanges(bamfile, region = rng)
nms <- "chr20"
names(nms) <- "20"
pgr <- renameSeqlevels(pgr, nms)
pgr.match <- pileupGRangesAsVariantTable(pgr, genome = Hsapiens)
pgr.match
p.v
p1 <- ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE)  +
  coord_cartesian(ylim = c(0, 10), wise = TRUE)
p.v
p3 <- autoplot(Hsapiens, which = rng.ori) + opts(legend.position = "none")
p3
obj <- tracks(p1, p.v, p3, heights = c(4, 0.9, 1), xlim = c(25235400, 25236100))

obj <- tracks(p1, p.v, p3, heights = c(4, 0.9, 1),
              xlim = c(25235720, 25235850))
obj
pdf("~/Desktop/mismatch.pdf", 18.3, 5.98)
obj
dev.off()

obj
update(obj, xlim = c(25238120, 25238470))
update(obj, xlim = c(25235400, 25236100))
ggbio:::reset(obj)

p2 <- autoplot(Hsapiens, which = gr.t, geom = "text") + theme_bw() +
  opts(legend.position = "none") + xlim(c(30080800 + 20, 30080800 + 100))

pdf("~/Desktop/mismatch.pdf", 13.1, 5.8)
tracks(p0, p1, p2, heights = c(4, 4, 1.5), xlim = c(30080800 + 20, 30080800 + 100))
dev.off()
pg.sub <- pgr.match[!values(pgr.match)$match]
pg.s <- start(pg.sub)
vcf.s <- start(alt(vcf))
ss <- pg.s[!is.na(match(pg.s, vcf.s))]
as.character(unlist(values(alt(vcf)[start(alt(vcf)) == 25238173])$ALT))
as.character(unlist(values(alt(vcf)[match(ss, vcf.s)])$ALT)) ==
  values(pg.sub[!is.na(match(pg.s, vcf.s)), "read"])[,1]
as.data.frame(pg.sub[!is.na(match(pg.s, vcf.s)), "read"])
