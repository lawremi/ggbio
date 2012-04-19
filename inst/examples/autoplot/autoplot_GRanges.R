## @knitr load
library(ggbio)

## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
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

## @knitr default
autoplot(gr[idx])

## @knitr bar
set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b2 <- GRanges(seqnames = "chr2", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b <- c(gr.b, gr.b2)
## default use score as y
autoplot(gr.b, geom = "bar", aes(fill = value))
autoplot(gr.b, geom = "bar", aes(y = value))


## @knitr geom/aes/facet
autoplot(gr, geom = "point", aes(y = score))
autoplot(gr, fill = "red")
autoplot(gr, aes(fill = value))
autoplot(gr, facets = sample ~ seqnames)
autoplot(gr[idx], geom = "chevron", offset = 1)
autoplot(gr[idx], geom = "arrowrect", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)
autoplot(gr[idx], geom = "alignment", aes(group = pair), facets = sample ~ seqnames)

## @knitr group
## make a minimal sample .
gra <- GRanges("chr1", IRanges(c(1,7,20), end = c(4,9,30)), group = c("a", "a", "b"))
## if you desn't specify group, then group based on stepping levels, and gaps are computed without
## considering extra group method
autoplot(gra, aes(fill = group), geom = "alignment")
## when use group method, gaps only computed for grouped intervals.
## default is group.selfish = TRUE, each group keep one row.
## in this way, group labels could be shown as y axis.
autoplot(gra, aes(fill = group, group = group), geom = "alignment")
## group.selfish = FALSE, save space
autoplot(gra, aes(fill = group, group = group), geom = "alignment", group.selfish = FALSE)
## without group method
autoplot(gra, aes(fill = group), geom = "alignment", group.selfish = FALSE)

## @knitr group-more
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)

## @knitr facet:strand
autoplot(gr, stat = "coverage", geom = "area", facets = strand ~ seqnames, aes(fill = strand))

## @knitr NULL
library(biovizBase)
autoplot(gr[idx], geom = "alignment", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair))
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

## @knitr stat
autoplot(gr, stat = "coverage", facets = sample ~ seqnames)
autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)
autoplot(gr, stat = "aggregate", y = "value")
## for boxplot, y need to be in aes()
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot", facets = sample ~ seqnames)

## @knitr NULL
autoplot(gr, stat = "coverage", facets = sample ~ seqnames, geom = "point")
autoplot(gr, stat = "table")
autoplot(gr, stat = "table", rect.height = 0.2)
autoplot(gr, stat = "table", rect.height = 0.2, fill = "red")

autoplot(gr, stat = "boxplot", aes(y = score))


autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)


autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot")
## FIXME: aggregate error
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 38)
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 2)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 10)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 30,
         geom = "histogram")


## @knitr coord:genome
## simple example
autoplot(gr, coord = "genome")

## @knitr coord:truncate_gaps
## simple example
autoplot(gr, coord = "genome")

## @knitr layout:circle
autoplot(gr, layout = "circle", geom = "ideo", rect.inter.n = 50, aes(fill = .ori.seqnames))


## @knitr NULL
gr1 <- gr[seqnames(gr) == "chr1"]
gr1 <- keepSeqlevels(gr1, names((seqlengths(gr1))))
idx <- sample(1:length(gr1), size = 50)
p1 <- autoplot(gr1[idx])
p2 <- autoplot(gr1[idx], stat = "coverage", geom = "area")

pdf("~/Desktop/stat_coverage.pdf", 10, 10)
tracks(p1, p2)
dev.off()

set.seed(123)
N <- 100
gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(c(1:300, 5000:5300, 10000:10300), size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE))

gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(c(1:300, 5000:5300, 10000:10300), size = N, replace = TRUE),
                      width = sample(70:75, size = N,replace = TRUE)))


gr.t <- 
p1 <- autoplot(gr)
p2 <- autoplot(gr, truncate.gaps = TRUE)
align.plots(p1, p2)
seqlengths(gr) <- c(10400, 2e4, 5e4)
autoplot(gr, coord = "genome")

p1 <- autoplot(gr1[idx])
p2 <- autoplot(gr1[idx], stat = "coverage", geom = "area")

autoplot(gr[idx])
autoplot(gr[idx], geom = "arrow")
autoplot(gr[idx], geom = "arrowrect")
autoplot(gr[idx], geom = "alignment")


## let's make a linear tracks for circular view.
load("~/Codes/svnrepos/released/biovizBase/data/crc1.GeRL.rda")

crc1 <- crc1.GeRL[[1]]
library(GenomicRanges)
lst <- lapply(1:length(crc1), function(i){
  gr1 <- crc1[i,]
  gr2 <- values(crc1[i,])$to.gr[1,]
  values(gr1) <- NULL
  values(gr2) <- NULL
  res <- c(gr1, gr2)
  values(res) <- values(crc1[i,])[c(1, 1),]
  res
})
grl <- do.call(GRangesList, lst)
grl
## crc1.grl <- transformToArch(crc1)
crc1.grl2 <- transformToGenome(grl)
crc1.grl2
## crc1.grl3 <- transformToArch(crc1.grl2)
crc1.grl3 <- split(crc1.grl2, values(crc1.grl2)$.group)
res <- transformToArch(crc1.grl3)
res
## crc1.grl3
ggplot() + geom_arch(crc1.gr)
metadata(res)$coord <- "genome"
p1 <- autoplot(res, geom = "arch", aes(color = rearrangements), space.skip = 0.0015) + ylab("")
p2 <- autoplot(crc1.GeRL[[2]], geom = "point", aes(y = score, size = tumreads), color = "red",
         coord = "genome",space.skip = 0.0015)
p3 <- autoplot(crc1.GeRL[[3]], color = "steelblue",  coord = "genome", space.skip = 0.0015)
p4 <- autoplot(crc1.GeRL[[4]], coord = "genome", fill = "gray70", space.skip = 0.0015)
pdf("~/Desktop/track_layout.pdf", 15, 8)

tracks(p1, p2 ,p3, p4, heights = c(2, 4, 1, 1))
dev.off()
autoplot(crc1.GeRL[[3]], layout = "karyogram")
nms <- names(seqlengths(crc1.GeRL[[3]]))
.nms <- gsub("chr", "", nms)
names(.nms) <- nms
res <- renameSeqlevels(crc1.GeRL[[3]], .nms)
autoplot(res, layout = "karyogram", color = "steelblue", fill = "steelblue") + xlab("Human genome")
ggsave("~/Desktop/karyogram.pdf")
autoplot(crc1.grl)

##
grs <- GRanges("chr1", IRanges(start = c(1, 70, 120), width = c(30, 30, 30)),
               strand = c("-", "*", "+"))
p1 <- autoplot(grs, aes(fill = strand)) + ylab("")
p2 <- autoplot(grs, aes(color = strand), geom = "segment") + ylab("")
p3 <- autoplot(grs, aes(color = strand), geom = "chevron") + ylab("")
p4 <- autoplot(grs, aes(color = strand), geom = "arch") + opts(legend.position = "none") + ylab("") 
p5 <- autoplot(grs, geom = "arrow") + ylab("")
p6 <- autoplot(grs, geom = "arrowrect") + ylab("")
p7 <- autoplot(grs, geom = "alignment") + ylab("")
p8 <- autoplot(grs, geom = "alignment", main.geom = "arrowrect", gap.geom = "segment") + ylab("")
library(gridExtra)
pdf("~/Desktop/geoms.pdf")
tracks(p1, p2, p3, p4, p5, p6, p7, p8) + scale_y_continuous(breaks = NULL)
dev.off() 

