## TODO:
## 1. tracks margin fix
## 2. exons labels for plotRangesLinkedToData
library(ggbio)
library(GenomicRanges)
## library(devtools)
## load_all("~/Codes/gitrepos/ggbio")
##  GRanges
set.seed(1)
N <- 1000

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

idx <- sample(1:length(gr), size = 200)

grl <- split(gr, values(gr)$sample)
grl <- endoapply(grl, function(gr){
  nms <- setdiff(colnames(values(gr)), "sample")
  values(gr) <- values(gr)[nms]
  gr
})

autoplot(gr, facets = strand ~ seqnames, stat = "coverage", geom = "area",
         aes(fill = strand))
## ======================================================================
##      LOWER  LEVEL API(test)
## ======================================================================
## TODO
## facets == GRanges
## test lower level API
## sta_identity()

## transform to a normal data.frame and do it as free

ggplot() + stat_identity(gr, aes(x = start, y = value), geom = "point")

## yes, geom: alignment, rect,
## FIXME
## ggplot() + stat_identity(gr, aes(xmin = start, xmax = end,
##                                  ymin = value - 0.5, ymax = value + 0.5),
##                           geom = "rect")
## ggplot() + stat_identity(gr, aes(y = values),
##                           geom = "rect")

ggplot() + stat_identity(gr, aes(x = start, y = value),  geom = "line")
ggplot() + stat_identity(gr, aes(x = start + width/2, y = value),  geom = "line")
ggplot() + stat_identity(gr, aes(x = midpoint, y = value),  geom = "line")
## identity for ggbio geom
ggplot() + stat_identity(gr, aes(y = value), geom = "rect")
ggplot() + stat_identity(gr, aes(y = value), geom = "segment")
## stat_stepping()
myfun <- function(data, ...){
  ggplot() + stat_identity(data, ...)
}


myfun(gr, aes(y = value), geom = "segment", facets = sample ~ seqnames)


ggplot() + stat_stepping(gr)
ggplot() + stat_stepping(gr, aes(color = strand))
ggplot() + stat_stepping(gr, aes(color = strand, fill = strand))
ggplot() + stat_stepping(gr, geom = "segment", xlab = "s", ylab = "y", main = "m")

ggplot() + stat_stepping(gr, aes(color = strand), geom = "segment")
ggplot() + stat_stepping(gr, aes(group = pair),geom = "alignment")
ggplot() + stat_stepping(gr, geom = "alignment")

ggplot() + stat_stepping(gr, facets = sample ~ seqnames)

myfun <- function(data, ..., facets){
  ggplot() + stat_stepping(data = data, ..., facets = facets)
}

myfun(data = gr, facets = sample ~ seqnames, color = "red", main = "main", aes(fill = score))
myfun(gr, geom = "alignment")

rm("foo")
setGeneric("foo", function(x, ...) standardGeneric("foo"))
setMethod("foo", "numeric", function(x, ..., drop){
  as.list(match.call())[-1]
})

setMethod("foo", "character", function(x, ..., drop){
  as.list(match.call())[-1]
})

myfun <- function(x, ..., drop){
  foo(x, ..., drop)
}

foo(1, drop = FALSE)
foo("1", drop = TRUE)
myfun(1, drop = FALSE)
myfun("2", drop = TRUE)
myfun("2",  i =2, drop = TRUE)
myfun(2,  i =2, drop = TRUE)

rm("foo")
setGeneric("foo", function(x, ...) standardGeneric("foo"))
setMethod("foo", "numeric", function(x, ..., drop){
  as.list(match.call(call = sys.call(sys.parent(2))))[-1]
})

setMethod("foo", "character", function(x, ..., drop){
  as.list(match.call(call = sys.call(sys.parent(2))))[-1]
})

myfun <- function(x, ..., drop){
  foo(x, ..., drop)
}

foo(1, drop = FALSE)
foo("1", drop = TRUE)
myfun(1, i = 2, drop = FALSE)
myfun("2", i = 2, drop = TRUE)



## stat_coverage
ggplot() + stat_coverage(gr, geom = "point")
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "point")
ggplot() + stat_coverage(gr, geom = "histogram")
ggplot() + stat_coverage(gr, geom = "area")
ggplot() + stat_coverage(gr, geom = "smooth")
ggplot() + stat_coverage(gr, geom = "step")

ggplot() + stat_coverage(gr, geom = "point", facets = NULL)
ggplot() + stat_coverage(gr, geom = "point", facets = sample ~ seqnames)

ggplot() + stat_coverage(gr, geom = "point", facets =  ~ seqnames)
ggplot() + stat_coverage(gr, geom = "point", facets =  . ~ seqnames)

ggplot() + stat_coverage(grl, geom = "area", facets = ..grl_name.. ~ seqnames,
                         aes(fill = ..grl_name..))

ggplot() + stat_coverage(grl, geom = "area", 
                         aes(fill = ..grl_name..))

myfun <- function(data, ...){
  ggplot() + stat_stepping(data, ...)
}
myfun(gr, geom = "alignment")


## stat_table, GRanges
ggplot() + stat_table(gr)
ggplot() + stat_table(gr, geom = "segment", aes(y = ..score.., color = ..score..))
## FIXME
ggplot() + stat_table(gr, aes(color = factor(score)))
## ggplot() + stat_table(gr, rect.height = 0.1, geom = "rect")
ggplot() + stat_table(gr, geom = "segment")
myfun <- function(data, ...){
  ggplot() + stat_table(data, ...)
}
myfun(grl, geom = "segment", aes(y = ..score.., color = ..score..))

## ggplot() + stat_table(gr, rect.height = 0.1, geom = "5poly")
## fix this could be just aes(y = ..score..)
## Maybe confused about stat
ggplot() + stat_table(gr, stat  = "aggregate", y = "score")
ggplot() + stat_table(gr, stat  = "identity", aes(x = midpoint, y = score), geom = "point")

ggplot() + stat_table(gr, stat  = "identity", aes(x = midpoint, y = score), geom = "point")

ggplot() + stat_table(grl, geom = "segment", aes(y = ..score.., color = ..score..))
## stat_aggregate
## 36,37 doesn't work
ggplot() + stat_aggregate(gr, y = "value",fill = "gray40")
ggplot() + stat_aggregate(gr, window = 30,  y = "value",fill = "gray40", geom = "histogram")
ggplot() + stat_aggregate(gr, window = 100, fill = "gray40",y = "value",
                          type = "max", geom = "histogram")
ggplot() + stat_aggregate(gr, window = 100, fill = "gray40",y = "value",
                          type = "max", geom = "bar")

ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "bar")

ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "point")
ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "line")
ggplot() + stat_aggregate(gr, window = 20, y = "value", fill = "gray40", geom = "area")

ggplot() + stat_aggregate(gr, window = 100, aes(y = value), geom = "boxplot")

## debug
ggplot() + stat_aggregate(gr, window = 100, aes(y = value),
                          geom = "boxplot", facets =  sample ~ seqnames)

myfun <- function(data, ...){
  ggplot() + stat_aggregate(data, ...)
}
myfun(gr, window = 20, y = "value", fill = "gray40", geom = "bar")

## stat_mismatch
require(ggbio)
require(BSgenome.Hsapiens.UCSC.hg19)
require(biovizBase)
data("genesymbol")
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", package="biovizBase")
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
## pgr <- pileupAsGRanges(bamfile, region = genesymbol["RBM17"])
pgr <- pileupAsGRanges(bamfile, region = genesymbol["ALDOA"])
pgr.match <- pileupGRangesAsVariantTable(pgr, genome = Hsapiens)

ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE)
ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE)
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE, geom = "bar")

myfun <- function(data, ...){
  ggplot() + stat_mismatch(data, ...)
}
myfun(pgr.match, show.coverage = FALSE, geom = "bar")
## ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE) +
##   coord_cartesian(xlim = c(6134000, 6135000),wise = TRUE) + theme_bw()

bf <- BamFile(bamfile)
myfun(bf, which = genesymbol["RBM17"],bsgenome = Hsapiens,show.coverage = TRUE)

ggplot() + stat_mismatch(bf, which = genesymbol["RBM17"],
                         bsgenome = Hsapiens,show.coverage = TRUE) +
  coord_cartesian(xlim = c(6134000, 6135000), wise = TRUE) + theme_bw()


## make a zoomed track
bf <- BamFile(bamfile)

gr.t <- GRanges("chr16", IRanges(30080000, 30080000 + 2000))
p0 <- ggplot() + stat_mismatch(bf, which = gr.t,
                         bsgenome = Hsapiens,show.coverage = TRUE,
                               geom = "bar") + ylab("Coverage") +
   ## coord_cartesian(ylim = c(0, 200), wise = TRUE) +
  theme_bw()
p1 <- ggplot() + stat_mismatch(bf, which = gr.t,
                         bsgenome = Hsapiens,show.coverage = FALSE,
                               geom = "bar") + 
  coord_cartesian(ylim = c(0, 6), wise = TRUE)+
  theme_bw()
p2 <- autoplot(Hsapiens, which = gr.t, geom = "text") + theme_bw() +
  opts(legend.position = "none") + xlim(c(30080800 + 20, 30080800 + 100))

pdf("~/Desktop/mismatch.pdf", 13.1, 5.8)
tracks(p0, p1, p2, heights = c(4, 4, 1.5), xlim = c(30080800 + 20, 30080800 + 100))
dev.off()

bf <- pgr.match
p0 <- ggplot() + stat_mismatch(bf, show.coverage = TRUE, geom = "bar") + ylab("Coverage") +
  theme_bw()
p1 <- ggplot() + stat_mismatch(bf,show.coverage = FALSE, geom = "bar") + 
  coord_cartesian(ylim = c(0, 6), wise = TRUE)+
  theme_bw()
p2 <- autoplot(Hsapiens, which = gr.t, geom = "text") + theme_bw() +
  opts(legend.position = "none") 
tracks(p0, p1, p2, heights = c(4, 4, 1.5), xlim = c(30080800 + 20, 30080800 + 100))




## autoplot, BamFile

## TODO
require(ggbio)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], fill = "black")
ggplot() + stat_gene(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")
gr2 <- resize(genesymbol["RBM17"], width(genesymbol["RBM17"])-10000)
ggplot() + stat_gene(txdb, which = gr2)

## geom_chevron

ggplot() + geom_chevron(gr[idx])
ggplot() + geom_chevron(gr[idx], facets = sample ~ seqnames)
ggplot() + geom_chevron(gr[idx], offset = 0) #just segment
ggplot() + geom_chevron(gr[idx], offset = 0, color = "red") #just segment
ggplot() + geom_chevron(gr[idx], stat = "identity",
                        aes(x = start, xend = end, y = value, yend = value))

myfun <- function(data, ...){
  ggplot() + geom_chevron(data, ...)
}

myfun(gr[idx], stat = "identity",
                        aes(x = start, xend = end, y = value, yend = value),
      facets = sample ~ seqnames)
## geom_arch
ggplot() + geom_arch(gr[idx])
ggplot() + geom_arch(gr[idx], max.height = 100)
ggplot() + geom_arch(gr[idx], max.height = 100, aes(height = value))
ggplot() + geom_arch(gr[idx], aes(height = width))
ggplot() + geom_arch(gr[idx], max.height = 100, aes(y = value)) #base
ggplot() + geom_arch(gr[idx], facets = sample ~ seqnames)
ggplot() + geom_arch(gr[idx], aes(color = value),)

ggplot() + geom_arch(gr[idx], aes(color = value, height = value, size = value),
                     alpha = 0.2, facets = sample ~ seqnames)


myfun <- function(data, ...){
  ggplot() + geom_arch(data, ...)
}
myfun(gr[idx], aes(color = value, height = value, size = value),
                     alpha = 0.2, facets = sample ~ seqnames)

## geom_arrow
library(grid)
ggplot() + geom_arrow(gr[idx])
ggplot() + geom_arrow(gr[idx], facets = sample ~ seqnames, color = "red")
ggplot() + geom_arrow(gr[idx], stat = "identity", aes(x = start, y = value,
                                        xend = end, yend = value))

myfun <- function(data, ...){
  ggplot() + geom_arrow(data, ...)
}
myfun(gr[idx], stat = "identity", aes(x = start, y = value,
                                        xend = end, yend = value),
      facets = sample ~ seqnames)
## ggplot() + geom_arrow(gr[idx], facets = sample ~ seqnames, color = "red",
##                       aes(x = start, y = value, xend = end, yend = value),
##                       stat = "identity")

## geom_arrowrect
ggplot() + geom_arrowrect(gr[idx])
ggplot() + geom_arrowrect(gr[idx], facets = sample ~ seqnames, fill = "red")
ggplot() + geom_arrowrect(gr[idx], stat = "identity", aes(y = value), rect.height = 0.1)


ggplot() + geom_arrowrect(gr[idx], facets = sample ~ seqnames, fill = "red",
                      aes(x = start, y = value, xend = end, yend = value),
                      stat = "identity",  rect.height = 0.2)

myfun <- function(data, ...){
  ggplot() + geom_arrowrect(data, ...)
}

myfun(gr[idx], facets = sample ~ seqnames, fill = "red",
                      aes(x = start, y = value, xend = end, yend = value),
                      stat = "identity",  rect.height = 0.2)
## geom_rect
## for data.frame
ggplot() + geom_rect(data = mtcars, aes(xmin = mpg, ymin = wt,
                       xmax = mpg + 10, ymax = wt + 10))
ggplot() + geom_rect(gr)
ggplot() + geom_rect(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))
ggplot() + geom_rect(gr, stat = "identity", aes(y = value))
ggplot() + geom_rect(gr, facets = sample ~ seqnames, aes(y = value,color = strand, fill = strand),
                     stat = "identity")

myfun <- function(data, ..., facets = NULL){
  ggplot() + geom_rect(data,..., facets = facets)
}
myfun(data = mtcars, aes(xmin = mpg, ymin = wt,
        xmax = mpg + 10, ymax = wt + 10), facets = .~cyl)
myfun(gr)
myfun(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))
myfun(gr, geom = "alignment")
library(biovizBase)

## geom_segment
ggplot() + geom_segment(gr)
ggplot() + geom_segment(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))

ggplot() + geom_segment(gr, stat = "identity", aes(y = value))
ggplot() + geom_segment(gr, facets = sample ~ seqnames,
                        aes(y = value,color = strand, fill = strand),
                     stat = "identity")


myfun <- function(data, ..., facets = NULL){
  ggplot() + geom_segment(data,..., facets = facets)
}
myfun(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))
## geom_chevron
ggplot() + geom_chevron(gr)
##  check later
ggplot() + geom_chevron(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))

ggplot() + geom_chevron(gr, stat = "identity", aes(y = value))

ggplot() + geom_chevron(gr, facets = sample ~ seqnames,
                        aes(y = value,color = strand, fill = strand),
                     stat = "identity")

## geom_alignment
ggplot() + geom_alignment(gr[idx])

ggplot() + geom_alignment(gr[idx], aes(group  = pair))
ggplot() + geom_alignment(gr[idx], aes(group  = pair), group.selfish = FALSE)
ggplot() + geom_alignment(gr[idx], main.geom = "arrowrect")
ggplot() + geom_alignment(gr[idx], main.geom = "arrowrect", aes(group  = pair))
ggplot() + geom_alignment(gr[idx], main.geom = "arrowrect", gap.geom = "arrow")
ggplot() + geom_alignment(gr[idx], facets = sample ~ seqnames, aes(color = strand, fill = strand))

myfun <- function(data, ..., facets = NULL){
  ggplot() + geom_alignment(data,..., facets = facets)
}
myfun(gr[idx], facets = sample ~ seqnames, aes(color = strand, fill = strand), color = "red", xlab = "xlab")
## ======================================================================
##           HIGHER LEVEL API
## ======================================================================

## ----------------------------------------------------------------------
##        autoplot, GRanges
## ----------------------------------------------------------------------
autoplot(gr)
## FIXME:
autoplot(gr, geom = "point", aes(y = score))

autoplot(gr, fill = "red")
autoplot(gr, aes(fill = value))
autoplot(gr, facets = sample ~ seqnames)
autoplot(gr, geom = "chevron", facets = sample ~ seqnames)


autoplot(gr[idx], geom = "5poly", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)

autoplot(gr, geom = "segment", facets = sample ~ seqnames)

autoplot(gr[idx], geom = "alignment", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair))
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

autoplot(gr, stat = "coverage", facets = sample ~ seqnames)
autoplot(gr, stat = "coverage", facets = sample ~ seqnames, geom = "point")

autoplot(gr, stat = "table")
autoplot(gr, stat = "table", rect.height = 0.2)
autoplot(gr, stat = "table", rect.height = 0.2, fill = "red")

autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)
autoplot(gr, stat = "aggregate", y = "value")
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot")
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot", facets = sample ~ seqnames)
## FIXME: aggregate error
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 2)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 10)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 30,
         geom = "histogram")


## ----------------------------------------------------------------------
##        autoplot, GRangesList
## ----------------------------------------------------------------------
library(ggbio)
require(GenomicRanges)
set.seed(1)
s1 <- c(1, 1000, 1500)
w1 <- c(600, 300, 100)
## canonical
gr.c <- GRanges("chr1", IRanges(s1, width = w1),
                strand = c("+", "+", "-"))
## gaps how to represent?
gaps1 <- gaps2 <- GRanges("chr1", gaps(ranges(gr.c), start = min(start(gr.c)),
                       end = max(end(gr.c))))
values(gaps1)$size <- c(100, 200)
values(gaps2)$size <- c(10, 1000)
gr.g <- GRangesList(r1 = gaps1,
                    r2 = gaps2)


N.r <- 1000
p.r1 <- c(0.45, 0.45, 0.1)
p.r2 <- c(0.45, 0.1, 0.45)
w.r1 <- 65 + sample(-10:10, size = N.r, replace = TRUE)
w.r2 <- 65 + sample(-10:10, size = N.r, replace = TRUE)
s.r1 <- sample(c(1:600, 1000:1299, 1500:1599),
              prob = rep(p.r1, w1),
              replace = TRUE, size = N.r)

s.r2 <- sample(c(1:600, 1000:1299, 1500:1599),
              prob = rep(p.r2, w1),
              replace = TRUE, size = N.r)
grl.r <- GRangesList(r1 = GRanges("chr1", IRanges(s.r1, width = w.r1)),
                     r2 = GRanges("chr1", IRanges(s.r2, width = w.r2)))




## plotting
p1 <- autoplot(gr.g, geom = "arch", aes(size = size), color = "steelblue",
               max.height = 200, ylab = "coverage") +
  stat_coverage(flatGrl(grl.r), facets = .grl.name ~ .)
p2 <- autoplot(gr.c, geom = "alignment", main.geom = "5poly", ylab = "")
pdf("~/Desktop/splice.pdf", 10, 5)
tracks(p1, p2, heights = c(3, 1))
dev.off()

p1 <- autoplot(gr.g, geom = "arch", aes(size = size), color = "steelblue",
               max.height = 200, ylab = "coverage") +
  stat_coverage(flatGrl(grl.r), fill = "grey50",facets = .grl.name ~ .) + theme_bw()
p2 <- autoplot(gr.c, geom = "alignment", fill = "gray50", ylab = "")+
  theme_alignment(border = FALSE)
pdf("~/Desktop/splice2.pdf", 10, 5)
tracks(p1, p2 , heights = c(3, 1))
dev.off()

## plot GRangesList, a more general approach
grs <- flatGrl(grl.r, "sample")
## group them make them looks like gaps
idx <- findOverlaps(grs, reduce(grs))@subjectHits
values(grs)$group.hits <- idx
Ngaps <- 300
idx <- sample(1:N.r, size = Ngaps, replace = FALSE)
.g <- c(1, 2,3 )
values(grs)$.id <- 1:length(grs)
values(grs)$group <- seq(300, by = 1, length = length(grs))
.i <- 0
for(id in idx){
  .i <- .i + 1
  cur.id <- values(grs[id])$.id
  hits.cur <- values(grs)[id, "group.hits"]
  next.hits <- setdiff(.g, hits.cur)[sample(1:2, size = 1)]
  next.gr <- grs[values(grs)$group.hits == next.hits]
  idx2 <- sample(1:length(next.gr), size = 1)
  next.id <- values(next.gr[idx2])$.id
  values(grs)$group[c(cur.id, next.id)] <- .i
}

grl <- split(grs, values(grs)$group)

## ggplot() + stat_table(unlist(gps), geom = "arch")

## defaul is alignment, slow
autoplot(grl[idx])
## chekc

## autoplot(grl[idx], group.selfish = TRUE)
## 
## autoplot(grl[idx], group.selfish = TRUE) + scale_y_continuous(breaks = NULL)
## rect/segment is fast
autoplot(grl[idx], geom = "segment")         #no groupping
autoplot(grl[idx], geom = "segment", group.selfish = TRUE)         #no groupping
autoplot(grl[idx], geom = "rect")
autoplot(grl, type = "sashimi")
autoplot(grl, type = "sashimi", coverage.col = "white", coverage.fill = "black",
         color = "red", arch.offset = 2, xlab = "Genomic Coordinates",
         ylab = "coverage", main = "hello world")



values(gr.c)$value <- 100
p <- autoplot(grl, type = "sashimi")
p + geom_rect(data = gr.c, stat = "identity", aes(y = value), rect.height = 10)
p + geom_arrowrect(data = gr.c, stat = "identity", aes(y = value), rect.height = 10)


## equals to lower level: more delicate control
gps <- unlist(psetdiff(unlist(range(grl), use.names=FALSE), grl))
ggplot() + stat_table(gps, geom = "arch", aes(size = score), alpha = 0.5,
                      color = "steelblue", max.height = 300) +
  stat_coverage(unlist(grl), color = "green", fill = "red")


## ## FIXME: fix the overlaped label, need to make it strict
## gr1 <- GRanges("chr1", IRanges(s1, width = e1), value = 1:2)
## gr2 <- GRanges("chr1", IRanges(c(30, 40), width = 5), value = 3:4)
## grl <- GRangesList(gr1, gr2)
## autoplot(grl, aes(fill = size))
## autoplot(grl, aes(fill = size))
## autoplot(grl, aes(size = size), geom = "arch")

## ----------------------------------------------------------------------
##        autoplot, TranscriptDb
## ----------------------------------------------------------------------
autoplot(txdb, which = genesymbol["RBM17"])
autoplot(txdb, which = genesymbol["RBM17"], geom = "reduced_gene")


## ----------------------------------------------------------------------
##        layout_karyogram
## ----------------------------------------------------------------------
## layout karyogram
requrie(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
library(GenomicRanges)
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
## lst <- as.list(new.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
ggplot() + layout_karyogram(new.ideo, cytoband = TRUE)


## ----------------------------------------------------------------------
##        autoplot, BSgenome
## ----------------------------------------------------------------------
## BSgenome for reference genome
gr <- GRanges("chr1", IRanges(5e7, 5e7+50))
autoplot(Hsapiens, which = gr, geom = "text")
autoplot(Hsapiens, which = gr, geom = "text", size = 10, color = "red")
autoplot(Hsapiens, which = gr, geom = "point")
autoplot(Hsapiens, which = gr, geom = "point", size = 10, color = "red") 
autoplot(Hsapiens, which = gr, geom = "segment")
autoplot(Hsapiens, which = gr, geom = "segment", size = 10, color = "red") 
autoplot(Hsapiens, which = gr, geom = "rect")
autoplot(Hsapiens, which = gr, geom = "rect", size = 10, color = "red") 


p1 <- autoplot(Hsapiens, which = gr, geom = "text") 
p2 <- autoplot(Hsapiens, which = gr, geom = "point") 
p3 <- autoplot(Hsapiens, which = gr, geom = "segment") 
p4 <- autoplot(Hsapiens, which = gr, geom = "rectangle") 
tracks(p1, p2, p3, p4)

p <- plotStackedOverview(new.ideo, cytoband = TRUE)
print(p)
## fixing order
new.ideo <- sort(new.ideo)
p <- plotStackedOverview(new.ideo, cytoband = FALSE, facets = . ~ seqnames)
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



ggplot() + geom_arch(data = grl, aes(size = size), rect.height = 5)
ggplot() + geom_arch(data = grl, aes(height = value, size = size, color = value), rect.height = 5)

## before 7 am
## layout for autoplot
## karyogram
## linear

## stat_gene(geom = c("gene", "reduced")), TranscriptDb
## stat_mismatch



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
autoplot(gr, geom = "segment", facets = sample ~ seqnames)
autoplot(gr, geom = "line", y = value, facets = sample ~ seqnames)
autoplot(gr, geom = "point", y = value, facets = sample ~ seqnames)
autoplot(gr, geom = "line", stat = "coverage", facets = sample ~ seqnames)
autoplot(gr, geom = "area", stat = "coverage", facets = sample ~ seqnames)

autoplot(gr[seqnames(gr) == "chr1"], facets = sample ~ seqnames)

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
                  sample = sample(c("Normal", "Tumor"), size = nchr*nsnps,
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
                facets = sample ~ .,  color.type = "twocolor")
library(biovizBase)
library(ggplot2)
## change two color
plotGrandLinear(gr.snp, y = pvalue, geom = "point",
                facets = sample ~ .,  color.type = "twocolor",
                two.color = c("red", "blue"))
## geom line
plotGrandLinear(gr.snp, y = pvalue,
                geom = "line", facet = sample ~ .)  




## add size and change color
plotGrandLinear(gr.snp, y = pvalue, size = pvalue,geom = "point",
                facet = sample ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, size = I(0.05),
                geom = "point", facet = sample ~ .)

plotGrandLinear(gr.snp, y = pvalue, color = sample, geom = "point",
                facet = sample ~ .,
                color.type = "identity")

plotGrandLinear(gr.snp, y = pvalue, color = I("blue"), geom = "point", facet = sample ~ .,
                color.type = "identity")


## facet by seqnames, slower
plotGrandLinear(gr.snp, y = pvalue,geom = "point", 
                facet = sample ~ seqnames, scales = "free", space = "free")


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
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames))+ theme_null()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line")+ theme_null()
ggplot() + layout_circle(gr, aes(y = "size", color = seqnames), geom = "line",
                         direction = "anticlockwise")+ theme_null()


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
values(gr)$sample <- rep(names(cstest.sub), times = elementLengths(cstest.sub))
library(ggbio)
p1 <- autoplot(gr, facets = sample ~ seqnames, stat = "coverage", geom = "line")
p2 <- autoplot(gr, facets = sample ~ seqnames, stat = "coverage", geom = "area", fill = sample)
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
library(TxDb.Mmusculus.UCSC.mm9.knownGene)
## names.expr accept expression now
p2 <- autoplot(TxDb.Mmusculus.UCSC.mm9.knownGene, 
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
p2 + theme_alignment(label = TRUE, border = TRUE, axis = TRUE, grid = TRUE)
p2 + theme_alignment(label = TRUE, border = TRUE, axis = TRUE, grid = TRUE)
p2 + theme_alignment()



## TODO: facet[[]]

## tracks

library(ggplot2)
library(gridExtra)
head(mtcars)
p1 <- qplot(data = mtcars, x = mpg, y = wt, facets = cyl ~ ., color = cyl)
p1 <- qplot(data = mtcars, x = mpg, y = wt, facets = cyl ~ .)
p2 <- qplot(data = mtcars, x = mpg, y = wt)
align.plots(p1, p2)



set.seed(1)
N <- 1000

gr <- GRanges(seqnames = 
              sample(c("chr1", "chr2", "chr3"),
                     size = N, replace = TRUE),
              IRanges(
                      start = sample(1:300, size = N, replace = TRUE),
                      width = sample(10:15, size = N,replace = TRUE)),
              strand = sample(c("+", "-", "*"), size = N, 
                replace = TRUE),
              value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))

N <- 1e3

gr2 <-  GRanges(seqnames = 
                sample(c("chr1", "chr2", "chr3"),
                       size = N, replace = TRUE),
                IRanges(
                        start = sample(1:300, size = N, replace = TRUE),
                        width = 1),
                strand = sample(c("+", "-", "*"), size = N, 
                  replace = TRUE),
                value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
                sample = sample(c("Normal", "Tumor"), 
                  size = N, replace = TRUE),
                pair = sample(letters, size = N, 
                  replace = TRUE))

gr3 <- c(gr, gr2)
seqlengths(gr) <- c(315, 315, 315)
## rect
## so, if width = 1, it doesn't work
ggplot() + layout_circle(data = gr3, aes(fill = value), geom = "rectangle") + theme_null()+
  opts(aspect.ratio = 1)

ggplot() + layout_circle(data = gr2, aes(fill = value), geom = "rectangle") + theme_null()+
  opts(aspect.ratio = 1)

ggplot() + layout_circle(data = gr2, geom = "rectangle") + theme_null()+
  opts(aspect.ratio = 1)


## segment
ggplot() + layout_circle(data = gr, aes(color = value), geom = "segment") + theme_null()+ #
  opts(aspect.ratio = 1)

## ideogram
## TODO: rect.inter.n need to automatically figure out the right number
ggplot() + layout_circle(data = gr,  geom = "ideogram", rect.inter.n = 10) + theme_null()+
  opts(aspect.ratio = 1)
## bar
ggplot() + layout_circle(data = gr2, aes(color = value), geom = "bar") + theme_null()+
  opts(aspect.ratio = 1)

## scale
library(ggplot2)
library(grid)
p <- qplot(data = mtcars, x = mpg, y = wt, facets = cyl ~ ., color = cyl)
p2 <- qplot(data = mtcars, x = mpg, y = wt)
p3 <- qplot(data = mtcars, x = mpg, y = wt, facets = cyl ~ .)
ggplot_gtable(ggplot_build(p3))
ggplot_build(p3)
tracks(p, p2)


plot_theme

library(grid)
v <- viewport()
obj <- ggplot_gtable(ggplot_build(p))
names(obj)
names(obj$grobs)
obj$widths
obj$heights
obj$respect

idx <- grep("guide-box", obj$layout$name)

grobWidth(obj$grobs[[idx]])
library(Biobase)
data(sample.ExpressionSet)
res <- as(sample.ExpressionSet, "data.frame")
res <- as.data.frame(exprs(sample.ExpressionSet))
max(res[2, ])
phenoData(sample.ExpressionSet)@data
head(exprs(sample.ExpressionSet))
plotmatrix(as.data.frame(exprs(sample.ExpressionSet)))
plotMatrix(sample.ExpressionSet)
es <- sample.ExpressionSet
head(as.data.frame(exprs(es)))
ggpcp(mtcars)  + geom_line()
ggpcp(mtcars, vars=names(mtcars[2:6])) + geom_line()
 ggpcp(mtcars) + geom_boxplot(aes(group=variable))

res <- transform(sample.ExpressionSet)
head(res)

transform <- function(object, ...){
}



library(reshape2)
library(ggplot2)
res.m <- melt(res)
head(res.m)
p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
     colour = "white") + scale_fill_gradient(low = "white",
     high = "steelblue")

res <- transform(es)
## matrix is slow
autoplot(es[1:10, 1:10], type = "matrix")
autoplot(es, type = "parallel")
autoplot(es, type = "boxplot")
autoplot(es, type = "heatmap")
autoplot(es, x = A, template = "none")
library(ggplot2)
plotmatrix(as.data.frame(exprs(es)))

head(df.m)

library(affy)
library(CLL)
data("CLLbatch")
data("disease")
rownames(disease) <- disease$SampleID
sampleNames(CLLbatch) <- sub("\\.CEL$", "", sampleNames(CLLbatch))
mt <- match(rownames(disease), sampleNames(CLLbatch))
vmd <- data.frame(labelDescription = c("Sample ID", "Disease Status"))
phenoData(CLLbatch) <- new("AnnotatedDataFrame", data = disease[mt,],
varMetadata = vmd)
CLLbatch <- CLLbatch[, !is.na(CLLbatch$Disease)]

autoplot(CLLbatch, type = "NUSE")
autoplot(CLLbatch, type = "RLE")
## TODO:reorder
badArray <- match("CLL1", sampleNames(CLLbatch))
CLLB <- CLLbatch[, -badArray]
CLLrma <- rma(CLLB)

library(genefilter)
library("hgu95av2")
CLLf <- nsFilter(CLLrma, remove.dupEntrez = FALSE, var.cutof = 0.5)$eset


library(limma)
## change to CLLf later
design <- model.matrix(~ CLLrma$Disease)
clllim <- lmFit(CLLrma, design)
clleb <- eBayes(clllim)
##
biocLite("CCl4")
library(CCl4)
dataPath <- system.file("extdata", package = "CCl4")
adf <- read.AnnotatedDataFrame("samplesInfo.txt", path = dataPath)
adf
targets <- pData(adf)
targets$FileName <- row.names(targets)
RG <- read.maimage
MA <- normalizeWithinArrays(RG, method = "none", bc.method = "none")

biocLite("ALL")
library(ALL)
data("ALL")
ALL
bcell <- grep("^B", as.character(ALL$BT))
moltype <- which(as.character(ALL$mol.biol) %in% c("NEG", "BCR/ABL"))
all_bcrneg <- ALL[, intersect(bcell, moltype)]
all_bcrneg$mol.biol <- factor(all_bcrneg$mol.biol)
getMethod("meanSdPlot", "matrix")
autoplot(all_bcrneg, ranks = FALSE, type = "mean-sd")
autoplot(all_bcrneg, ranks = TRUE, type = "mean-sd")

sds <- esApply(ALL, 1, sd)
sel <- (sds > quantile(sds, 0.8))
allset1 <- all_bcrneg[sel,]

autoplot(allset1, type = "volcano")
autoplot(allset1, fac = "mol.biol", type = "volcano")

allset1
plotMA(allset1, array = 2)
?plotMA
showMethods("plotMA", "ExpressionSet")
res <- as.matrix(allset1)
ncol(res)


## geom arrow
library(ggplot2)
library(grid)
df <- data.frame(x = 1:10, y = rnorm(10), value = runif(10))
library(GenomicRanges)
gr <- GRanges("chr1", IRanges(start = 1:10, width = 5), value = rnorm(10))
ggplot() + geom_arrow(data = gr)


library(Gviz)
dat <- sin(seq(pi, 10 * pi, len = 500))
dTrack.big <- DataTrack(start = seq(1, 1e+05, len = 500),
                        width = 15, chromosome = "chrX", genome = "hg19",
                        name = "sinus", data = sin(seq(pi, 5 * pi, len = 500)) *
                        runif(500, 0.5, 1.5))


aggregate(gr, by = IRanges(start = c(1, 5),width = 4), function(x) values(x))
aggregate(gr, width = 5,  FUN = function(x) values(x))
df <- data.frame(x = seq(1,100,length = 100), y = rnorm(100))
df <- data.frame(x = c(1, 5, 6), y = 3:5)
qplot(data = df, x = x, y = y, geom = "histogram", stat = "identity")
qplot(data = df, x = x, y = y, geom = "step", direction = "vh")
gr
ggplot() + stat_aggregate(gr, window = 30, aes(y = value), fill = "gray40")
ggplot() + stat_aggregate(gr, window = 30, aes(y = value), fill = "gray40",
                          type = "max")


data <- gr
data
window = 4


## broken code
varsToEval <- c("main", "xlim", "ylim")

fun <- function(x, ...) {  
  mc <- as.list(match.call()[-1])
  missingSym <- alist(foo=)
  catcherArgNames <- c(setdiff(names(mc), varsToEval), "...")
  catcherArgs <- structure(rep(missingSym, length(catcherArgNames)),
                           names = catcherArgNames)
  catcher <- as.function(c(catcherArgs, quote(list(...))))
  catcher(...)
}

fun_broken <- function(x, ...) {
  list(...)[varsToEval]
}

fun_broken2 <- function(x, ...) {
  mc <- as.list(match.call()[-1])
  lapply(mc[varsToEval], eval)
}

call_fun <- function(f = fun) {
  main <- "plot title"
  f(x, color = score, linetype = strand, main = main)
}

call_fun() # works
call_fun(fun_broken) # cannot find the quoted arguments
call_fun(fun_broken2) # cannot find the other arguments, like 'main'
