## @knitr maf
mut <- read.csv("/home/tengfei/Datas/circle/CRC9.genomic.v3.maf", sep = "\t")

## @knitr subset
idx <- mut$Tumor_Sample_Barcode == "CRC-1-Tumor" &   mut$Variant_Classification == "Missense"
crc1 <- mut[idx, 1:13]
write.csv(crc1, file = "~/Codes/svnrepos/devel/biovizBase/inst/extdata/crc1-missense.csv",
          row.names = FALSE)


## @knitr read.xlsx
library(xlsx)
rearr <- read.xlsx("/home/tengfei/Datas/circle/ng.936-S3.xlsx", sheetIndex = 1, rowIndex = 20:702)

## rearr.bk <- rearr
## rearr <- rearr.bk
## @knitr chr22
rearr <- rearr[!rearr$chr1 %in% c(23, 24) & !rearr$chr2 %in% c(23, 24), ]
write.csv(rearr, file = "~/Codes/svnrepos/devel/biovizBase/inst/extdata/crc-rearrangment.csv",
          row.names = FALSE)

dim(rearr)
print(object.size(rearr), unit = "Mb")


library(GenomicRanges)
gr1 <- with(rearr, GRanges(chr1, IRanges(pos1, width = 1)))
gr2 <- with(rearr, GRanges(chr2, IRanges(pos2, width = 1)))
nms <- colnames(rearr)
.extra.nms <- setdiff(nms, c("chr1", "chr2", "pos1", "pos2"))
values(gr1) <- rearr[,.extra.nms]
data("hg19Ideogram", package = "biovizBase")
hg19Ideo <- hg19Ideogram
.nms <- paste("chr", 1:22, sep = "")
hg19Ideo <- keepSeqlevels(hg19Ideogram, .nms)
mut.gr <- keepSeqlevels(mut.gr, .nms)
.nm.new <- as.character(1:22)
names(.nm.new) <- .nms
hg19Ideo <- renameSeqlevels(hg19Ideo, .nm.new)
mut.gr <- renameSeqlevels(mut.gr, .nm.new)

bidx <- start(mut.gr) <= seqlengths(hg19Ideo)[match(as.character(seqnames(mut.gr)),
              names(seqlengths(hg19Ideo)))]

mut.gr <- mut.gr[which(bidx)]

seqlengths(mut.gr) <- seqlengths(hg19Ideo)
data(hg19IdeogramCyto)
data("hg19IdeogramCyto", package = "biovizBase")
hg19cyto <- hg19IdeogramCyto
start(hg19cyto) <- start(hg19cyto) + 1
hg19cyto <- keepSeqlevels(hg19cyto, .nms)
hg19cyto <- renameSeqlevels(hg19cyto, .nm.new)
seqlengths(hg19cyto) <- seqlengths(hg19Ideo)


seqs <- as.character(seqnames(gr1))
.mx <- seqlengths(hg19Ideo)[seqs]
idx1 <- start(gr1) > .mx
seqs <- as.character(seqnames(gr2))
.mx <- seqlengths(hg19Ideo)[seqs]
idx2 <- start(gr2) > .mx

idx <- !idx1 & !idx2
gr1 <- gr1[idx]
seqlengths(gr1) <- seqlengths(hg19Ideo)

gr2 <- gr2[idx]
seqlengths(gr2) <- seqlengths(hg19Ideo)

##

values(gr1)$to.gr <- gr2
gr <- gr1

values(gr)$rearrangements <- ifelse(as.character(seqnames(gr))
                                    == as.character(seqnames((values(gr)$to.gr))),
                                    "intrachromosomal", "interchromosomal")


library(ggbio)
## make a subset

gr.crc1 <- gr[values(gr)$individual == "CRC-1"]
gr.crc1
values(gr.crc4[4:5])$to.gr

## fixme: radius for link not r
p1 <- ggplot() + layout_circle(gr.cur, geom = "link", linked.to = "to.gr",
                         aes(color = rearrangements), radius = 7.1) +
  ## layout_circle(gr.cur, geom = "rect", trackWidth = 1, radius = 9) +
  layout_circle(hg19Ideo, geom = "ideo", trackWidth = 2,
                         color = "steelblue", fill = "gray70") +
  layout_circle(hg19Ideo, geom = "text", trackWidth = 2,
                aes(label = seqnames),
                color = "white", size = 4) +
  layout_circle(hg19Ideo, geom = "scale", trackWidth = 0.5, radius = 12, size = 2.6) +
  opts(title = (unique(values(gr)$individual)), legend.position = "none") 


legendGrob(ggplotGrob(p1))
p = qplot(mpg, wt, data=mtcars, colour=cyl); 
g = ggplotGrob(p1)
gg = editGrob(getGrob(g, gPath("guide-box"), 
  grep=TRUE), vp=viewport())

p1 
## save the legend


## lst <- lapply(grl, function(gr.cur){
##   print(unique(values(gr.cur)$individual))
## p <- ggplot() + layout_circle(gr.cur, geom = "link", linked.to = "to.gr",
##                          aes(color = rearrangements), radius = 7.1) +
##   ## layout_circle(gr.cur, geom = "rect", trackWidth = 1, radius = 9) +
##                            layout_circle(hg19Ideo, geom = "ideo", trackWidth = 2,
##                          color = "steelblue", fill = "gray70") +
##   layout_circle(hg19Ideo, geom = "text", trackWidth = 2,
##                 aes(label = seqnames),
##                 color = "white", size = 4) +
##   layout_circle(hg19Ideo, geom = "scale", trackWidth = 0.5, radius = 12, size = 2.6) +
##   opts(title = (unique(values(gr.cur)$individual)), legend.position = "none") 
## })

grl <- split(gr, values(gr)$individual)
lst2 <- lapply(grl, function(gr.cur){
  print(unique(values(gr.cur)$individual))
cols <- RColorBrewer::brewer.pal(3, "Set2")[2:1]
  names(cols) <- c("interchromosomal", "intrachromosomal")
  ## values(gr.cur)$rearrangements <- factor(values(gr.cur)$rearrangements,
  ##                                         levels = c("interchromosomal", "intrachromosomal"))
  p <- ggplot() + layout_circle(gr.cur, geom = "link", linked.to = "to.gr",
                         aes(color = rearrangements), radius = 7.1) +
                           layout_circle(hg19Ideo, geom = "ideo", trackWidth = 1.5,
                         color = "gray70", fill = "gray70") +
                           scale_color_manual(values = cols)                           + 
  layout_circle(hg19Ideo, geom = "text", trackWidth = 2,
                aes(label = seqnames),
                color = "black", size = 3, radius = 11.5) +
  opts(title = (unique(values(gr.cur)$individual)), legend.position = "none") +
    opts(plot.margin = unit(rep(0, 4), "lines"))

})


ggsave("~/Desktop/cir-crc4.pdf")
grid.arrange(lst[[1]], lst[[2]], ncol = 2)
## one needs to provide the legend with a well-defined width
## legend=gTree(children=gList(leg), cl="legendGrob")
widthDetails.legendGrob <- function(x) unit(2, "cm")


## res.lst <- c(lst2, list(legend = gg))
## do.call(grid.arrange, res.lst)

l.g <- lapply(lst2, ggplotGrob)
square <- do.call(arrangeGrob, l.g)
pdf("~/Desktop/cir.pdf", 12, 10)
grid.arrange(square, gg, ncol = 2, widths = c(4/5, 1/5))
dev.off()



gr.crc1
## individule circular view for
gr.crc1

grl <- GenomicRangesList(gr.crc1,  gr.crc1, mut.gr, hg19Ideo,  hg19Ideo, hg19Ideo)
autoplot(grl, args = list(list(geom = "link" , linked.to = "to.gr", aes(color = rearrangements)),
                           list(geom = "point", aes(y = score, size = tumreads), color = "red"),
                           list(geom = "rect", fill = "steelblue",
                                color = "steelblue"),                
                           list(geom = "ideo", fill = "gray70"),
                           list(geom = "scale", size = 2),
                           list(geom = "text", aes(label  = seqnames), vjust = 0)),
         trackWidth = c(1, 10, 6, 4, 2, 7), radius = 30,
         trackSkip = c(1, 1, 1, 1, 0, 1),
         grid = c(F, T, F, F, F, F)) + scale_size(range = c(1, 2.5))

ggsave("~/Desktop/cir-single.pdf")



## exampels
## make a sample data?
library(GenomicRanges)
data(hg19Ideogram, package = "biovizBase")
obj <- hg19Ideogram
obj <- keepSeqlevels(obj, paste("chr", c(1:22, "X", "Y"), sep = ""))
## define a set of data points
set.seed(1)
seqs <- seqlengths(obj)
N <- 20
lst <- lapply(1:length(seqs), function(i){
  seq <- seqs[i]
  pos <- runif(N, max = seq)
  score <- rnorm(N)
  GRanges(names(seq), IRanges(pos, width = 100))
})
res <- do.call(c, lst)
width(res) <- round(rnorm(N, mean = 1e7, sd = 10))
## seqlengths *MUST* be consistency
seqlengths(res) <- seqlengths(obj)
## adding some scores
values(res)$score <- rnorm(N)

## let's adding a link!
## simulate some data
lst <- lapply(1:length(seqs), function(i){
  seq <- seqs[i]
  pos <- runif(N, max = seq)
  score <- rnorm(N)
  GRanges(names(seq), IRanges(pos, width = 1))
})
to.gr <- do.call(c, lst)
## don't forget to add seqlengths for GRanges object
seqlengths(to.gr) <- seqlengths(obj)
width(to.gr) <- round(rnorm(N, mean = 1e3, sd = 10))
to.gr <- to.gr[sample(1:length(to.gr), size = length(to.gr))]
values(res)$to <- to.gr

## edit text label
values(obj)$label <- sub("chr", "", as.character(seqnames(obj)))
library(Hmisc)
library(ggbio)
ggplot() +
  layout_circle(res, geom = "link", rank = 0, linked.to = "to",
                         alpha = 0.4, aes(color = score) ) +
  layout_circle(res, geom = "ideogram", fill = "gray80", rank = 1) +
  layout_circle(obj, geom = "text", aes(label = label), rank = 2) +
  layout_circle(res, aes(y = score), geom = "point", rank = 3, size = 0.5) +
    layout_circle(res, aes(y = score), geom = "line", rank = 4) +
       layout_circle(res, geom = "rect", rank = 5) +
          layout_circle(res, geom = "segment", rank = 6) +
         layout_circle(res, geom = "scale", rank = 7)


autoplot(res, geom = "ideogram", layout = "circle", fill = "blue")

## NULL
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
## get seqlengths
obj <- hg19IdeogramCyto
library(GenomicRanges)
seqlengths(obj) <- seqlengths(hg19Ideogram)[names(seqlengths(obj))]
values(obj)$score <- rnorm(length(obj), 10, 5)
values(obj)$to.gr <- obj[sample(1:length(obj), size = length(obj))]
ggplot() + layout_circle(obj, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(obj, geom = "bar", radius = 11, trackWidth = 5, aes(y = score)) +
   layout_circle(obj, geom = "link", radius = 6, trackWidth = 1, linked.to = "to.gr",
                 n = 10)


N <- 100
require(ggbio)
require(GenomicRanges)
## @knitr simul
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
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
    layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6, trackWidth = 1)
