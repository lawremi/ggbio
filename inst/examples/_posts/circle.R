## @knitr mut_processing
crc1 <- system.file("extdata", "crc1-missense.csv", package = "biovizBase")
crc1 <- read.csv(crc1)
library(GenomicRanges)
mut.gr <- with(crc1,GRanges(Chromosome, IRanges(Start_position, End_position),
                            strand = Strand))
values(mut.gr) <- subset(crc1, select = -c(Start_position, End_position, Chromosome))
data("hg19Ideogram", package = "biovizBase")
seqs <- seqlengths(hg19Ideogram)
## subset_chr
chr.sub <- paste("chr", 1:22, sep = "")
## levels tweak
seqlevels(mut.gr) <- c(chr.sub, "chrX")
mut.gr <- keepSeqlevels(mut.gr, chr.sub)
seqs.sub <- seqs[chr.sub]
## remove wrong position
bidx <- end(mut.gr) <= seqs.sub[match(as.character(seqnames(mut.gr)),
              names(seqs.sub))]
mut.gr <- mut.gr[which(bidx)]
## assign_seqlengths
seqlengths(mut.gr) <- seqs.sub
## reanme to shorter names
new.names <- as.character(1:22)
names(new.names) <- paste("chr", new.names, sep = "")
new.names
mut.gr.new <- renameSeqlevels(mut.gr, new.names)
head(mut.gr.new)


## @knitr ideo
hg19Ideo <- hg19Ideogram
hg19Ideo <- keepSeqlevels(hg19Ideogram, chr.sub)
hg19Ideo <- renameSeqlevels(hg19Ideo, new.names)
head(hg19Ideo)


## @knitr lower-ideo-track
library(ggbio)
p <- ggplot() + layout_circle(hg19Ideo, geom = "ideo", fill = "gray70",
                              radius = 30, trackWidth = 4)
p
## @knitr lower-scale-track
p <- p + layout_circle(hg19Ideo, geom = "scale", size = 2, radius = 35, trackWidth = 2)
p

## @knitr lower-text-track
p <- p + layout_circle(hg19Ideo, geom = "text", aes(label = seqnames), vjust = 0,
                       radius = 38, trackWidth = 7)
p

## @knitr lower-mut-track
p <- p + layout_circle(mut.gr, geom = "rect", color = "steelblue",
                       radius = 23 ,trackWidth = 6)
p


## @knitr links
rearr  <- read.csv(system.file("extdata", "crc-rearrangment.csv", package = "biovizBase"))
## start position
gr1 <- with(rearr, GRanges(chr1, IRanges(pos1, width = 1)))
## end position
gr2 <- with(rearr, GRanges(chr2, IRanges(pos2, width = 1)))
## add extra column
nms <- colnames(rearr)
.extra.nms <- setdiff(nms, c("chr1", "chr2", "pos1", "pos2"))
values(gr1) <- rearr[,.extra.nms]
## remove out-of-limits data
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

## @knitr link-data
values(gr1)$to.gr <- gr2
## rename to gr
gr <- gr1

## @knitr rearr
values(gr)$rearrangements <- ifelse(as.character(seqnames(gr))
                                    == as.character(seqnames((values(gr)$to.gr))),
                                    "intrachromosomal", "interchromosomal")

## @knitr subset-crc-1
gr.crc1 <- gr[values(gr)$individual == "CRC-1"]

## @knitr lower-point-track
p <- p + layout_circle(gr.crc1, geom = "point", aes(y = score, size = tumreads), color = "red",
                       radius = 12 ,trackWidth = 10, grid = TRUE) +
  scale_size(range = c(1, 2.5))
p

## @knitr lower-link-track
p <- p + layout_circle(gr.crc1, geom = "link", linked.to = "to.gr", aes(color = rearrangements),
                       radius = 10 ,trackWidth = 1)
p


## @knitr autoplot
grl <- GenomicRangesList(gr.crc = gr.crc1,  gr.crc = gr.crc1,
                         mut.gr = mut.gr,
                         hg19Ideo = hg19Ideo,
                         hg19Ideo = hg19Ideo,
                         hg19Ideo = hg19Ideo)

autoplot(grl, args = list(list(geom = "link" , linked.to = "to.gr", aes(color = rearrangements)),
                           list(geom = "point", aes(y = score, size = tumreads), color = "red"),
                           list(geom = "rect", fill = "steelblue",
                                color = "steelblue"),                
                           list(geom = "ideo", fill = "gray70"),
                           list(geom = "scale", size = 2),
                           list(geom = "text", aes(label  = .ori.seqnames), vjust = 0)),
         trackWidth = c(1, 10, 6, 4, 2, 7), radius = 30,
         trackSkip = c(1, 1, 1, 1, 0, 1),
         grid = c(F, T, F, F, F, F)) + scale_size(range = c(1, 2.5))

## @knitr single-arr
cols <- RColorBrewer::brewer.pal(3, "Set2")[2:1]
names(cols) <- c("interchromosomal", "intrachromosomal")

p0 <- ggplot() + layout_circle(gr.crc1, geom = "link", linked.to = "to.gr",
                              aes(color = rearrangements), radius = 7.1) +
  layout_circle(hg19Ideo, geom = "ideo", trackWidth = 1.5,
                color = "gray70", fill = "gray70") +
  scale_color_manual(values = cols)  
p0

## @knitr legend
library(gridExtra)
g = ggplotGrob(p0)
gg = editGrob(getGrob(g, gPath("guide-box"), 
  grep=TRUE), vp=viewport())


## @knitr arrangement
grl <- split(gr, values(gr)$individual)
## need "unit", load grid
library(grid)
lst <- lapply(grl, function(gr.cur){
  print(unique(as.character(values(gr.cur)$individual)))
  cols <- RColorBrewer::brewer.pal(3, "Set2")[2:1]
  names(cols) <- c("interchromosomal", "intrachromosomal")
  p <- ggplot() + layout_circle(gr.cur, geom = "link", linked.to = "to.gr",
                         aes(color = rearrangements), radius = 7.1) +
                           layout_circle(hg19Ideo, geom = "ideo", trackWidth = 1.5,
                         color = "gray70", fill = "gray70") +
                           scale_color_manual(values = cols)  + 
     opts(title = (unique(values(gr.cur)$individual)), legend.position = "none") +
    opts(plot.margin = unit(rep(0, 4), "lines"))
})
l.g <- lapply(lst, ggplotGrob)
square <- do.call(arrangeGrob, l.g)


## @knitr 9-circle
grid.arrange(square, gg, ncol = 2, widths = c(4/5, 1/5))

## @knitr sessionInfo
sessionInfo()

## @knitr NULL
example(GRanges)

ggplot() + layout_circle(gr, geom = "ideo")
ggplot() + layout_circle(gr, geom = "bar")
