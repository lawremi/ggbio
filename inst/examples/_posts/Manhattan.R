## @knitr load
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
library(GenomicRanges)

## @knitr simul_gr
library(biovizBase)
gr <- GRanges(rep(c("chr1", "chr2"), each = 5),
              IRanges(start = rep(seq(1, 100, length = 5), times = 2),
                      width = 50))
autoplot(gr)

## @knitr coord:genome
autoplot(gr, coord = "genome")
autoplot(gr, coord = "genome", aes(fill = seqnames))
gr.t <- transformToGenome(gr)
head(gr.t)

## @knitr is
is_coord_genome(gr.t)
metadata(gr.t)$coord


## @knitr simul_snp
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
seqlths <- seqlengths(hg19Ideogram)[chrs]
set.seed(1)
nchr <- length(chrs)
nsnps <- 100
gr.snp <- GRanges(rep(chrs,each=nsnps),
                  IRanges(start =
                          do.call(c, lapply(chrs, function(chr){
                            N <- seqlths[chr]
                            runif(nsnps,1,N)
                          })), width = 1),
                  SNP=sapply(1:(nchr*nsnps), function(x) paste("rs",x,sep='')),
                  pvalue =  -log10(runif(nchr*nsnps)),
                  group = sample(c("Normal", "Tumor"), size = nchr*nsnps,
                    replace = TRUE)
                  )

## @knitr shorter
seqlengths(gr.snp)
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)



## @knitr unorder
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr sort
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr with_seql
names(seqlths) <- gsub("chr", "", names(seqlths))
seqlengths(gr.snp) <- seqlths[names(seqlengths(gr.snp))]
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0.01)

## @knitr line
autoplot(gr.snp, coord = "genome", geom = "line", aes(y = pvalue, group = seqnames,
                                     color = seqnames))



## @knitr plotGrandLinear
plotGrandLinear(gr.snp, aes(y = pvalue))

## @knitr morecolor
plotGrandLinear(gr.snp, aes(y = pvalue, color = seqnames))
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue"))
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue", "red"))
plotGrandLinear(gr.snp, aes(y = pvalue), color = "red")

## @knitr cutoff
plotGrandLinear(gr.snp, aes(y = pvalue), cutoff = 3, cutoff.color = "blue", cutoff.size = 4)

## @knitr cutoff-low
plotGrandLinear(gr.snp, aes(y = pvalue)) + geom_hline(yintercept = 3, color = "blue", size = 4)

## @knitr longer
## let's make a long name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- paste("chr00000", nms, sep = "")
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)

## @knitr rotate
plotGrandLinear(gr.snp, aes(y = pvalue)) + opts(axis.text.x=theme_text(angle=-90, hjust=0))

## @knitr sessionInfo
sessionInfo()

## @knitr NULL
p1 <- autoplot(gr.snp, aes(y = pvalue), geom = "point")
p2 <- plotGrandLinear(gr.snp, aes(y = pvalue))
pdf("~/Desktop/man.pdf", 13, 10)
grid.arrange(p1, p2)
dev.off()

