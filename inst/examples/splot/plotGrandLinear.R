library(GenomicRanges)

IRanges(1e10, width = 1)
IRanges(as.double(1e10), width = 1)

library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
seqlths <- seqlengths(hg19Ideogram)[chrs]
seqlths <- round(seqlths/1000)
set.seed(1)
nchr <- length(chrs)
nsnps <- 100
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
## processing the name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)


## compact view
## no facet by samples, but make sure you want it that way
## default is two color
plotGrandLinear(gr.snp, aes(y = pvalue))
plotGrandLinear(gr.snp, y = pvalue, space.skip = 0.1)

## sort chromosomes
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
plotGrandLinear(gr.snp, y = pvalue)

## tweak with seqlengths
nms <- names(seqlengths(gr.snp))
seqs <- rep(3e5, length(nms))
names(seqs) <- nms
seqlengths(gr.snp) <- seqs
plotGrandLinear(gr.snp, y = pvalue)

## facet by samples, comparison across groups
plotGrandLinear(gr.snp, y = pvalue, 
                facets = group ~ .,  color.type = "twocolor")
## change two color
plotGrandLinear(gr.snp, y = pvalue, 
                facet = group ~ .,  color.type = "twocolor",
                two.color = c("red", "blue"))



## geom line
plotGrandLinear(gr.snp, y = pvalue,
                geom = "line", facet = group ~ .)  


## add size and change color
plotGrandLinear(gr.snp, y = pvalue, size = 5,
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, aes(size = pvalue),
                facet = group ~ .,  color.type = "seqnames")

plotGrandLinear(gr.snp, y = pvalue, color = group,
                facet = group ~ .,
                color.type = "identity")

plotGrandLinear(gr.snp, y = pvalue, color = "blue", facet = group ~ .,
                color.type = "identity")
