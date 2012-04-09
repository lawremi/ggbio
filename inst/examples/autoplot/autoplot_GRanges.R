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
idx <- sample(1:length(gr), size = 200)

## @knitr default
autoplot(gr)



## @knitr geom/aes/facet
autoplot(gr, geom = "point", aes(y = score))
autoplot(gr, fill = "red")
autoplot(gr, aes(fill = value))
autoplot(gr, facets = sample ~ seqnames)
autoplot(gr[idx], geom = "chevron", offset = 1)
autoplot(gr[idx], geom = "arrowrect", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

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




data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
## just for now
## seqlths <- seqlengths(hg19Ideogram)[chrs]
seqlths <- round(seqlths/1000)
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


## processing the name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
gr.snp
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue))

## sort and add seqlengths
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
gr.snp
autoplot(gr.snp, coord = "genome", space.skip = 0)
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue))
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), space.skip = 0)

nms <- names(seqlengths(gr.snp))
seqs <- runif(length(nms), 1e5, 1e7)
names(seqs) <- nms
seqlengths(gr.snp) <- seqs
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue))
