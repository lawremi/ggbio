## @knitr load_mut
crc1 <- system.file("extdata", "crc1-missense.csv", package = "biovizBase")
crc1 <- read.csv(crc1)
head(crc1)

## @knitr mut.gr
library(GenomicRanges)
mut.gr <- with(crc1,GRanges(Chromosome, IRanges(Start_position, End_position),
                            strand = Strand))
values(mut.gr) <- subset(crc1, select = -c(Start_position, End_position, Chromosome))

## @knitr transformDfToGr
library(biovizBase)
mut.gr <- transformDfToGr(crc1, seqnames = "Chromosome", start = "Start_position",
                          end = "End_position", strand = "Strand")

## @knitr seqlengths
seqlengths(mut.gr)

## @knitr get_seqlengths_ucsc
## supported genomes, use db column
library(rtracklayer)
head(ucscGenomes())
seqs.gr <- GRangesForUCSCGenome("hg19")
seqs <- seqlengths(seqs.gr)


## @knitr get_seqlengths_ideo
seqs.gr <- getIdeogram("hg19", cytoband  = FALSE)
seqs <- seqlengths(seqs.gr)



## @knitr get_seqlengths_data
data("hg19Ideogram", package = "biovizBase")
seqs <- seqlengths(hg19Ideogram)

## @knitr subset_chr
chr.sub <- paste("chr", 1:22, sep = "")
chr.sub

## @knitr keep_seqlevels
head(seqlengths(hg19Ideogram), 22)
head(seqlengths(keepSeqlevels(hg19Ideogram, chr.sub)), 22)

## @knitr levels_mut
seqlevels(mut.gr) <- c(chr.sub, "chrX")
mut.gr <- keepSeqlevels(mut.gr, chr.sub)
seqs.sub <- seqs[chr.sub]
seqs.sub

## @knitr remove_wrong
bidx <- end(mut.gr) <= seqs.sub[match(as.character(seqnames(mut.gr)),
              names(seqs.sub))]
mut.gr <- mut.gr[which(bidx)]


## @knitr assign_seqlengths
seqlengths(mut.gr) <- seqs.sub
head(mut.gr)

## @knitr rename_seqlevels
new.names <- as.character(1:22)
names(new.names) <- paste("chr", new.names, sep = "")
new.names
mut.gr.new <- renameSeqlevels(mut.gr, new.names)
head(mut.gr.new)

## @knitr sessionInfo
sessionInfo()
