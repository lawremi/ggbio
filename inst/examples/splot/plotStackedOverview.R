library(GenomicRanges)
data(hg19IdeogramCyto, package = "biovizBase")
## make shorter and clean labels
old.chrs <- seqnames(seqinfo(hg19IdeogramCyto))
new.chrs <- gsub("chr", "", old.chrs)
names(new.chrs) <- old.chrs
new.ideo <- renameSeqlevels(hg19IdeogramCyto, new.chrs)
p <- plotStackedOverview(new.ideo, cytoband = TRUE)
print(p)
## reorder the levels
new.ideo <- keepSeqlevels(new.ideo,  c(1:22, "X", "Y"))
## with cytoband
p <- plotStackedOverview(new.ideo, cytoband = TRUE)
print(p)
## with nocytoband
p <- plotStackedOverview(new.ideo, cytoband = FALSE)
print(p)
