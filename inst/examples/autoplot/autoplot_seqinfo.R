library(ggbio)
data(hg19Ideogram)
seqs <- seqinfo(hg19Ideogram)
class(seqs)
autoplot(seqs[paste("chr", c(1:10, 22:11), sep = "")])
autoplot(seqs["chr1"])
autoplot(seqs["chr1"], FALSE)


