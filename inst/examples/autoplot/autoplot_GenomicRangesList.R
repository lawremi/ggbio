## make a sample data?
library(ggbio)
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
grl <- GenomicRangesList(res, res, obj, res)

autoplot(grl, args =
         list(list(geom = "link", linked.to = "to", alpha = 0.4, aes(color = score)),
              list(geom = "ideogram", fill = "gray80"),
              list(geom = "text", aes(label= label)),
              list(geom = "point", aes(y = score))))

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
