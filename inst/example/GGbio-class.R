library(ggbio)
p <- ggplot(data = mtcars)
## obj.new <- obj.new + layout_circle()

p  <- p + geom_point(aes(x = mpg, y = wt), color = "red")

p
class(p)

N <- 100
library(GenomicRanges)
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

## doesn't pass gr to the ggplot
ggplot() + layout_circle(gr, geom = "ideo", fill = "gray70", radius = 7, trackWidth = 3) +
  layout_circle(gr, geom = "bar", radius = 10, trackWidth = 4, aes(fill = score, y = score)) +
  layout_circle(gr, geom = "point", color = "red", radius = 14,
                trackWidth = 3, grid = TRUE, aes(y = score)) +
  layout_circle(gr, geom = "link", linked.to = "to.gr", radius = 6,
                trackWidth = 1)

p <- ggplot(gr) + layout_circle() + geom_bar(aes(fill = score, y = score))
p
library(grid)
p <- ggplot() + geom_rect(gr)

autoplot(gr)
class(p)
p
p + xlim(100, 200)
p <- ggplot(gr) + geom_rect(which = gr)
p
class(p)


gr1 <- GRanges("chr1", IRanges(1, 3))
gr2 <- GRanges("chr1", IRanges(c(2, 4, 1), c(3, 5, 2)))
countOverlaps(gr2, gr1, type = "within")

## test cache ability

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
data(genesymbol, package = "biovizBase")
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
library(ggbio)
p1 <- autoplot(txdb, which = genesymbol["ALDOA"], names.expr = "tx_name")

ggbio:::cached_which(p1)
ggbio:::cached_item(p1)

gr1 <- genesymbol["ALDOA"]
gr1
gr2 <- GRanges("chr16", IRanges(30074491, 30075733))
gr3 <- GRanges("chr12", IRanges(30074491, 30081733))

print(ggbio:::needCache(p1, gr2))
p11 = p1
p11
p1
p1 + xlim(gr1)
p3 <- p1 + xlim(gr3)
p3
class( xlim(gr3))
ggbio:::cached(p1)
res <- xlim(gr2)
res <- xlim_car(res)





