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

