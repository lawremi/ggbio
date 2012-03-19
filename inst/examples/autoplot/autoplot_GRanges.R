library(GenomicRanges)
## library(devtools)
## load_all("~/Codes/gitrepos/ggbio")
##  GRanges
set.seed(1)
N <- 1000

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

idx <- sample(1:length(gr), size = 200)
## ----------------------------------------------------------------------
autoplot(gr)
## FIXME:
autoplot(gr, geom = "point", aes(y = score))

autoplot(gr, fill = "red")
autoplot(gr, aes(fill = value))
autoplot(gr, facets = sample ~ seqnames)
autoplot(gr, geom = "chevron", facets = sample ~ seqnames, aes(color = value), size = 3)
myfun <- function(data, ...){
  autoplot(data, ...)
}
myfun(gr, geom = "chevron", facets = sample ~ seqnames, aes(color = value), size = 3)

autoplot(gr[idx], geom = "5poly", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)

autoplot(gr, geom = "segment", facets = sample ~ seqnames)

autoplot(gr[idx], geom = "alignment", facets = sample ~ seqnames)
autoplot(gr[idx], geom = "alignment", aes(group = pair))
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)

autoplot(gr, stat = "coverage", facets = sample ~ seqnames)
autoplot(gr, stat = "coverage", facets = sample ~ seqnames, geom = "point")

autoplot(gr, stat = "table")
autoplot(gr, stat = "table", rect.height = 0.2)
autoplot(gr, stat = "table", rect.height = 0.2, fill = "red")

autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)
autoplot(gr, stat = "aggregate", y = "value")
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot")
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot", facets = sample ~ seqnames)
## FIXME: aggregate error
autoplot(gr, stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 2)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 10)
autoplot(gr[idx], stat = "aggregate", y = "value", facets = sample ~ seqnames, window = 30,
         geom = "histogram")

