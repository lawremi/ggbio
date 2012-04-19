## @knitr load
library(ggbio)
library(GenomicRanges)

## @knitr simul
set.seed(123)
gr.b <- GRanges(seqnames = "chr1", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b2 <- GRanges(seqnames = "chr2", IRanges(start = seq(1, 100, by = 10),
                  width = sample(4:9, size = 10, replace = TRUE)),
                score = rnorm(10, 10, 3), value = runif(10, 1, 100))
gr.b <- c(gr.b, gr.b2)
## default use score as y

## @knitr bar
ggplot() + geom_bar(gr.b, aes(fill = value))
ggplot() + geom_bar(gr.b,  aes(y = value))
## equal to
autoplot(gr.b, geom = "bar")

