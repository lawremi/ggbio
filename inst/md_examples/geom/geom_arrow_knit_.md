---
layout: static
title: geom_arrow
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_arrow-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(imgur.key = "7733c9b660907f0975935cc9ba657413")
    opts_knit$set(upload = TRUE)
end.rcode-->

### Introduction
`geom_arrow` is lower level API for creating small arrows for interval data,
such as *GRanges* object.

### Objects
  * *GRanges*
  
### Usage
  upcomming
  
### Examples

Let's generate some simulated interval data and store it as *GRanges* object.

<!--begin.rcode message = FALSE, warning = FALSE, error = FALSE
set.seed(1)
N <- 100
library(ggbio)
library(GenomicRanges)
## =======================================
##  simmulated GRanges
## =======================================
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
end.rcode-->

Default `stat` is "stepping". 
<!--begin.rcode message = FALSE, warning = FALSE, error = FALSE
ggplot() + geom_arrow(gr)
end.rcode-->

Faceting and aesthetics mapping
<!--begin.rcode message = FALSE, warning = FALSE, error = FALSE
ggplot() + geom_arrow(gr, facets = sample ~ seqnames, color = "red")
end.rcode-->

`stat = "identity"` allows you assign customized `y` value.
<!--begin.rcode message = FALSE, warning = FALSE, error = FALSE
ggplot() + geom_arrow(gr, stat = "identity", aes(x = start, y = value,
                                        xend = end, yend = value))
end.rcode-->
