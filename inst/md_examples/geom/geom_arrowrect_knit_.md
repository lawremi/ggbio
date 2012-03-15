---
layout: static
title: geom_arrowrect
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_arrowrect-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(imgur.key = "7733c9b660907f0975935cc9ba657413")
    opts_knit$set(upload = TRUE)
end.rcode-->

### Introduction
`geom_arrowrect` is lower level API for creating a 5-side polygon, like a
rectangle with arrow head, for interval data such as *GRanges* object. It map
the strand information automatically back to arrow head direction, when the
strand is "\*", it's just rectangle.

### Objects
  * *GRanges*
  
### Usage
  upcomming
  
### Examples

Let's generate some simulated interval data and store it as *GRanges* object.

<!--begin.rcode message = FALSE, warning = FALSE
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

Default `stat` is "stepping"

<!--begin.rcode message = FALSE, warning = FALSE
ggplot() + geom_arrowrect(gr)
end.rcode-->

Facetting and aesthetics mapping
<!--begin.rcode message = FALSE, warning = FALSE
ggplot() + geom_arrowrect(gr, facets = sample ~ seqnames, fill = "red")
end.rcode-->

`stat` "identity" is used for mapping y axis to a variable, and argument
`rect.height` is used to control the width of the arrow body. 

<!--begin.rcode message = FALSE, warning = FALSE
ggplot() + geom_arrowrect(gr, stat = "identity", aes(y = value), rect.height = 0.1)
end.rcode-->
