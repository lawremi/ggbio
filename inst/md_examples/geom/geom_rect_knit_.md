---
layout: static
title: geom_rect
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_rect-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(imgur.key = "7733c9b660907f0975935cc9ba657413")
    opts_knit$set(upload = TRUE)
end.rcode-->

### Introduction
`geom_rect` is lower level API for creating rectangles for interval data,
such as *GRanges* object.

### Objects
  * *GRanges*
  * *data.frame* , just like ggplot2::geom_rect
  
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

Default is use stat stepping, which laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting, it's a very rough exploration as first step for some interval data.

<!--begin.rcode message = FALSE, warning = FALSE
## =======================================
##  default
## =======================================
ggplot() + geom_rect(gr)
end.rcode-->

Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.

<!--begin.rcode message = FALSE, warning = FALSE
## =======================================
##  facetting and aesthetics
## =======================================
ggplot() + geom_rect(gr, facets = sample ~ seqnames, aes(color = strand, fill = strand))
end.rcode-->

Stat "identity" allows you to specify a y value to use as y-axis instead of
default stepping level.

<!--begin.rcode message = FALSE, warning = FALSE
## =======================================
##  stat:identity
## =======================================
ggplot() + geom_rect(gr, stat = "identity", aes(y = value))
end.rcode-->

`group` make sure grouped intervals are on the same levels when `stat =
"stepping"`,  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.

<!--begin.rcode message = FALSE, warning = FALSE
## =======================================
##  stat:stepping
## =======================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, group = pair))
end.rcode-->

`group.selfish` force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

<!--begin.rcode message = FALSE, echo = FALSE, warning = FALSE
## =======================================
##  group.selfish 
## =======================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, group = pair), group.selfish = FALSE)
end.rcode-->

