---
layout: static
title: geom_arch
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_arch-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/geom/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/geom"
    fl<- file.path(dir.path, "geom_arch.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`geom_arch` is lower level API for creating arches for interval data,
such as *GRanges* object. It could be used for showing splicing events or any
defined interactions in linear view.

A special thanks to **Jennifer Change**, a PhD student in
Iowa state univeristy, who initialize the first visualization of arches in
**ggbio** and distributed most of her code into this package.

### Objects
  * *GRanges*
  * *data.frame* 
  
### Usage
  upcomming
  
### Examples

<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Let's generate some simulated interval data and store it as *GRanges* object.

<!--begin.rcode simul, message = FALSE, warning = FALSE
end.rcode-->

Default use equal height to show the arches, for each interval, it's being
connect by two ends, so make sure if you want to connect two exons, you have to
get gaps intervals to show the links between exons. 

<!--begin.rcode default, message = FALSE, warning = FALSE
end.rcode-->

Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.

<!--begin.rcode facet_aes, message = FALSE, warning = FALSE
end.rcode-->


