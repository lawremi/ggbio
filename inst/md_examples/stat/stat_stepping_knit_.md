---
layout: static
title: stat_stepping
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "stat_stepping-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/stat/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/stat"
    fl<- file.path(dir.path, "stat_stepping.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`stat_stepping` is lower level API laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting. It uses `add_stepping` function in *biovizBase* package to compute the
stepping levels for *GRanges* object.

### Objects
  * *GRanges*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->
  
  Let's generate some simulated interval data and store it as *GRanges* object.
<!--begin.rcode simul, message = FALSE, warning = FALSE
end.rcode-->


Default is use `geom_rect`, it's a very rough exploration as first step for some interval data.

<!--begin.rcode default,  message = FALSE, warning = FALSE
end.rcode-->

Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.

<!--begin.rcode facet_aes, message = FALSE, warning = FALSE
end.rcode-->

Use different geom, such as `segment`.
<!--begin.rcode geom_segment, message = FALSE, warning = FALSE
end.rcode-->

geom `alignment`
<!--begin.rcode geom_alignment, message = FALSE, warning = FALSE
end.rcode-->

geom `alignment` with group 
<!--begin.rcode geom_alignment_group, message = FALSE, warning = FALSE
end.rcode-->

