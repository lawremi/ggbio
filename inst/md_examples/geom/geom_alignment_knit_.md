---
layout: static
title: geom_alignment
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_alignment-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/geom/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/geom"
    fl<- file.path(dir.path, "geom_alignment.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`geom_alignment` is lower level API for creating alignemtns for interval data,
such as *GRanges*  and even more native *GRangesList* object. 

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


Default is use stat stepping, which laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting, it's a very rough exploration as first step for some interval data.


**NOTICE** default groupping intervals based on stepping levels, which doesn't
  make sense in mose cases! the chevron connect them doesn't make too much sense
  too, so make sure you group them based on some meaningful values, like
  transcript id.

<!--begin.rcode default,  message = FALSE, warning = FALSE
end.rcode-->

Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.

<!--begin.rcode facet_aes, message = FALSE, warning = FALSE
end.rcode-->


`group` make sure grouped intervals are on the same levels when `stat =
"stepping"`,  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.

<!--begin.rcode stat:stepping, message = FALSE, warning = FALSE
end.rcode-->

`group.selfish` force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

<!--begin.rcode group.selfish, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

We allow you to change main geoms and gaps geoms too, you can always use
eligible geoms for intervals data, for example, `geom_arrowrect` could be
extracted to name "arrowrect" and passed to argument `main.geom`, so does
gap.geom.

<!--begin.rcode main_gap,message = FALSE, warning = FALSE
end.rcode-->



