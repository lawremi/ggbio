---
layout: static
title: geom_chevron
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "geom_chevron-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/geom/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/geom"
    fl<- file.path(dir.path, "geom_chevron.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`geom_chevron` is is lower level API for creating chevrons for interval data, such as GRanges object, it could be used to visuzalize introns or splicing.

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
plotting.

<!--begin.rcode default,  message = FALSE, warning = FALSE
end.rcode-->

Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.

<!--begin.rcode facet_aes, message = FALSE, warning = FALSE
end.rcode-->

Stat "identity" allows you to specify a y value to use as y-axis instead of
default stepping level.

<!--begin.rcode stat:identity, message = FALSE, warning = FALSE
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

`offset` controls the height of the chevron, notice the default rectangle height
is always 0.4*2.

<!--begin.rcode offset, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

<!--begin.rcode offset:default, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

<!--begin.rcode offset:0, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

<!--begin.rcode offset:0.4, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

`chevron.height` is useful to rescale the offset when you specify the offset as
one the the variables.
<!--begin.rcode chevron.height:default, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->

<!--begin.rcode chevron.height, message = FALSE, echo = FALSE, warning = FALSE
end.rcode-->


