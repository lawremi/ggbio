---
layout: static
title: stat_identity
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "stat_identity-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/stat/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/stat"
    fl<- file.path(dir.path, "stat_identity.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`stat_identity` is coerce a *GRanges* object to a data.frame inside with new
variable `midpoint` which is `(start + end)/2`. `stat_identity` allows you to
treat a *GRanges* object as a *data.frame*, and use all supported geom in
*ggplot2* and *ggbio*.


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

Geom *point*.
<!--begin.rcode geom_point_start, message = FALSE, warning = FALSE
end.rcode-->

Geom *point*, with x set to `midpoint`, which doesn't exists in original data.
<!--begin.rcode geom_point_midpoint, message = FALSE, warning = FALSE
end.rcode-->

Geom *rect*, need to specify `xmin`, `xmax`, `ymin`, `ymax`.
<!--begin.rcode geom_rect_all, message = FALSE, warning = FALSE
end.rcode-->

Geom *rect*, or just specify a `y` use default for boundary.
<!--begin.rcode geom_rect_y, message = FALSE, warning = FALSE
end.rcode-->

Geom *line*
<!--begin.rcode geom_line, message = FALSE, warning = FALSE
end.rcode-->

Geom *segment*
<!--begin.rcode geom_segment, message = FALSE, warning = FALSE
end.rcode-->



