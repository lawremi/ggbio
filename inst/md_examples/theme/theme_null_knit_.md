---
layout: static
title: theme_null
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "theme_null-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/theme/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/theme"
    fl<- file.path(dir.path, "theme_null.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`theme_null` is designed for creating *blank* or *null* theme. This return a
option list like normal `theme_bw` in *ggplot2* package.

### Usage
  upcomming
  
### Examples
Load packages
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Simulate an inteval set.
<!--begin.rcode simul, message = FALSE, warning = FALSE
end.rcode-->


Default theme is with gray background, it's actually a preset theme in
*ggplot2*, called `theme_gray`.
<!--begin.rcode default, message = FALSE, warning = FALSE
end.rcode-->

Compare to `theme_gray`, here is how we make blank theme with `theme_null`.
<!--begin.rcode theme_null, message = FALSE, warning = FALSE
end.rcode-->
