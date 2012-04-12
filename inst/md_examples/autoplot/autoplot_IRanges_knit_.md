---
layout: static
title: autoplot_IRanges
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_IRanges-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_IRanges.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`autoplot` for *IRanges* is used to visualize simple interval data with element
data together.

Supported geom designed specifically for *IRanges*, including "rect", "chevron",
"alignment", "arrowrect", "arrow", "segment", "arch", and special statistical
transformation contains "identity", "coverage", "stepping", "aggregate",
"table", "gene", "mismatch". And they are implemented in lower API, such as
`geom_alignment` and `stat_coverage`. If you pass other `geom` and `stat` other
than those ones, it first coerces a *IRanges* into a `data.frame` object
*together* with extra element meta data, and added to `data.frame`.


Inside, `autoplot` will choose the best choice for your combination of `geom`
and `stat`.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *IRanges*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->
  
Let's generate some simulated interval data and store it as *IRanges*
object. and add some element meta data.
<!--begin.rcode simul, message = TRUE, warning = FALSE
end.rcode-->

`autoplot` will coerce *IRanges* together with its element meta data, so
aesthetics mapping works for those extra information too.
<!--begin.rcode exp,  message = FALSE, warning = FALSE
end.rcode-->

