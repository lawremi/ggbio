---
layout: static
title: autoplot_GRanges
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_GRanges-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_GRanges.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`autoplot` for *GRanges* object is designed to be most general plot API in
*ggbio* package. *GRanges* is most suitable data structure for storing interval
data with medata data, which could be used for representing a set of short reads
or genomic features. 

Supported geom designed specifically for *GRanges*, including "rect", "chevron",
"alignment", "arrowrect", "arrow", "segment", "arch", and special statistical
transformation contains "identity", "coverage", "stepping", "aggregate",
"table", "gene", "mismatch". And they are implemented in lower API, such as
`geom_alignment` and `stat_coverage`. If you pass other `geom` and `stat` other
than those ones, it first use `fortify` method in *ggbio* to coerce a *GRanges*
into a `data.frame` object. And a new variable `midpoint` is created and added
to final `data.frame` to be used to mapped as `x`. So you can use it as other
*ggplot2* API.

Inside, `autoplot` will choose the best choice for your combination of `geom`
and `stat`.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *GRanges*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->
  
Let's generate some simulated interval data and store it as *GRanges* object.
<!--begin.rcode simul, message = TRUE, warning = FALSE
end.rcode-->

default is use geom "rect".
<!--begin.rcode default,  message = FALSE, warning = FALSE
end.rcode-->

Facetting, some combination of geom/stat
<!--begin.rcode geom/aes/facet, message = FALSE, warning = FALSE
end.rcode-->

Group need to be specified in `aes()` use aesthetics `group`, this help to
assign grouped intervals showing on the same y level, especially , when you use
`geom` *alignment*, gaps will be created based on group information and shown on
the plot. A minimal example is shown in the following chunks.
<!--begin.rcode group, message = FALSE, warning = FALSE
end.rcode-->

more example
<!--begin.rcode group-more, message = FALSE, warning = FALSE
end.rcode-->


Faceted by strand help you understand coverage from different sequencing direction. 
<!--begin.rcode facet:strand, message = FALSE, warning = FALSE
end.rcode-->

More stats
<!--begin.rcode stat, message = FALSE, warning = FALSE
end.rcode-->

*New* coordinate transformation "genome" will transform a *GRanges* object into
a genome space, align them up based on `seqlevel` orders. This transformation
allows you to add `seqlengths` to your *GRanges* object to produce a fixed
width. and add buffer in between by specifying `space.skip`. This transformation
is useful for grand linear view as Manhattan plot or circular view.
<!--begin.rcode coord:genome, message = FALSE, warning = FALSE
end.rcode-->

Layout circle is another special transformation.




