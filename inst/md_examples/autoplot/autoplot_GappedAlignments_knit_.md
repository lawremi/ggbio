---
layout: static
title: autoplot_GappedAlignments
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_GappedAlignments-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_GappedAlignments.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`autoplot` for *GappedAlignments*

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *GappedAlignments*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->

Read a example bam file
<!--begin.rcode read, message = TRUE, warning = FALSE
end.rcode-->

Example geoms
<!--begin.rcode exp, message = TRUE, warning = FALSE
end.rcode-->

