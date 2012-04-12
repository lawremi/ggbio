---
layout: static
title: autoplot_TranscriptDb
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_TranscriptDb-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_TranscriptDb.R")
    read_chunk(fl)
end.rcode-->

### Introduction

*TranscriptDb* is used for storing genomic features such as exons, cds,
 transcrits,`autoplot` is designed to plot gene structure, it parse information
 stored in the object to generate proper introns and utrs, it provides two
 `geom` now, one called `gene`, used to plot isoform for different
 transcript. and geom `reduce` used to reduce the information to generate single
 gene structure.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *TranscriptDb*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->
  
Two geoms
<!--begin.rcode tracks, message = FALSE, warning = FALSE
end.rcode-->

`which` argument also accept gene id.
<!--begin.rcode id, message = FALSE, warning = FALSE
end.rcode-->

