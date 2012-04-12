---
layout: static
title: autoplot_BamFile
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_BamFile-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_BamFile.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`autoplot` for *BamFile*. Default is using esimated coverage(fast) and the first
chromosome presented in the bam file header unless you specify a specific region
you want to visualize.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *BamFile*
  
### Usage
  upcomming

### Examples
Load packages and read an example bam file
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->

Default method for stat "coverage" is "estiamted".
<!--begin.rcode coverage_est, message = TRUE, warning = FALSE
end.rcode-->

method "raw" is way slow and need to provide a small region to parsing the raw
files to a set of short reads stored as *GRanges* object, then make coverage
transformation. 
<!--begin.rcode coverage_raw, message = TRUE, warning = FALSE
end.rcode-->

stat "mismatch" will generate mismatch summary, it's a wrapper for lower level
API, `stat_mismatch`.
<!--begin.rcode mismatch, message = TRUE, warning = FALSE
end.rcode-->

If you specify other geom and stat, this will simply parse a set of short reads
and use `autoplot` for `GRanges` instead, so extra arguments could be provided
too.
<!--begin.rcode other, message = TRUE, warning = FALSE
end.rcode-->


