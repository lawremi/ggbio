---
layout: static
title: stat_coverage
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "stat_coverage-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/stat/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/stat"
    fl<- file.path(dir.path, "stat_coverage.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`stat_coverage` is lower level API computing coverage from raw bam file,
*GRanges*, and *GRangesList*. For *Bamfile*, a fast estimated method has been
implemented by Michael Lawrence in package *biovizBase* and wrapped as one
method in `stat_coverage`.

### Objects
  * *GRanges*
  * *GRangesList* 
  * *BamFile*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->
  
Let's generate some simulated interval data and store it as *GRanges* object.
<!--begin.rcode simul, message = TRUE, warning = FALSE
end.rcode-->


Test different geom, notice `..coverage..` is a variable name that is not in
original data but in transformed data, if you hope to use this new statistics,
please you `..` to wrap around `coverage`, it indicates it belongs to interval
variable. 
<!--begin.rcode geom,  message = FALSE, warning = FALSE
end.rcode-->

Facetting, column for `seqnames` is requried.
<!--begin.rcode facet:sample, message = FALSE, warning = FALSE
end.rcode-->

Faceted by strand help you understand coverage from different sequencing direction. 
<!--begin.rcode facet:strand, message = FALSE, warning = FALSE
end.rcode-->

Let's create a *GRangesList* object.
<!--begin.rcode grl, message = FALSE, warning = FALSE
end.rcode-->

For *GRangesList* object, default is coerce it to *GRanges*.
<!--begin.rcode grl:default, message = FALSE, warning = FALSE
end.rcode-->

Internal variable `..grl_name..` added to keep a track for grouping information,
you could use it for faceting or other mapping.
<!--begin.rcode grl:facet, message = FALSE, warning = FALSE
end.rcode-->

Load a RNA-seq data
<!--begin.rcode BamFile, message = FALSE, warning = FALSE
end.rcode-->

Default method is "estimate", which is very fast and efficient estimation for
whole genome, if you didn't provide which, we only show the first chromosome.
<!--begin.rcode BamFile:est, message = FALSE, warning = FALSE
end.rcode-->

If you really want to get accurate coverage information on the fly, use method
"raw", make sure you provide a relatively small region for `which` argument,
otherwise, it's going to be very slow. Internally it parse short reads to
*GRanges* and then calling coverage on it.
<!--begin.rcode BamFile:raw, message = FALSE, warning = FALSE
end.rcode-->






