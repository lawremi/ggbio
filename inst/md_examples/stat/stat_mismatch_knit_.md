---
layout: static
title: stat_mismatch
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "stat_mismatch-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/stat/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/stat"
    fl<- file.path(dir.path, "stat_mismatch.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`stat_mismatch` is lower level API to read in a bam file and show mismatch
summary for certain region, counts at each position are summarized, those reads
which are identical as reference will be either shown as gray background or
removed, it's controled by argument `show.coverage`, mismatched part will be
shown as color-coded bar or segment.

### Objects
  * *Bamfile*
  * *GRanges*, this will pass interval checking which make sure the GRanges has
     required columns.

  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->
  
Load example bam file
<!--begin.rcode load_bam, message = FALSE, warning = FALSE
end.rcode-->

If the object is *BamFile*, a *BSgenome* object is required to compute the
mismatch summary. 
<!--begin.rcode BamFile, message = FALSE, warning = FALSE
end.rcode-->

Sometimes bam file and *BSgenome* object might have a different naming schema
for chromosomes, currently, `stat_mismatch` is not smart enough to deal with
complicated cases, in this way, you may want to get mismatch summary as
*GRanges* yourself and correct the names, with `keepSeqlevels` or
`renamesSeqleves` functions in package *GenomicRanges*. Following examples
doesn't show you how to manipulate seqnames, but just show you how to compute
mismatch summary. 
<!--begin.rcode pag, message = FALSE, warning = FALSE
end.rcode-->

And directly plot the mismatch *GRanges* object.
<!--begin.rcode pag_v, message = FALSE, warning = FALSE, fig.keep = "all"
end.rcode-->



