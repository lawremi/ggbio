---
layout: static
title: stat_gene
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "stat_gene-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
#    opts_knit$set(upload.fun = function(file) 
#       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/stat/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/stat"
    fl<- file.path(dir.path, "stat_gene.R")
    read_chunk(fl)
end.rcode-->

### Introduction

`stat_gene` is lower level API parsing a *TranscriptDb* object and create gene
structures, two geoms supported

 *  gene: showing a full transcripts with cds/utr/introns
 *  reduced_gene: reduce cds/utrl/introns to generate single gene structure.

### Objects
  * *TranscriptDb*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Let's create a track for both geoms
<!--begin.rcode tracks, message = FALSE, warning = FALSE
end.rcode-->

  
