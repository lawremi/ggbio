---
layout: static
title: theme_alignment
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "theme_alignment-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/theme"
    fl<- file.path(dir.path, "theme_alignment.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`theme_alignment` is designed for creating appropriate themes for interval
data. This return a option list like normal `theme_bw` in *ggplot2* package. 


### Usage
  upcomming
  
### Examples
Load packages, get a `genesymbol` data set from package *biovizBase*, and load
gene features from package *TxDb.Hsapiens.UCSC.hg19.knownGene*.
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Default theme is with gray background, it's actually a preset theme in
*ggplot2*, called `theme_gray`.
<!--begin.rcode theme:default, message = FALSE, warning = FALSE
end.rcode-->

Compare to `theme_gray`, here is how we make flexible tweak with themes with
`theme_alignment`. It's not limited to alignments, since in *ggbio*, most tracks
are linear interval alignments sitting on the genomic coordinates, that's the
reason I called it `theme_alignment`.
<!--begin.rcode theme:alignment, message = FALSE, warning = FALSE
end.rcode-->
