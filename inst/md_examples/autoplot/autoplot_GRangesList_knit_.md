---
layout: static
title: autoplot_GRangesList
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "autoplot_GRangesList-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/autoplot/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/autoplot"
    fl<- file.path(dir.path, "autoplot_GRangesList.R")
    read_chunk(fl)
end.rcode-->

### Introduction

*GRangesList* is most suitable data structure for storing a set of genomic
 features, for example, exons/utrs in a gene. `autoplot` is designed to consider
 the native grouping information in this structure and automatically showing
 gaps within group in `geom` *alignment* and make sure grouped items are shown
 together on the same level with nothing falling in between.

`main.geom` and `gap.geom` control geometry for entities and gaps computed for
them. `group.selfish` help you put grouped items in unique y levels and show the
y labels for group names.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *GRangesList*
  
### Usage
  upcomming

### Examples
Load packages
<!--begin.rcode load, message = TRUE, warning = FALSE
end.rcode-->
  
Let's create a *GRangesList* object.
<!--begin.rcode simul, message = FALSE, warning = FALSE
end.rcode-->

For *GRangesList* object, default is coerce it to *GRanges* and adding extra
column to preserve the grouping information. main geoms and gaps geom are
separately controlled.
<!--begin.rcode exp, message = FALSE, warning = FALSE
end.rcode-->

Internal variable `grl_name` added to keep a track for grouping information, you
could use it for faceting or other aesthetic mapping, the variables could be
renamed by `indName` argument in `autoplot`, you could pass either
`..grl_name..` or `grl_name` in the mapping, I prefer the first one, it tells
that it's interval variables.
<!--begin.rcode grl_name, message = FALSE, warning = FALSE
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






