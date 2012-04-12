---
layout: static
title: tracks
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.path = "tracks-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
##    opts_knit$set(upload.fun = function(file) 
##       imgur_upload(file, key = "7733c9b660907f0975935cc9ba657413"))
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/tracks/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/tracks"
    fl<- file.path(dir.path, "tracks.R")
    read_chunk(fl)
end.rcode-->

### Introduction
`tracks` is a conventient wrapper for bindind graphics as trakcs. You dont' have
to worry about adjusting different graphics, `tracks` did that for you. It's NOT
just limited to bind genomic tracks, you can use this function to bind any
tracks with the same defination of x axis, for example, sets of time series
plots you made.

Tracks view is most common way to viewing genome features and annotation data
and widely used by most genome browsers. Our assumption is that, most graphics
you made with *ggbio* or by yourself using *ggplot2*, are almost always sitting
on the genomic coordinates or the same x axis. And to compare annotation
information along with genome features, we need to align those plots on exactly
the same x axis in order to form your hypothesis. This function leaves you the
flexibility to construct each tracks separately with worrying your alignments
later.

*ggbio* provide a set of utilities to reset, backup, and apply options to
 tracks, please see examples below.

**NOTICE**: `tracks` did following things for you
 
 *  Only keep the bottom x axis based on assumption that all tracks are on the
 same space, but still keep x ticks. For simply wrapping, please use `align.plots`.
 *  `ncol` which defines columns is always 1, because binding tracks in the
    context of genomic data is almost always one single column. Multiple column
    alignments are not supported yet.


 
Alternatively you have a `align.plots` whic simply align the plots based on x axis
without modifying any other attributes about the plots.

### Objects
  * A set of ggplot graphic objects.
  
### Usage
  upcomming
  
### Examples
Load packages required for getting gene features.
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Let's generate two plots for full and reduced gene model, and bind them in
tracks.
<!--begin.rcode tracks, message = FALSE, warning = FALSE
end.rcode-->

`align.plots` simply align the plots based on x axis
without modifying any other attributes about the plots.
<!--begin.rcode align.plots, message = FALSE, warning = FALSE
end.rcode-->

`reset` and `backup` help you play with options and appearance of the tracks,
you could save certain status by calling `backup`, and get backup version back
by calling `reset`.
<!--begin.rcode reset, message = FALSE, warning = FALSE
end.rcode-->

`summary` give you meta information about tracks, and `update` allow you to
update a plot xlim on the fly, you can simply keep the plot window and run
`update` to tweak with the view. Other wise you need to revise the tracks object
and print it again.
<!--begin.rcode utils, message = FALSE, warning = FALSE
end.rcode-->

For convenient use and advanced *ggplot2* and *ggbio* users, if you are familiar
with `theme` and `options`, you could always use the same way with tracks, I
redefined `+` and make those works for `Tracks` object too. All the options will
be applied to every plot in the tracks.
<!--begin.rcode opts, message = FALSE, warning = FALSE
end.rcode-->



