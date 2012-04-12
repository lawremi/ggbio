---
layout: post
title: Coordiante "genome" and how to make a Manhattan plot 
category: blog
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "manhattan-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(base.url='https://github.com/tengfei/ggbio/raw/gh-pages/_posts/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/_posts"
    fl<- file.path(dir.path, "Manhattan.R")
    read_chunk(fl)
end.rcode-->

- [Step 0: Introduction](#s0)
- [Step 1: Understand the new coordinate](#s1)
- [Step 2: Simulate a SNP data set](#s2)
- [Step 3: Start to make Manhattan plot by using `autoplot`](#s3)
- [Step 4: Convenient `plotGrandLinear` function](#s4) 


## Step 0: Introduction<a id = "s0"></a>
In this section, we introduce a new coordinate system called "genome" for
genomic data. This transformation is to put all chromosomes on the same genome
coordinates following specified orders and adding buffers in between.  One may
think about facet ability based on `seqnames`, it can produce something similar
to [*Manhattan plot*](http://en.wikipedia.org/wiki/Manhattan), but the view will
not be compact. What's more, genome transformation is previous step  to form a circular
view. In this tutorial, we will simulate some SNP data and use this special
coordinate and a specialized function `plotGrandLinear` to make a Manhattan
plot. 

*Manhattan plot* is just a special use design with this coordinate system.

## Step 1: Understand the new coordinates <a id = "s1"></a>
Let's load some package and data first
<!--begin.rcode load, message = FALSE, warning = FALSE
end.rcode-->

Make a minimal example `GRanges`, and see what the default looks like, pay
attention that, by default, the graphics are faceted by `seqnames`
<!--begin.rcode simul_gr, message = FALSE, warning = FALSE
end.rcode-->

What if we specify the coordinate system to be "genome" in `autoplot`, there is
no faceting anymore, the two plots are merged into one single genome space, and
properly labeled. The internal transformation are implemented into the function
`transformToGenome`, there is a limitation on *integer* in *R*, so the genome
space cannot be too long, to overcome this limitation, a default argument called
`maxSize` is defined with this function, if the genome space is over limits, it
will rescale everything automatically, function `tranformToGenome` with return a
transformed `GRanges` object, with only one single `seqnames` called "genome"
and the `seqlengths` of it, is just genome space(with buffering region). There
will be an column called ".ori" which stored the original data sets, when
`fortify` that object to a `data.frame`, all information there will be coerced
as extra columns but with prefix ".ori.", for example, ".ori.seqnames" is the
original one, you could use this for aesthetics mapping. Extra arguments called
`space.ratio` control the skipped region between chromosomes.
<!--begin.rcode coord:genome, message = FALSE, warning = FALSE
end.rcode-->

And there is some simple way to test if a `GRanges` is transformed to coordinate
"genome" or not
<!--begin.rcode is, message = FALSE, warning = FALSE
end.rcode-->


## Step 2: Simulate a SNP data set <a id = "s2"></a>
Let's use the real genome space to simulate a SNP data set.
<!--begin.rcode simul_snp, message = FALSE, warning = FALSE
end.rcode-->

As introduced in the tutorial about
[processing](http://tengfei.github.com/ggbio/blog/2012/04/11/processing/), we
mentioned how to tweak with chromosome names and orders. Here we use the same
trick to make a shorter names.
<!--begin.rcode shorter, message = FALSE, warning = FALSE
end.rcode-->

## Step 3: Start to make Manhattan plot by using `autoplot` <a id = "s3"></a> We
wrapped basic functions into `autoplot`, you can specify the coordinate. so what
does the unordered object looks like?
<!--begin.rcode unorder, message = FALSE, warning = FALSE
end.rcode-->

That's probably not what you want, if you want to change to specific order, just
sort them by hand and use `keepSeqlevels`.
<!--begin.rcode sort, message = FALSE, warning = FALSE
end.rcode-->

**NOTICE**, the data now doesn't have information about lengths of each
  chromosomes, this is allowed to be plotted, but it's misleading sometimes,
  without chromosomes lengths information, *ggbio* use data space to make
  estimated lengths for you, this is not accurate! So let's just assign
  `seqlengths` to the object. Then you will find the data space now is
  distributed proportional to real space.
<!--begin.rcode with_seql, message = FALSE, warning = FALSE
end.rcode-->

In `autoplot`, argument `coord` is just used to transform the data, after that,
you can use it as common `GRanges`, all other geom/stat works for it. Here just
show a simple example for another geom "line"
<!--begin.rcode line, message = FALSE, warning = FALSE
end.rcode-->

## Step 4: Convenient `plotGrandLinear` function <a id = "s4"></a>
In *ggbio*, sometimes we develop specialized function for certain types of
plots, it's basically a wrapper over lower level API and `autoplot`,  but more
convenient to use. Here for *Manhattan plot*, we have a function called
`plotGrandLinear` used for it. aes(y = ) is required.
<!--begin.rcode plotGrandLinear, message = FALSE, warning = FALSE
end.rcode-->

Color mapping is automatically figured out by *ggbio* following the rules

- if `color` present in `aes()`, like `aes(color = seqnames)`, it will assume
  it's mapping to data column.
- if `color` is not wrapped in `aes()`, then this function will *recylcle* them
  to all chromosomes. 
- if `color` is single character, then just use one arbitrary color

<!--begin.rcode morecolor, message = FALSE, warning = FALSE
end.rcode-->

You can also add cutoff line
<!--begin.rcode cutoff, message = FALSE, warning = FALSE
end.rcode-->

This is equivalent to 
<!--begin.rcode cutoff-low, message = FALSE, warning = FALSE
end.rcode-->



Sometimes the names of chromosomes maybe very long, you may want to rotate them, 
let's make a longer name first
<!--begin.rcode longer, message = FALSE, warning = FALSE
end.rcode-->

Then rotate it!
<!--begin.rcode rotate, message = FALSE, warning = FALSE
end.rcode-->
all utilities works for *ggplot2* will work for *ggbio* too.

sessionInfo
<!--begin.rcode sessionInfo, message = FALSE, warning = FALSE
end.rcode-->







