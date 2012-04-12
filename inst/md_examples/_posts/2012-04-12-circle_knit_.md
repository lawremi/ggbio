---
layout: post
title: Layout "circle" and how to make circular view.
category: blog
---
<!--roptions dev='png', fig.width=8, fig.height=8, fig.keep = "all", fig.path = "2012-04-12-circle-" -->
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(base.url='https://github.com/tengfei/ggbio/raw/gh-pages/_posts/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/_posts"
    fl<- file.path(dir.path, "circle.R")
    read_chunk(fl)
end.rcode-->

- [Step 0: Introduction](#s0)
- [Step 1: Understand the layout circle](#s1)
- [Step 2: Get your data ready to plot](#s2)
- [Step 3: low level API: `layout_circle`](#s3)
- [Step 4: All-in-one: `autoplot` for `GenomicRangesList`](#s4) 
- [Step 5: Complex arragnment of plots](#s5) 


## Step 0: Introduction<a id = "s0"></a>
Layout "circle" is inspired by [Circos project](http://circos.ca/). We extended
it into the grammar of graphics and make it a general layout. Layout is generally
more complex than a coordinate transformation, it's a combination of different
components like coordinate transformation(genome and polar), and tracks-based
layout, etc. Especially, circular view is very useful to show links between
different locations. Since we are following the grammar of graphics, aesthetics
mapping are fairly easy in *ggbio*.

In this tutorial, we will start from the raw data, if you are already familiar
with how to process your data into the right format, which here I mean `GRanges`
and `GenomicRangesList`, you can jump to [Step 3](#s3) directly.


## Step 1: Understand the layout circle <a id = "s1"></a>
We have discussed about the new
[coordinate "genome"](http://tengfei.github.com/ggbio/blog/2012/04/12/Manhattan/)
before, now this time, it's one step further compared to genome coordinate
transformation. We specify ring radius `radius` and track width `trackWidth` to
help transform a linear genome coordinate system to a circular coordinate
system. By using `layout_circle` function which we will introduce later.

Before we visualize our data, we need to have something in mind

- How many tracks we want?
- Can they be combined into the same data?
- Do I have chromosomes lengths information?
- Do I have interesting variables attached as one column?

## Step 2: Get your data ready to plot <a id = "s2"></a>
Ok,  let's start to process some raw data to the format we want. The data used
in this study is from this
[paper](http://www.nature.com/ng/journal/v43/n10/full/ng.936.html). In this
example, We are going to 

1. Visualize somatic mutation as segment
2. Visualize inter,intro-chromosome rearrangement as links
3. Visualize mutation score as point tracks with grid-background
4. Add scale and ticks and labels.
5. To arrange multiple plots and legend. create multiple sample comparison.
6. Don't put too much tracks on it.

We introduced
[how to process your data](http://tengfei.github.com/ggbio/blog/2012/04/11/processing/)
before, since we use the same data, I simply put script here to get mutation
data as `GRanges` object.
<!--begin.rcode mut_processing, message = FALSE, warning = FALSE
end.rcode-->

To get ideogram track, we need to load human hg19 ideogram data, for details
about how to get genome lengths information, please refer back to [how to process your data](http://tengfei.github.com/ggbio/blog/2012/04/11/processing/).
<!--begin.rcode ideo, message = FALSE, warning = FALSE
end.rcode-->

## Step 3: low level API: `layout_circle` <a id = "s3"></a>
`layout_circle` is a lower level API for creating circular plot, it accepts
`GRanges` object, and users need to specify radius, track width, and other
aesthetics, it's very flexible. But keep in mind, you **have to ** pay attention
rules when you make circular plots.

- For now, `seqlengths`, `seqlevels` and chromosomes names should be exactly the same, so
  you have to make sure data on all tracks have this uniform information to make
  a comparison.
- Set arguments `space.skip` to the same value for all tracks, that matters for
  transformation, default is the same, so you don't have to change it, unless
  you want to add/remove space in between.
- `direction` argument should be exactly the same, either "clockwise" or
  "counterclockwise".
- Tweak with your radius and tracks width to get best results. 

since low level API leave you as much flexibility as possible, this may looks
hard to adjust, but it can produce various types of graphics which higher levels
API like `autoplot` hardly can, for instance, if you want to overlap multiple
tracks or fine-tune your layout.

Ok, let's start to add tracks one by one.

First to add a "ideo" track
<!--begin.rcode lower-ideo-track, message = FALSE, warning = FALSE
end.rcode-->

Then a "scale" track with ticks
<!--begin.rcode lower-scale-track, message = FALSE, warning = FALSE
end.rcode-->

Then a "text" track to label chromosomes. *NOTICE*, after genome coordinate
transformation, original data will be stored in column ".ori", and for mapping,
just use ".ori" prefix to it. Here we use `.ori.seqnames`, if you use
`seqnames`, that is going to be just "genome" character.
<!--begin.rcode lower-text-track, message = FALSE, warning = FALSE
end.rcode-->

Then a "rectangle" track to show somatic mutation, this will looks like vertical
segments. 
<!--begin.rcode lower-mut-track, message = FALSE, warning = FALSE
end.rcode-->

Ok, fun part comes, we need to add some "links" to show the rearrangement, of
course, links can be used to map any kind of association between two or more
different locations to indicate relationships like copies or fusions.
<!--begin.rcode links, message = FALSE, warning = FALSE
end.rcode-->

To create a suitable structure to plot, please use another `GRanges` to
represent the end of the links, and stored as elementMetadata for the "start
point" `GRanges`. Here we named it as "to.gr" and will be used later.
<!--begin.rcode link-data, message = FALSE, warning = FALSE
end.rcode-->

Here we show the flexibility of *ggbio*, for example, if you want to use color
to indicate your links, make sure you add extra information in the data, used
for mapping later. Here in this example, we use "intrachromosomal" to label
rearrangement within the same chromosomes and use "interchromosomal" to label
rearrangement in different chromosomes.
<!--begin.rcode rearr, message = FALSE, warning = FALSE
end.rcode-->

Get subset of links data for only one sample "CRC1"
<!--begin.rcode subset-crc-1, message = FALSE, warning = FALSE
end.rcode-->

Ok, add a "point" track with grid background for rearrangement data and map `y`
to variable "score", map `size` to variable "tumreads", rescale the size to a
proper size range.
<!--begin.rcode lower-point-track, message = FALSE, warning = FALSE
end.rcode-->

Finally, let's add links and map color to rearrangement types. Remember you need
to specify `linked.to` to the column that contain end point of the data.
<!--begin.rcode lower-link-track, message = FALSE, warning = FALSE
end.rcode-->

## Step 4: All-in-one: `autoplot` for `GenomicRangesList` <a id = "s4"></a>
`GenomicRangesList` is a container for `GRanges` like object, they can have
different column, this is different from `GRangesList` which require all element
have the same element meta data column. 

Now we introduce higher level API `autoplot`, for object `GenomicRangesList`,
what `autoplot` for it?

- You can only specify aesthetics mapping
- All radius and track width are automatically set up, you don't have to adjust
  them by hand, you can just revise track widths.
- Of course you can revise radius/track width/grid background too.

Let's use `autoplot` to reproduce what we got in Step 3.
<!--begin.rcode autoplot, message = FALSE, warning = FALSE
end.rcode-->

## Step 5: Complex arragnment of plots <a id = "s5"></a>
In this step, we are going to make multiple sample comparison, this may require some
knowledge about package *grid* and *gridExtra*.

We just want 9 single circular plots put together in one page, since we cannot
keep too many tracks, we only keep ideogram and links. Here is one sample.
<!--begin.rcode single-arr, message = FALSE, warning = FALSE
end.rcode-->

We need to get one legend for all of them, and put it on the right, ok, that's a
little tricky, we can first store the legend from the plot we just created.
<!--begin.rcode legend, message = FALSE, warning = FALSE
end.rcode-->

Start to creating plots for each sample.
<!--begin.rcode arrangement, message = FALSE, warning = FALSE
end.rcode-->


Use package *gridExtra* to arrange them and add legend to the right.
<!--begin.rcode 9-circle, message = FALSE, warning = FALSE, eval = FALSE
end.rcode-->
![plot of 9 circle](http://tengfei.github.com/ggbio/images/cir.png)


sessionInfo
<!--begin.rcode sessionInfo, message = FALSE, warning = FALSE
end.rcode-->




