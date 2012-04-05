---
layout: static
title: geom_rect
---




### Introduction
`geom_rect` is lower level API for creating rectangles for interval data,
such as *GRanges* object.

### Objects
  * *GRanges*
  * *data.frame* , just like ggplot2::geom_rect
  
### Usage
  upcomming
  
### Examples
Load packages


{% highlight r %}
set.seed(1)
N <- 100
require(ggbio)
require(GenomicRanges)
{% endhighlight %}




When the object is a *data.frame*, it calls ggplot2::geom_rect inside, here is a
silly example:














