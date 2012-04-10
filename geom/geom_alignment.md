---
layout: static
title: geom_alignment
---




### Introduction
`geom_alignment` is lower level API for creating alignemtns for interval data,
such as *GRanges*  and even more native *GRangesList* object. 

### Objects
  * *GRanges*
  
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




Let's generate some simulated interval data and store it as *GRanges* object.


{% highlight r %}
## ======================================================================
## simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, 
    replace = TRUE), IRanges(start = sample(1:300, size = N, replace = TRUE), 
    width = sample(70:75, size = N, replace = TRUE)), strand = sample(c("+", 
    "-", "*"), size = N, replace = TRUE), value = rnorm(N, 10, 3), score = rnorm(N, 
    100, 30), sample = sample(c("Normal", "Tumor"), size = N, replace = TRUE), 
    pair = sample(letters, size = N, replace = TRUE))
{% endhighlight %}





Default is use stat stepping, which laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting, it's a very rough exploration as first step for some interval data.


**NOTICE** default groupping intervals based on stepping levels, which doesn't
  make sense in mose cases! the chevron connect them doesn't make too much sense
  too, so make sure you group them based on some meaningful values, like
  transcript id.



{% highlight r %}
## ======================================================================
## default
## ======================================================================
ggplot() + geom_alignment(gr)
{% endhighlight %}

![plot of chunk default](http://tengfei.github.com/ggbio/geom/geom_alignment-default.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
## ======================================================================
## facetting and aesthetics
## ======================================================================
ggplot() + geom_alignment(gr, facets = sample ~ seqnames, aes(color = strand, 
    fill = strand))
{% endhighlight %}

![plot of chunk facet_aes](http://tengfei.github.com/ggbio/geom/geom_alignment-facet_aes.png) 



`group` make sure grouped intervals are on the same levels when `stat =
"stepping"`,  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.



{% highlight r %}
## ======================================================================
## stat:stepping
## ======================================================================
ggplot() + geom_alignment(gr, stat = "stepping", aes(group = pair))
{% endhighlight %}

![plot of chunk stat:stepping](http://tengfei.github.com/ggbio/geom/geom_alignment-stat:stepping.png) 


`group.selfish` force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

![plot of chunk group.selfish](http://tengfei.github.com/ggbio/geom/geom_alignment-group.selfish.png) 


We allow you to change main geoms and gaps geoms too, you can always use
eligible geoms for intervals data, for example, `geom_arrowrect` could be
extracted to name "arrowrect" and passed to argument `main.geom`, so does
gap.geom.



{% highlight r %}
## ======================================= main/gap geom
## =======================================
ggplot() + geom_alignment(gr, main.geom = "arrowrect", gap.geom = "chevron")
{% endhighlight %}

![plot of chunk main_gap](http://tengfei.github.com/ggbio/geom/geom_alignment-main_gap.png) 




