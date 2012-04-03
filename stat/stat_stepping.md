---
layout: static
title: stat_stepping
---




### Introduction

`stat_stepping` is lower level API laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting. It uses `add_stepping` function in *biovizBase* package to compute the
stepping levels for *GRanges* object.

### Objects
  * *GRanges*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
set.seed(1)
N <- 50
require(ggbio)
require(GenomicRanges)
{% endhighlight %}



  
  Let's generate some simulated interval data and store it as *GRanges* object.


{% highlight r %}
##
#   ======================================================================
##  simmulated GRanges
##
#   ======================================================================
gr <- GRanges(seqnames = sample(c("chr1", "chr2", 
    "chr3"), size = N, replace = TRUE), IRanges(start = sample(1:300, 
    size = N, replace = TRUE), width = sample(70:75, size = N, 
    replace = TRUE)), strand = sample(c("+", "-", "*"), size = N, 
    replace = TRUE), value = rnorm(N, 10, 3), score = rnorm(N, 
    100, 30), sample = sample(c("Normal", "Tumor"), size = N, 
    replace = TRUE), pair = sample(letters, size = N, replace = TRUE))
{% endhighlight %}





Default is use `geom_rect`, it's a very rough exploration as first step for some interval data.



{% highlight r %}
ggplot() + stat_stepping(gr)
{% endhighlight %}

![plot of chunk default](http://i.imgur.com/wB403.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
ggplot() + stat_stepping(gr, aes(color = strand, 
    fill = strand), facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk facet_aes](http://i.imgur.com/QGYdO.png) 


Use different geom, such as `segment`.


{% highlight r %}
ggplot() + stat_stepping(gr, aes(color = strand), 
    geom = "segment", xlab = "Genomic coord", ylab = "y", 
    main = "hello")
{% endhighlight %}

![plot of chunk geom_segment](http://i.imgur.com/70u7j.png) 


geom `alignment`


{% highlight r %}
ggplot() + stat_stepping(gr, geom = "alignment")
{% endhighlight %}

![plot of chunk geom_alignment](http://i.imgur.com/tiyRJ.png) 


geom `alignment` with group 


{% highlight r %}
ggplot() + stat_stepping(gr, aes(group = pair), 
    geom = "alignment")
{% endhighlight %}

![plot of chunk geom_alignment_group](http://i.imgur.com/eFqFN.png) 


