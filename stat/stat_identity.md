---
layout: static
title: stat_identity
---




### Introduction

`stat_identity` is coerce a *GRanges* object to a data.frame inside with new
variable `midpoint` which is `(start + end)/2`. `stat_identity` allows you to
treat a *GRanges* object as a *data.frame*, and use all supported geom in
*ggplot2* and *ggbio*.


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




Geom *point*.


{% highlight r %}
ggplot() + stat_identity(gr, aes(x = start, y = value), geom = "point")
{% endhighlight %}

![plot of chunk geom_point_start](http://tengfei.github.com/ggbio/stat/stat_identity-geom_point_start.png) 


Geom *point*, with x set to `midpoint`, which doesn't exists in original data.


{% highlight r %}
ggplot() + stat_identity(gr, aes(x = midpoint, y = value), geom = "point")
{% endhighlight %}

![plot of chunk geom_point_midpoint](http://tengfei.github.com/ggbio/stat/stat_identity-geom_point_midpoint.png) 


Geom *rect*, need to specify `xmin`, `xmax`, `ymin`, `ymax`.


{% highlight r %}
ggplot() + stat_identity(gr, aes(xmin = start, xmax = end, ymin = value - 
    0.5, ymax = value + 0.5), geom = "rect")
{% endhighlight %}



{% highlight text %}
## Error: object 'args.aes.seg' not found
{% endhighlight %}




Geom *rect*, or just specify a `y` use default for boundary.


{% highlight r %}
ggplot() + stat_identity(gr, aes(y = value), geom = "rect")
{% endhighlight %}

![plot of chunk geom_rect_y](http://tengfei.github.com/ggbio/stat/stat_identity-geom_rect_y.png) 


Geom *line*


{% highlight r %}
ggplot() + stat_identity(gr, aes(x = start, y = value), geom = "line")
{% endhighlight %}

![plot of chunk geom_line](http://tengfei.github.com/ggbio/stat/stat_identity-geom_line.png) 


Geom *segment*


{% highlight r %}
ggplot() + stat_identity(gr, aes(y = value), geom = "segment")
{% endhighlight %}

![plot of chunk geom_segment](http://tengfei.github.com/ggbio/stat/stat_identity-geom_segment.png) 




