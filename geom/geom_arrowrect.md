---
layout: static
title: geom_arrowrect
---




### Introduction
`geom_arrowrect` is lower level API for creating a 5-side polygon, like a
rectangle with arrow head, for interval data such as *GRanges* object. It map
the strand information automatically back to arrow head direction, when the
strand is "\*", it's just rectangle.

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



{% highlight r %}
## ======================================================================
## default
## ======================================================================
ggplot() + geom_arrowrect(gr)
{% endhighlight %}

![plot of chunk default](http://tengfei.github.com/ggbio/geom/geom_arrowrect-default.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
## ======================================================================
## facetting and aesthetics
## ======================================================================
ggplot() + geom_arrowrect(gr, facets = sample ~ seqnames, aes(color = strand, 
    fill = strand))
{% endhighlight %}

![plot of chunk facet_aes](http://tengfei.github.com/ggbio/geom/geom_arrowrect-facet_aes.png) 


Stat "identity" allows you to specify a y value to use as y-axis instead of
default stepping level.



{% highlight r %}
## ======================================================================
## stat:identity
## ======================================================================
ggplot() + geom_arrowrect(gr, stat = "identity", aes(y = value))
{% endhighlight %}

![plot of chunk stat:identity](http://tengfei.github.com/ggbio/geom/geom_arrowrect-stat:identity.png) 


`group` make sure grouped intervals are on the same levels when `stat =
"stepping"`,  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.



{% highlight r %}
## ======================================================================
## stat:stepping
## ======================================================================
ggplot() + geom_arrowrect(gr, stat = "stepping", aes(y = value, group = pair))
{% endhighlight %}

![plot of chunk stat:stepping](http://tengfei.github.com/ggbio/geom/geom_arrowrect-stat:stepping.png) 


`group.selfish` force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

![plot of chunk group.selfish](http://tengfei.github.com/ggbio/geom/geom_arrowrect-group.selfish.png) 



