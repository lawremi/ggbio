---
layout: static
title: geom_chevron
---




### Introduction
`geom_chevron` is is lower level API for creating chevrons for interval data, such as GRanges object, it could be used to visuzalize introns or splicing.

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





Default is use stat stepping, which laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting.



{% highlight r %}
##
#   ======================================================================
##  default
##
#   ======================================================================
ggplot() + geom_chevron(gr)
{% endhighlight %}

![plot of chunk default](http://i.imgur.com/Lk9q4.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
##
#   ======================================================================
##  facetting and aesthetics
##
#   ======================================================================
ggplot() + geom_chevron(gr, facets = sample ~ 
    seqnames, aes(color = strand))
{% endhighlight %}

![plot of chunk facet_aes](http://i.imgur.com/eHYbD.png) 


Stat "identity" allows you to specify a y value to use as y-axis instead of
default stepping level.



{% highlight r %}
##
#   ======================================================================
##  stat:identity
##
#   ======================================================================
ggplot() + geom_chevron(gr, stat = "identity", 
    aes(y = value))
{% endhighlight %}

![plot of chunk stat:identity](http://i.imgur.com/15Yce.png) 


`group` make sure grouped intervals are on the same levels when `stat =
"stepping"`,  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.



{% highlight r %}
##
#   ======================================================================
##  stat:stepping
##
#   ======================================================================
ggplot() + geom_chevron(gr, stat = "stepping", 
    aes(group = pair))
{% endhighlight %}

![plot of chunk stat:stepping](http://i.imgur.com/khcAT.png) 


`group.selfish` force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

![plot of chunk group.selfish](http://i.imgur.com/5J8Vu.png) 


`offset` controls the height of the chevron, notice the default rectangle height
is always 0.4*2.




![plot of chunk offset:default](http://i.imgur.com/bSlbJ.png) 


![plot of chunk offset:0](http://i.imgur.com/Y3efs.png) 


![plot of chunk offset:0.4](http://i.imgur.com/oRGtT.png) 


`chevron.height` is useful to rescale the offset when you specify the offset as
one the the variables.
![plot of chunk chevron.height:default](http://i.imgur.com/R8MTm.png) 


![plot of chunk chevron.height](http://i.imgur.com/TVc9F.png) 



