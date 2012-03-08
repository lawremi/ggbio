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

Let's generate some simulated interval data and store it as *GRanges* object.



{% highlight r %}
set.seed(1)
N <- 100
library(ggbio)
library(GenomicRanges)
## =======================================
##  simmulated GRanges
## =======================================
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
plotting, it's a very rough exploration as first step for some interval data.

**NOTICE** default groupping intervals based on stepping levels, which doesn't
  make sense in mose cases! the chevron connect them doesn't make too much sense
  too, so make sure you group them based on some meaningful values, like
  transcript id.



{% highlight r %}
## =======================================
##  default
## =======================================
ggplot() + geom_alignment(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/kmNZp.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
## =======================================
##  facetting and aesthetics
## =======================================
ggplot() + geom_alignment(gr, facets = sample ~ 
    seqnames, aes(color = strand, fill = strand))
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/dQGax.png) 


Group your interval based on some other variable, here is simulated "pair"
variable. Default `group.selfish` is set to `TRUE`, so each group take up unique
stepping level which is one unique row in the plot, and this enable the group
names showing as y labels.



{% highlight r %}
## =======================================
##  grouping with group.selfish = TRUE
## =======================================
ggplot() + geom_alignment(gr, aes(group = pair))
{% endhighlight %}

![plot of chunk unnamed-chunk-4](http://i.imgur.com/6a6Tx.png) 


Group your interval based on some other variable, here is simulated "pair"
variable. When `group.selfish` is set to FALSE, we allow overlapped group fall
into the same stepping level, this will remove the group names.


{% highlight r %}
## =======================================
##  grouping with group.selfish = FALSE
## =======================================
ggplot() + geom_alignment(gr, aes(group = pair), 
    group.selfish = FALSE)
{% endhighlight %}

![plot of chunk unnamed-chunk-5](http://i.imgur.com/ptP6p.png) 


We allow you to change main geoms and gaps geoms too, you can always use
eligible geoms for intervals data, for example, `geom_arrowrect` could be
extracted to name "arrowrect" and passed to argument `main.geom`, so does gap.geom.


{% highlight r %}
## =======================================
##  main/gap geom
## =======================================
ggplot() + geom_alignment(gr, main.geom = "arrowrect", 
    gap.geom = "arrow")
{% endhighlight %}

![plot of chunk unnamed-chunk-6](http://i.imgur.com/lARLW.png) 




