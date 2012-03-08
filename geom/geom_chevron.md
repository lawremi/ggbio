---
layout: static
title: geom_chevron
---




### Introduction
`geom_chevron` is lower level API for creating chevrons for interval data,
such as *GRanges* object, it could be used to visuzalize introns or splicing. 

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




Default `stat` is still "stepping"



{% highlight r %}
ggplot() + geom_chevron(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/qUcQn.png) 



But most time, **chevron** is going to be used together with other geoms to
create a slightly complex new "geom", in this case, we need the flexibility to
allow a customized y value by using `stat` "identity".



{% highlight r %}
ggplot() + geom_chevron(gr, stat = "identity", 
    aes(y = value))
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/yZELz.png) 
