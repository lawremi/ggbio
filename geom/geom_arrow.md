---
layout: static
title: geom_arrow
---




### Introduction
`geom_arrow` is lower level API for creating small arrows for interval data,
such as *GRanges* object.

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




Default `stat` is "stepping". 


{% highlight r %}
ggplot() + geom_arrow(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/r6kJN.png) 


Faceting and aesthetics mapping


{% highlight r %}
ggplot() + geom_arrow(gr, facets = sample ~ seqnames, 
    color = "red")
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/Zy8ac.png) 


`stat = "identity"` allows you assign customized `y` value.


{% highlight r %}
ggplot() + geom_arrow(gr, stat = "identity", aes(x = start, 
    y = value, xend = end, yend = value))
{% endhighlight %}

![plot of chunk unnamed-chunk-4](http://i.imgur.com/Zife4.png) 
