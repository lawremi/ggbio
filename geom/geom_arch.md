---
layout: static
title: geom_arch
---




### Introduction
`geom_arch` is lower level API for creating arches for interval data,
such as *GRanges* object. It could be used for showing splicing events or any
defined interactions in linear view.

A special thanks to **Jennifer Change**, a PhD student in
Iowa state univeristy, who initialize the first visualization of arches in
**ggbio** and distributed most of her code into this package.

### Objects
  * *GRanges*
  * *data.frame* 
  
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




Default use equal height to show the arches, for each interval, it's being
connect by two ends, so make sure if you want to connect two exons, you have to
get gaps intervals to show the links between exons. 



{% highlight r %}
## =======================================
##  default
## =======================================
ggplot() + geom_arch(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/mkypP.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.



{% highlight r %}
## =======================================
##  facetting and aesthetics
## =======================================
ggplot() + geom_arch(gr, aes(color = value, height = value, 
    size = value), alpha = 0.2, facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/N8ESX.png) 



