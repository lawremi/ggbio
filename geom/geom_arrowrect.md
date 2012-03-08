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




Default `stat` is "stepping"



{% highlight r %}
ggplot() + geom_arrowrect(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/42Y0L.png) 


Facetting and aesthetics mapping


{% highlight r %}
ggplot() + geom_arrowrect(gr, facets = sample ~ 
    seqnames, fill = "red")
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/ZX9KA.png) 


`stat` "identity" is used for mapping y axis to a variable, and argument
`rect.height` is used to control the width of the arrow body. 



{% highlight r %}
ggplot() + geom_arrowrect(gr, stat = "identity", 
    aes(y = value), rect.height = 0.1)
{% endhighlight %}

![plot of chunk unnamed-chunk-4](http://i.imgur.com/5KEz4.png) 
