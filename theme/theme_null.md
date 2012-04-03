---
layout: static
title: theme_null
---




### Introduction
`theme_null` is designed for creating *blank* or *null* theme. This return a
option list like normal `theme_bw` in *ggplot2* package.

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




Simulate an inteval set.


{% highlight r %}
##
#   ======================================================================
##  simmulated GRanges
##
#   ======================================================================
gr <- GRanges(seqnames = "chr1", IRanges(start = sample(1:300, 
    size = N, replace = TRUE), width = sample(70:75, size = N, 
    replace = TRUE)), strand = sample(c("+", "-", "*"), size = N, 
    replace = TRUE))
{% endhighlight %}





Default theme is with gray background, it's actually a preset theme in
*ggplot2*, called `theme_gray`.


{% highlight r %}
autoplot(gr)
{% endhighlight %}

![plot of chunk default](http://i.imgur.com/tzUTA.png) 


Compare to `theme_gray`, here is how we make blank theme with `theme_null`.


{% highlight r %}
autoplot(gr) + theme_null()
{% endhighlight %}

![plot of chunk theme_null](http://i.imgur.com/9KcYO.png) 

