---
layout: static
title: autoplot_GRanges
---




### Introduction

`autoplot` for *GRanges* object is designed to be most general plot API in
*ggbio* package. *GRanges* is most suitable data structure for storing interval
data with medata data, which could be used for representing a set of short reads
or genomic features. 

Supported geom designed specifically for *GRanges*, including "rect", "chevron",
"alignment", "arrowrect", "arrow", "segment", "arch", and special statistical
transformation contains "identity", "coverage", "stepping", "aggregate",
"table", "gene", "mismatch". And they are implemented in lower API, such as
`geom_alignment` and `stat_coverage`. If you pass other `geom` and `stat` other
than those ones, it first use `fortify` method in *ggbio* to coerce a *GRanges*
into a `data.frame` object. And a new variable `midpoint` is created and added
to final `data.frame` to be used to mapped as `x`. So you can use it as other
*ggplot2* API.

Inside, `autoplot` will choose the best choice for your combination of `geom`
and `stat`.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *GRanges*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
{% endhighlight %}



  
Let's generate some simulated interval data and store it as *GRanges* object.


{% highlight r %}
## ======================================================================
## simmulated GRanges
## ======================================================================
set.seed(1)
N <- 1000
library(GenomicRanges)
gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, 
    replace = TRUE), IRanges(start = sample(1:300, size = N, replace = TRUE), 
    width = sample(70:75, size = N, replace = TRUE)), strand = sample(c("+", 
    "-", "*"), size = N, replace = TRUE), value = rnorm(N, 10, 3), score = rnorm(N, 
    100, 30), sample = sample(c("Normal", "Tumor"), size = N, replace = TRUE), 
    pair = sample(letters, size = N, replace = TRUE))
idx <- sample(1:length(gr), size = 200)
{% endhighlight %}




default is use geom "rect".


{% highlight r %}
autoplot(gr)
{% endhighlight %}

![plot of chunk default](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-default.png) 


Facetting, some combination of geom/stat


{% highlight r %}
autoplot(gr, geom = "point", aes(y = score))
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet1.png) 

{% highlight r %}
autoplot(gr, fill = "red")
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet2.png) 

{% highlight r %}
autoplot(gr, aes(fill = value))
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet3.png) 

{% highlight r %}
autoplot(gr, facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet4.png) 

{% highlight r %}
autoplot(gr[idx], geom = "chevron", offset = 1)
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet5.png) 

{% highlight r %}
autoplot(gr[idx], geom = "arrowrect", facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet6.png) 

{% highlight r %}
autoplot(gr[idx], geom = "arrow", facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet7.png) 

{% highlight r %}
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = TRUE)
{% endhighlight %}

![plot of chunk geom/aes/facet](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-geom/aes/facet8.png) 


Group need to be specified in `aes()` use aesthetics `group`, this help to
assign grouped intervals showing on the same y level, especially , when you use
`geom` *alignment*, gaps will be created based on group information and shown on
the plot. A minimal example is shown in the following chunks.


{% highlight r %}
## make a minimal sample .
gra <- GRanges("chr1", IRanges(c(1, 7, 20), end = c(4, 9, 30)), group = c("a", 
    "a", "b"))
## if you desn't specify group, then group based on stepping levels, and
## gaps are computed without considering extra group method
autoplot(gra, aes(fill = group), geom = "alignment")
{% endhighlight %}

![plot of chunk group](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-group1.png) 

{% highlight r %}
## when use group method, gaps only computed for grouped intervals.
## default is group.selfish = TRUE, each group keep one row.  in this way,
## group labels could be shown as y axis.
autoplot(gra, aes(fill = group, group = group), geom = "alignment")
{% endhighlight %}

![plot of chunk group](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-group2.png) 

{% highlight r %}
## group.selfish = FALSE, save space
autoplot(gra, aes(fill = group, group = group), geom = "alignment", 
    group.selfish = FALSE)
{% endhighlight %}

![plot of chunk group](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-group3.png) 

{% highlight r %}
## without group method
autoplot(gra, aes(fill = group), geom = "alignment", group.selfish = FALSE)
{% endhighlight %}

![plot of chunk group](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-group4.png) 


more example


{% highlight r %}
autoplot(gr[idx], geom = "alignment", aes(group = pair), group.selfish = FALSE)
{% endhighlight %}

![plot of chunk group-more](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-group-more.png) 



Faceted by strand help you understand coverage from different sequencing direction. 


{% highlight r %}
autoplot(gr, stat = "coverage", geom = "area", facets = strand ~ 
    seqnames, aes(fill = strand))
{% endhighlight %}

![plot of chunk facet:strand](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-facet:strand.png) 


More stats


{% highlight r %}
autoplot(gr, stat = "coverage", facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk stat](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-stat1.png) 

{% highlight r %}
autoplot(gr[idx], stat = "stepping", facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk stat](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-stat2.png) 

{% highlight r %}
autoplot(gr, stat = "aggregate", y = "value")
{% endhighlight %}

![plot of chunk stat](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-stat3.png) 

{% highlight r %}
## for boxplot, y need to be in aes()
autoplot(gr, stat = "aggregate", aes(y = value), geom = "boxplot", 
    facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk stat](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-stat4.png) 


*New* coordinate transformation "genome" will transform a *GRanges* object into
a genome space, align them up based on `seqlevel` orders. This transformation
allows you to add `seqlengths` to your *GRanges* object to produce a fixed
width. and add buffer in between by specifying `space.skip`. This transformation
is useful for grand linear view as Manhattan plot or circular view.


{% highlight r %}
## simple example
autoplot(gr, coord = "genome")
{% endhighlight %}

![plot of chunk coord:genome](http://tengfei.github.com/ggbio/autoplot/autoplot_GRanges-coord:genome.png) 


Layout circle is another special transformation.




