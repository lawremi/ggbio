---
layout: static
title: autoplot_GRangesList
---




### Introduction

*GRangesList* is most suitable data structure for storing a set of genomic
 features, for example, exons/utrs in a gene. `autoplot` is designed to consider
 the native grouping information in this structure and automatically showing
 gaps within group in `geom` *alignment* and make sure grouped items are shown
 together on the same level with nothing falling in between.

`main.geom` and `gap.geom` control geometry for entities and gaps computed for
them. `group.selfish` help you put grouped items in unique y levels and show the
y labels for group names.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *GRangesList*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
library(GenomicRanges)
{% endhighlight %}



  
Let's create a *GRangesList* object.


{% highlight r %}
set.seed(1)
N <- 100
## ======================================================================
## simmulated GRanges
## ======================================================================
gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, 
    replace = TRUE), IRanges(start = sample(1:300, size = N, replace = TRUE), 
    width = sample(30:40, size = N, replace = TRUE)), strand = sample(c("+", 
    "-", "*"), size = N, replace = TRUE), value = rnorm(N, 10, 3), score = rnorm(N, 
    100, 30), sample = sample(c("Normal", "Tumor"), size = N, replace = TRUE), 
    pair = sample(letters, size = N, replace = TRUE))
grl <- split(gr, values(gr)$pair)
{% endhighlight %}




For *GRangesList* object, default is coerce it to *GRanges* and adding extra
column to preserve the grouping information. main geoms and gaps geom are
separately controlled.


{% highlight r %}
autoplot(grl)
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_GRangesList-exp1.png) 

{% highlight r %}
autoplot(grl, group.selfish = TRUE)
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_GRangesList-exp2.png) 

{% highlight r %}
autoplot(grl, group.selfish = TRUE, main.geom = "arrowrect", gap.geom = "segment")
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_GRangesList-exp3.png) 


Internal variable `grl_name` added to keep a track for grouping information, you
could use it for faceting or other aesthetic mapping, the variables could be
renamed by `indName` argument in `autoplot`, you could pass either
`..grl_name..` or `grl_name` in the mapping, I prefer the first one, it tells
that it's interval variables.


{% highlight r %}
autoplot(grl, aes(fill = ..grl_name..))
{% endhighlight %}

![plot of chunk grl_name](http://tengfei.github.com/ggbio/autoplot/autoplot_GRangesList-grl_name.png) 

{% highlight r %}
## equal to autoplot(grl, aes(fill = grl_name))
{% endhighlight %}




Load a RNA-seq data



Default method is "estimate", which is very fast and efficient estimation for
whole genome, if you didn't provide which, we only show the first chromosome.



If you really want to get accurate coverage information on the fly, use method
"raw", make sure you provide a relatively small region for `which` argument,
otherwise, it's going to be very slow. Internally it parse short reads to
*GRanges* and then calling coverage on it.








