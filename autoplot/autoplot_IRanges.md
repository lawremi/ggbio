---
layout: static
title: autoplot_IRanges
---




### Introduction

`autoplot` for *IRanges* is used to visualize simple interval data with element
data together.

Supported geom designed specifically for *IRanges*, including "rect", "chevron",
"alignment", "arrowrect", "arrow", "segment", "arch", and special statistical
transformation contains "identity", "coverage", "stepping", "aggregate",
"table", "gene", "mismatch". And they are implemented in lower API, such as
`geom_alignment` and `stat_coverage`. If you pass other `geom` and `stat` other
than those ones, it first coerces a *IRanges* into a `data.frame` object
*together* with extra element meta data, and added to `data.frame`.


Inside, `autoplot` will choose the best choice for your combination of `geom`
and `stat`.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *IRanges*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
library(GenomicRanges)
set.seed(1)
N <- 200
{% endhighlight %}



  
Let's generate some simulated interval data and store it as *IRanges*
object. and add some element meta data.


{% highlight r %}
## ======================================================================
## simmulated GRanges
## ======================================================================
ir <- IRanges(start = sample(1:300, size = N, replace = TRUE), width = sample(70:75, 
    size = N, replace = TRUE))
## add meta data
df <- DataFrame(value = rnorm(N, 10, 3), score = rnorm(N, 100, 30), 
    sample = sample(c("Normal", "Tumor"), size = N, replace = TRUE), pair = sample(letters, 
        size = N, replace = TRUE))
values(ir) <- df
{% endhighlight %}




`autoplot` will coerce *IRanges* together with its element meta data, so
aesthetics mapping works for those extra information too.


{% highlight r %}
autoplot(ir)
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp1.png) 

{% highlight r %}
autoplot(ir, aes(fill = pair))
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp2.png) 

{% highlight r %}
autoplot(ir, stat = "coverage", geom = "line")
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp3.png) 

{% highlight r %}
autoplot(ir, stat = "coverage", geom = "point")
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp4.png) 

{% highlight r %}
autoplot(ir, stat = "coverage", geom = "line", facets = sample ~ 
    .)
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp5.png) 

{% highlight r %}
autoplot(ir, stat = "boxplot", aes(y = score, x = sample))
{% endhighlight %}

![plot of chunk exp](http://tengfei.github.com/ggbio/autoplot/autoplot_IRanges-exp6.png) 


