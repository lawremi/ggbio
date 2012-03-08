---
layout: static
title: geom_rect
---



{% highlight r %}
render_jekyll()
opts_knit$set(imgur.key = "7733c9b660907f0975935cc9ba657413")
opts_knit$set(upload = TRUE)
{% endhighlight %}







{% highlight r %}
set.seed(1)
N <- 100
library(ggbio)
{% endhighlight %}



{% highlight text %}
## Warning message: found methods to import for function 'append' but not the generic itself
{% endhighlight %}



{% highlight text %}
## Warning message: found methods to import for function 'as.factor' but not the generic itself
{% endhighlight %}



{% highlight r %}
library(GenomicRanges)
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






{% highlight r %}
##
#   ======================================================================
##  default
##
#   ======================================================================
ggplot() + geom_rect(gr)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](http://i.imgur.com/XgccG.png) 




{% highlight r %}
##
#   ======================================================================
##  facetting and aesthetics
##
#   ======================================================================
ggplot() + geom_rect(gr, facets = sample ~ seqnames, 
    aes(color = strand, fill = strand))
{% endhighlight %}

![plot of chunk unnamed-chunk-3](http://i.imgur.com/yf8jW.png) 




{% highlight r %}
##
#   ======================================================================
##  stat:identity
##
#   ======================================================================
ggplot() + geom_rect(gr, stat = "identity", aes(y = value))
{% endhighlight %}

![plot of chunk unnamed-chunk-4](http://i.imgur.com/rFx8I.png) 




{% highlight r %}
##
#   ======================================================================
##  stat:stepping
##
#   ======================================================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, 
    group = pair))
{% endhighlight %}

![plot of chunk unnamed-chunk-5](http://i.imgur.com/tfTkj.png) 




{% highlight r %}
##
#   ======================================================================
##  group.selfish controls when
##
#   ======================================================================
ggplot() + geom_rect(gr, stat = "stepping", aes(y = value, 
    group = pair), group.selfish = FALSE)
{% endhighlight %}

![plot of chunk unnamed-chunk-6](http://i.imgur.com/pdQQF.png) 


