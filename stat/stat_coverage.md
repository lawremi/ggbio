---
layout: static
title: stat_coverage
---




### Introduction

`stat_coverage` is lower level API computing coverage from raw bam file,
*GRanges*, and *GRangesList*. For *Bamfile*, a fast estimated method has been
implemented by Michael Lawrence in package *biovizBase* and wrapped as one
method in `stat_coverage`.

### Objects
  * *GRanges*
  * *GRangesList* 
  * *BamFile*
  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
{% endhighlight %}



{% highlight text %}
## Loading required package: methods
{% endhighlight %}



{% highlight text %}
## Loading required package: ggplot2
{% endhighlight %}



{% highlight text %}
## 
## Attaching package: 'ggbio'
## 
{% endhighlight %}



{% highlight text %}
## The following object(s) are masked from 'package:ggplot2':
## 
##     geom_rect, geom_segment, stat_identity, xlim
## 
{% endhighlight %}



  
Let's generate some simulated interval data and store it as *GRanges* object.


{% highlight r %}
## ======================================================================
## simmulated GRanges
## ======================================================================
set.seed(1)
N <- 1000
library(GenomicRanges)
{% endhighlight %}



{% highlight text %}
## Loading required package: BiocGenerics
{% endhighlight %}



{% highlight text %}
## 
## Attaching package: 'BiocGenerics'
## 
{% endhighlight %}



{% highlight text %}
## The following object(s) are masked from 'package:stats':
## 
##     xtabs
## 
{% endhighlight %}



{% highlight text %}
## The following object(s) are masked from 'package:base':
## 
##     anyDuplicated, cbind, colnames, duplicated, eval, Filter,
##     Find, get, intersect, lapply, Map, mapply, mget, order, paste,
##     pmax, pmax.int, pmin, pmin.int, Position, rbind, Reduce,
##     rep.int, rownames, sapply, setdiff, table, tapply, union,
##     unique
## 
{% endhighlight %}



{% highlight text %}
## Loading required package: IRanges
{% endhighlight %}



{% highlight r %}
gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, 
    replace = TRUE), IRanges(start = sample(1:300, size = N, replace = TRUE), 
    width = sample(70:75, size = N, replace = TRUE)), strand = sample(c("+", 
    "-", "*"), size = N, replace = TRUE), value = rnorm(N, 10, 3), score = rnorm(N, 
    100, 30), sample = sample(c("Normal", "Tumor"), size = N, replace = TRUE), 
    pair = sample(letters, size = N, replace = TRUE))
{% endhighlight %}





Test different geom, notice `..coverage..` is a variable name that is not in
original data but in transformed data, if you hope to use this new statistics,
please you `..` to wrap around `coverage`, it indicates it belongs to interval
variable. 


{% highlight r %}
ggplot() + stat_coverage(gr)
{% endhighlight %}

![plot of chunk geom](stat_coverage-geom1.png) 

{% highlight r %}
ggplot() + stat_coverage(gr, geom = "point")
{% endhighlight %}

![plot of chunk geom](stat_coverage-geom2.png) 

{% highlight r %}
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "histogram")
{% endhighlight %}

![plot of chunk geom](stat_coverage-geom3.png) 

{% highlight r %}
ggplot() + stat_coverage(gr, aes(y = ..coverage..), geom = "area")
{% endhighlight %}

![plot of chunk geom](stat_coverage-geom4.png) 

{% highlight r %}
ggplot() + stat_coverage(gr, geom = "smooth")
{% endhighlight %}

![plot of chunk geom](stat_coverage-geom5.png) 


Facetting, column for `seqnames` is requried.


{% highlight r %}
ggplot() + stat_coverage(gr, geom = "line", facets = sample ~ seqnames)
{% endhighlight %}

![plot of chunk facet:sample](stat_coverage-facet:sample.png) 


Faceted by strand help you understand coverage from different sequencing direction. 


{% highlight r %}
ggplot() + stat_coverage(gr, geom = "line", facets = strand ~ seqnames)
{% endhighlight %}

![plot of chunk facet:strand](stat_coverage-facet:strand.png) 


Let's create a *GRangesList* object.


{% highlight r %}
grl <- split(gr, values(gr)$sample)
grl <- endoapply(grl, function(gr) {
    nms <- setdiff(colnames(values(gr)), "sample")
    values(gr) <- values(gr)[nms]
    gr
})
{% endhighlight %}




For *GRangesList* object, default is coerce it to *GRanges*.


{% highlight r %}
ggplot() + stat_coverage(grl)
{% endhighlight %}

![plot of chunk grl:default](stat_coverage-grl:default.png) 


Internal variable `..grl_name..` added to keep a track for grouping information,
you could use it for faceting or other mapping.


{% highlight r %}
ggplot() + stat_coverage(grl, geom = "area", facets = ..grl_name.. ~ 
    seqnames, aes(fill = ..grl_name..))
{% endhighlight %}

![plot of chunk grl:facet](stat_coverage-grl:facet.png) 


Load a RNA-seq data


{% highlight r %}
library(Rsamtools)
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)
{% endhighlight %}




Default method is "estimate", which is very fast and efficient estimation for
whole genome, if you didn't provide which, we only show the first chromosome.



If you really want to get accurate coverage information on the fly, use method
"raw", make sure you provide a relatively small region for `which` argument,
otherwise, it's going to be very slow. Internally it parse short reads to
*GRanges* and then calling coverage on it.








