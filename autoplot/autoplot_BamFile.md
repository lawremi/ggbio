---
layout: static
title: autoplot_BamFile
---




### Introduction

`autoplot` for *BamFile*. Default is using esimated coverage(fast) and the first
chromosome presented in the bam file header unless you specify a specific region
you want to visualize.

_For aesthetics mapping now, users have to pass them to aes() functions and
pass it into autoplot_, for example
    
	autoplot(data, color = score)
	
won't work, you have to use 

    autoplot(data, aes(color = score))
	
for now.	

### Objects
  * *BamFile*
  
### Usage
  upcomming

### Examples
Load packages and read an example bam file


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



{% highlight r %}
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
library(Rsamtools)
{% endhighlight %}



{% highlight text %}
## Loading required package: Biostrings
{% endhighlight %}



{% highlight r %}
bamfile <- "~/Datas/seqs/ENCODE/caltech/single/wgEncodeCaltechRnaSeqK562R1x75dAlignsRep1V2.bam"
bf <- BamFile(bamfile)
{% endhighlight %}




Default method for stat "coverage" is "estiamted".


{% highlight r %}
p1 <- autoplot(bamfile, geom = "line", method = "estimate")
{% endhighlight %}



{% highlight text %}
## reading in as Bamfile
{% endhighlight %}



{% highlight text %}
## Estimating coverage...
{% endhighlight %}



{% highlight text %}
## Constructing graphics...
{% endhighlight %}




method "raw" is way slow and need to provide a small region to parsing the raw
files to a set of short reads stored as *GRanges* object, then make coverage
transformation. 


{% highlight r %}
data(genesymbol, package = "biovizBase")
p2 <- autoplot(bamfile, method = "raw", which = genesymbol["ALDOA"])
{% endhighlight %}



{% highlight text %}
## reading in as Bamfile
{% endhighlight %}



{% highlight text %}
## Parsing raw coverage...
{% endhighlight %}



{% highlight text %}
## Read GappedAlignments from BamFile...
{% endhighlight %}




stat "mismatch" will generate mismatch summary, it's a wrapper for lower level
API, `stat_mismatch`.


{% highlight r %}
library(BSgenome.Hsapiens.UCSC.hg19)
{% endhighlight %}



{% highlight text %}
## Loading required package: BSgenome
{% endhighlight %}



{% highlight r %}
autoplot(bf, stat = "mismatch", which = genesymbol["ALDOA"], bsgenome = Hsapiens)
{% endhighlight %}

![plot of chunk mismatch](autoplot_BamFile-mismatch1.png) 

{% highlight r %}

## Fixme
autoplot(bf, geom = "gapped.pair", which = genesymbol["ALDOA"])
{% endhighlight %}



{% highlight text %}
## Read GappedAlignments from BamFile...
{% endhighlight %}



{% highlight text %}
## plotting...
{% endhighlight %}

![plot of chunk mismatch](autoplot_BamFile-mismatch2.png) 


If you specify other geom and stat, this will simply parse a set of short reads
and use `autoplot` for `GRanges` instead, so extra arguments could be provided
too.


{% highlight r %}
library(biovizBase)
autoplot(bf, geom = "segment", stat = "stepping", which = genesymbol["ALDOA"])
{% endhighlight %}

![plot of chunk other](autoplot_BamFile-other.png) 



