---
layout: static
title: theme_alignment
---




### Introduction
`theme_alignment` is designed for creating appropriate themes for interval
data. This return a option list like normal `theme_bw` in *ggplot2* package. 


### Usage
  upcomming
  
### Examples
Load packages, get a `genesymbol` data set from package *biovizBase*, and load
gene features from package *TxDb.Hsapiens.UCSC.hg19.knownGene*.


{% highlight r %}
library(ggbio)
data(genesymbol, package = "biovizBase")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
{% endhighlight %}




Default theme is with gray background, it's actually a preset theme in
*ggplot2*, called `theme_gray`.


{% highlight r %}
p <- autoplot(txdb, which = genesymbol["RBM17"])
p
{% endhighlight %}

![plot of chunk theme:default](http://i.imgur.com/23vDy.png) 


Compare to `theme_gray`, here is how we make flexible tweak with themes with
`theme_alignment`. It's not limited to alignments, since in *ggbio*, most tracks
are linear interval alignments sitting on the genomic coordinates, that's the
reason I called it `theme_alignment`.


{% highlight r %}
p + theme_alignment(border = TRUE, grid = FALSE, 
    label = TRUE)
{% endhighlight %}

![plot of chunk theme:alignment](http://i.imgur.com/B9KdA.png) 

