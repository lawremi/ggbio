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




