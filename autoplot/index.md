---
layout: static
title: autoplot
---

**autoplot** is a generic function defined inside *ggbio*, which aim to be the
  most useful API for creating both specialized plots and wrapping lower level
  API. It wraps the whole idea about the grammar of graphics and it's extension
  in genomic world into single function, which means users are able to control
  coordinate system, layout, and aesthetics mapping, etc in one single pass, if
  you are familiar with *ggplot2*, it's kind of like `qplot` function. Most
  important difference, is that `autoplot` can dispatch different visualization
  method based on different objects.
  
  currently supported objects:
  
  -  [autoplot, GRanges]({{site.url}}/autoplot/autoplot_GRanges)
  -  [autoplot, GRangesList]({{site.url}}/autoplot/autoplot_GRangesList)
  -  [autoplot, IRanges]({{site.url}}/autoplot/autoplot_IRanges)
  -  [autoplot, TranscriptDb]({{site.url}}/autoplot/autoplot_TranscriptDb)
  -  [autoplot, BamFile]({{site.url}}/autoplot/autoplot_BamFile)
  -  [autoplot, GappedAlignment]({{site.url}}/autoplot/autoplot_GappedAlignment)
  -  [autoplot, BSgenome]({{site.url}}/autoplot/autoplot_BSgenome)
  -  [autoplot, Rle]({{site.url}}/autoplot/autoplot_Rle)
  -  [autoplot, RleList]({{site.url}}/autoplot/autoplot_RleList)
  -  [autoplot, character]({{site.url}}/autoplot/autoplot_character)
  
  



