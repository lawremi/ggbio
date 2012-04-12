---
layout: post
title: Coordiante "genome" and how to make a Manhattan plot 
category: blog
---




- [Step 0: Introduction](#s0)
- [Step 1: Understand the new coordinate](#s1)
- [Step 2: Simulate a SNP data set](#s2)
- [Step 3: Start to make Manhattan plot by using `autoplot`](#s3)
- [Step 4: Convenient `plotGrandLinear` function](#s4) 


## Step 0: Introduction<a id = "s0"></a>
In this section, we introduce a new coordinate system called "genome" for
genomic data. This transformation is to put all chromosomes on the same genome
coordinates following specified orders and adding buffers in between.  One may
think about facet ability based on `seqnames`, it can produce something similar
to [*Manhattan plot*](http://en.wikipedia.org/wiki/Manhattan), but the view will
not be compact. What's more, genome transformation is previous step  to form a circular
view. In this tutorial, we will simulate some SNP data and use this special
coordinate and a specialized function `plotGrandLinear` to make a Manhattan
plot. 

*Manhattan plot* is just a special use design with this coordinate system.

## Step 1: Understand the new coordinates <a id = "s1"></a>
Let's load some package and data first


{% highlight r %}
library(ggbio)
data(hg19IdeogramCyto, package = "biovizBase")
data(hg19Ideogram, package = "biovizBase")
library(GenomicRanges)
{% endhighlight %}




Make a minimal example `GRanges`, and see what the default looks like, pay
attention that, by default, the graphics are faceted by `seqnames`


{% highlight r %}
library(biovizBase)
gr <- GRanges(rep(c("chr1", "chr2"), each = 5), IRanges(start = rep(seq(1, 
    100, length = 5), times = 2), width = 50))
autoplot(gr)
{% endhighlight %}

![plot of chunk simul_gr](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-simul_gr.png) 


What if we specify the coordinate system to be "genome" in `autoplot`, there is
no faceting anymore, the two plots are merged into one single genome space, and
properly labeled. The internal transformation are implemented into the function
`transformToGenome`, there is a limitation on *integer* in *R*, so the genome
space cannot be too long, to overcome this limitation, a default argument called
`maxSize` is defined with this function, if the genome space is over limits, it
will rescale everything automatically, function `tranformToGenome` with return a
transformed `GRanges` object, with only one single `seqnames` called "genome"
and the `seqlengths` of it, is just genome space(with buffering region). There
will be an column called ".ori" which stored the original data sets, when
`fortify` that object to a `data.frame`, all information there will be coerced
as extra columns but with prefix ".ori.", for example, ".ori.seqnames" is the
original one, you could use this for aesthetics mapping. Extra arguments called
`space.ratio` control the skipped region between chromosomes.


{% highlight r %}
autoplot(gr, coord = "genome")
{% endhighlight %}

![plot of chunk coord:genome](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-coord:genome.png) 

{% highlight r %}
gr.t <- transformToGenome(gr)
head(gr.t)
{% endhighlight %}



{% highlight text %}
## GRanges with 6 ranges and 1 elementMetadata col:
##       seqnames     ranges strand |      .ori
##          <Rle>  <IRanges>  <Rle> | <GRanges>
##   [1]   genome [ 30,  79]      * |  ########
##   [2]   genome [ 54, 103]      * |  ########
##   [3]   genome [ 79, 128]      * |  ########
##   [4]   genome [104, 153]      * |  ########
##   [5]   genome [129, 178]      * |  ########
##   [6]   genome [209, 258]      * |  ########
##   ---
##   seqlengths:
##    genome
##       357
{% endhighlight %}




And there is some simple way to test if a `GRanges` is transformed to coordinate
"genome" or not


{% highlight r %}
is_coord_genome(gr.t)
{% endhighlight %}



{% highlight text %}
## [1] TRUE
{% endhighlight %}



{% highlight r %}
metadata(gr.t)$coord
{% endhighlight %}



{% highlight text %}
## [1] "genome"
{% endhighlight %}





## Step 2: Simulate a SNP data set <a id = "s2"></a>
Let's use the real genome space to simulate a SNP data set.


{% highlight r %}
chrs <- as.character(levels(seqnames(hg19IdeogramCyto)))
seqlths <- seqlengths(hg19Ideogram)[chrs]
set.seed(1)
nchr <- length(chrs)
nsnps <- 100
gr.snp <- GRanges(rep(chrs, each = nsnps), IRanges(start = do.call(c, 
    lapply(chrs, function(chr) {
        N <- seqlths[chr]
        runif(nsnps, 1, N)
    })), width = 1), SNP = sapply(1:(nchr * nsnps), function(x) paste("rs", 
    x, sep = "")), pvalue = -log10(runif(nchr * nsnps)), group = sample(c("Normal", 
    "Tumor"), size = nchr * nsnps, replace = TRUE))
{% endhighlight %}




As introduced in the tutorial about
[processing](http://tengfei.github.com/ggbio/blog/2012/04/11/processing/), we
mentioned how to tweak with chromosome names and orders. Here we use the same
trick to make a shorter names.


{% highlight r %}
seqlengths(gr.snp)
{% endhighlight %}



{% highlight text %}
##  chr1 chr10 chr11 chr12 chr13 chr14 chr15 chr16 chr17 chr18 chr19  chr2 
##    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA 
## chr20 chr21 chr22  chr3  chr4  chr5  chr6  chr7  chr8  chr9  chrX  chrY 
##    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA 
{% endhighlight %}



{% highlight r %}
nms <- seqnames(seqinfo(gr.snp))
nms.new <- gsub("chr", "", nms)
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)
{% endhighlight %}



{% highlight text %}
##  1 10 11 12 13 14 15 16 17 18 19  2 20 21 22  3  4  5  6  7  8  9  X  Y 
## NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA 
{% endhighlight %}




## Step 3: Start to make Manhattan plot by using `autoplot` <a id = "s3"></a> We
wrapped basic functions into `autoplot`, you can specify the coordinate. so what
does the unordered object looks like?


{% highlight r %}
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), 
    space.skip = 0.01)
{% endhighlight %}

![plot of chunk unorder](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-unorder.png) 


That's probably not what you want, if you want to change to specific order, just
sort them by hand and use `keepSeqlevels`.


{% highlight r %}
gr.snp <- keepSeqlevels(gr.snp, c(1:22, "X", "Y"))
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), 
    space.skip = 0.01)
{% endhighlight %}

![plot of chunk sort](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-sort.png) 


**NOTICE**, the data now doesn't have information about lengths of each
  chromosomes, this is allowed to be plotted, but it's misleading sometimes,
  without chromosomes lengths information, *ggbio* use data space to make
  estimated lengths for you, this is not accurate! So let's just assign
  `seqlengths` to the object. Then you will find the data space now is
  distributed proportional to real space.


{% highlight r %}
names(seqlths) <- gsub("chr", "", names(seqlths))
seqlths
{% endhighlight %}



{% highlight text %}
##         1        10        11        12        13        14        15 
## 249250621 135534747 135006516 133851895 115169878 107349540 102531392 
##        16        17        18        19         2        20        21 
##  90354753  81195210  78077248  59128983 243199373  63025520  48129895 
##        22         3         4         5         6         7         8 
##  51304566 198022430 191154276 180915260 171115067 159138663 146364022 
##         9         X         Y 
## 141213431 155270560  59373566 
{% endhighlight %}



{% highlight r %}
seqlengths(gr.snp) <- seqlths
{% endhighlight %}



{% highlight text %}
## Error: when the supplied 'seqlengths' vector is named, the names must match the seqnames
{% endhighlight %}



{% highlight r %}
autoplot(gr.snp, coord = "genome", geom = "point", aes(y = pvalue), 
    space.skip = 0.01)
{% endhighlight %}

![plot of chunk with_seql](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-with_seql.png) 


In `autoplot`, argument `coord` is just used to transform the data, after that,
you can use it as common `GRanges`, all other geom/stat works for it. Here just
show a simple example for another geom "line"


{% highlight r %}
autoplot(gr.snp, coord = "genome", geom = "line", aes(y = pvalue, 
    group = .ori.seqnames, color = .ori.seqnames))
{% endhighlight %}

![plot of chunk line](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-line.png) 


## Step 4: Convenient `plotGrandLinear` function <a id = "s4"></a>
In *ggbio*, sometimes we develop specialized function for certain types of
plots, it's basically a wrapper over lower level API and `autoplot`,  but more
convenient to use. Here for *Manhattan plot*, we have a function called
`plotGrandLinear` used for it. aes(y = ) is required.


{% highlight r %}
plotGrandLinear(gr.snp, y = pvalue)
{% endhighlight %}



{% highlight text %}
## Error: object 'pvalue' not found
{% endhighlight %}




Color mapping is automatically figured out by *ggbio* following the rules

- if `color` present in `aes()`, like `aes(color = seqnames)`, it will assume
  it's mapping to data column.
- if `color` is not wrapped in `aes()`, then this function will *recylcle* them
  to all chromosomes. 
- if `color` is single character, then just use one arbitrary color



{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue))
{% endhighlight %}

![plot of chunk morecolor](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-morecolor1.png) 

{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue, color = seqnames))
{% endhighlight %}

![plot of chunk morecolor](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-morecolor2.png) 

{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue"))
{% endhighlight %}

![plot of chunk morecolor](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-morecolor3.png) 

{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue), color = c("green", "deepskyblue", 
    "red"))
{% endhighlight %}

![plot of chunk morecolor](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-morecolor4.png) 

{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue), color = "red")
{% endhighlight %}

![plot of chunk morecolor](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-morecolor5.png) 


You can also add cutoff line


{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue), cutoff = 3, cutoff.color = "blue", 
    cutoff.size = 4)
{% endhighlight %}

![plot of chunk cutoff](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-cutoff.png) 


This is equivalent to 


{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue)) + geom_hline(yintercept = 3, 
    color = "blue", size = 4)
{% endhighlight %}

![plot of chunk cutoff-low](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-cutoff-low.png) 




Sometimes the names of chromosomes maybe very long, you may want to rotate them, 
let's make a longer name first


{% highlight r %}
## let's make a long name
nms <- seqnames(seqinfo(gr.snp))
nms.new <- paste("chr00000", nms, sep = "")
names(nms.new) <- nms
gr.snp <- renameSeqlevels(gr.snp, nms.new)
seqlengths(gr.snp)
{% endhighlight %}



{% highlight text %}
##  chr000001  chr000002  chr000003  chr000004  chr000005  chr000006 
##         NA         NA         NA         NA         NA         NA 
##  chr000007  chr000008  chr000009 chr0000010 chr0000011 chr0000012 
##         NA         NA         NA         NA         NA         NA 
## chr0000013 chr0000014 chr0000015 chr0000016 chr0000017 chr0000018 
##         NA         NA         NA         NA         NA         NA 
## chr0000019 chr0000020 chr0000021 chr0000022  chr00000X  chr00000Y 
##         NA         NA         NA         NA         NA         NA 
{% endhighlight %}




Then rotate it!


{% highlight r %}
plotGrandLinear(gr.snp, aes(y = pvalue)) + opts(axis.text.x = theme_text(angle = -90, 
    hjust = 0))
{% endhighlight %}

![plot of chunk rotate](https://github.com/tengfei/ggbio/raw/gh-pages/_posts/manhattan-rotate.png) 

all utilities works for *ggplot2* will work for *ggbio* too.

sessionInfo


{% highlight r %}
sessionInfo()
{% endhighlight %}



{% highlight text %}
## R Under development (unstable) (2012-04-07 r58925)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=zh_CN.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=C                 LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] methods   stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] ggbio_1.3.0         ggplot2_0.9.0       rtracklayer_1.16.0 
## [4] biovizBase_1.3.0    GenomicRanges_1.8.3 IRanges_1.14.2     
## [7] BiocGenerics_0.2.0  knitr_0.4          
## 
## loaded via a namespace (and not attached):
##  [1] AnnotationDbi_1.18.0    Biobase_2.16.0         
##  [3] biomaRt_2.12.0          Biostrings_2.24.1      
##  [5] bitops_1.0-4.1          BSgenome_1.24.0        
##  [7] cluster_1.14.2          codetools_0.2-8        
##  [9] colorspace_1.1-1        DBI_0.2-5              
## [11] dichromat_1.2-4         digest_0.5.2           
## [13] evaluate_0.4.2          formatR_0.4            
## [15] GenomicFeatures_1.8.1   grid_2.16.0            
## [17] gridExtra_0.9           highlight_0.3.1        
## [19] Hmisc_3.9-3             lattice_0.20-6         
## [21] MASS_7.3-17             Matrix_1.0-6           
## [23] memoise_0.1             munsell_0.3            
## [25] parser_0.0-14           plyr_1.7.1             
## [27] proto_0.3-9.2           RColorBrewer_1.0-5     
## [29] Rcpp_0.9.10             RCurl_1.91-1           
## [31] reshape2_1.2.1          Rsamtools_1.8.0        
## [33] RSQLite_0.11.1          scales_0.2.0           
## [35] snpStats_1.6.0          splines_2.16.0         
## [37] stats4_2.16.0           stringr_0.6            
## [39] survival_2.36-12        tools_2.16.0           
## [41] VariantAnnotation_1.2.5 XML_3.9-4              
## [43] zlibbioc_1.2.0         
{% endhighlight %}










