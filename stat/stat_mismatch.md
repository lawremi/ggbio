---
layout: static
title: stat_mismatch
---




### Introduction

`stat_mismatch` is lower level API to read in a bam file and show mismatch
summary for certain region, counts at each position are summarized, those reads
which are identical as reference will be either shown as gray background or
removed, it's controled by argument `show.coverage`, mismatched part will be
shown as color-coded bar or segment.

### Objects
  * *Bamfile*
  * *GRanges*, this will pass interval checking which make sure the GRanges has
     required columns.

  
### Usage
  upcomming

### Examples
Load packages


{% highlight r %}
library(ggbio)
library(BSgenome.Hsapiens.UCSC.hg19)
data("genesymbol", package = "biovizBase")
{% endhighlight %}



  
Load example bam file


{% highlight r %}
bamfile <- system.file("extdata", "SRR027894subRBM17.bam", 
    package = "biovizBase")
library(Rsamtools)
bf <- BamFile(bamfile)
{% endhighlight %}




If the object is *BamFile*, a *BSgenome* object is required to compute the
mismatch summary. 


{% highlight r %}
ggplot() + stat_mismatch(bf, which = genesymbol["RBM17"], 
    bsgenome = Hsapiens, show.coverage = TRUE) + coord_cartesian(xlim = c(6134000, 
    6135000), wise = TRUE) + theme_bw()
{% endhighlight %}

![plot of chunk BamFile](http://i.imgur.com/TgA52.png) 


Sometimes bam file and *BSgenome* object might have a different naming schema
for chromosomes, currently, `stat_mismatch` is not smart enough to deal with
complicated cases, in this way, you may want to get mismatch summary as
*GRanges* yourself and correct the names, with `keepSeqlevels` or
`renamesSeqleves` functions in package *GenomicRanges*. Following examples
doesn't show you how to manipulate seqnames, but just show you how to compute
mismatch summary. 


{% highlight r %}
library(biovizBase)
pgr <- pileupAsGRanges(bamfile, region = genesymbol["RBM17"])
pgr.match <- pileupGRangesAsVariantTable(pgr, 
    genome = Hsapiens)
{% endhighlight %}




And directly plot the mismatch *GRanges* object.


{% highlight r %}
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE)
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/T2DSp.png) 

{% highlight r %}
ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE)
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/4B3kW.png) 

{% highlight r %}
ggplot() + stat_mismatch(pgr.match, show.coverage = FALSE, 
    geom = "bar")
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/3t24Q.png) 

{% highlight r %}


## ggplot() + stat_mismatch(pgr.match, show.coverage =
#   TRUE) +
## coord_cartesian(xlim = c(6134000, 6135000),wise =
#   TRUE) + theme_bw()
library(ggbio)
library(Rsamtools)
bamfile <- "~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam"
bf <- BamFile(bamfile)
data(genesymbol, package = "biovizBase")

rng.ori <- genesymbol["PYGB"]
rng <- GRanges("20", ranges(rng.ori))
rng
{% endhighlight %}



{% highlight text %}
## GRanges with 1 range and 0 elementMetadata cols:
##        seqnames               ranges strand
##           <Rle>            <IRanges>  <Rle>
##   PYGB       20 [25228706, 25278647]      *
##   ---
##   seqlengths:
##    20
##    NA
{% endhighlight %}



{% highlight r %}
## red vcf
library(VariantAnnotation)
svp_all <- ScanVcfParam(which = rng)
vcf <- readVcf("~/Datas/1000genome/ALL.chr20.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz", 
    genome = "hg19", svp_all)
p.v <- autoplot(vcf, type = "fixed") + coord_cartesian(ylim = c(0.6, 
    1.4), wise = TRUE) + scale_y_continuous(breaks = NULL) + 
    opts(legend.position = "none")

ggplot() + stat_mismatch(bf, which = rng, bsgenome = Hsapiens, 
    show.coverage = TRUE)
{% endhighlight %}



{% highlight text %}
## Error: sequence 20 not found
{% endhighlight %}



{% highlight r %}
coord_cartesian(xlim = c(6134000, 6135000), wise = TRUE) + 
    theme_bw()
{% endhighlight %}



{% highlight text %}
## Error: non-numeric argument to binary operator
{% endhighlight %}



{% highlight r %}


##
gr.t <- GRanges("chr16", IRanges(30080000, 30080000 + 
    2000))
p0 <- ggplot() + stat_mismatch(bf, which = gr.t, 
    bsgenome = Hsapiens, show.coverage = TRUE, geom = "bar") + 
    ylab("Coverage") + ## coord_cartesian(ylim = c(0, 200), wise = TRUE) +
theme_bw()
{% endhighlight %}



{% highlight text %}
## Error: applyPileups: 'chr16' not in bam file 1
{% endhighlight %}



{% highlight r %}
library(biovizBase)

pgr <- pileupAsGRanges(bamfile, region = rng)
nms <- "chr20"
names(nms) <- "20"
pgr <- renameSeqlevels(pgr, nms)
pgr.match <- pileupGRangesAsVariantTable(pgr, 
    genome = Hsapiens)
pgr.match
{% endhighlight %}



{% highlight text %}
## GRanges with 48898 ranges and 6 elementMetadata cols:
##           seqnames               ranges strand   |         ref        read
##              <Rle>            <IRanges>  <Rle>   | <character> <character>
##       [1]    chr20 [25228706, 25228706]      +   |           G           G
##       [2]    chr20 [25228707, 25228707]      +   |           C           C
##       [3]    chr20 [25228708, 25228708]      +   |           A           A
##       [4]    chr20 [25228709, 25228709]      +   |           G           G
##       [5]    chr20 [25228710, 25228710]      +   |           T           T
##       [6]    chr20 [25228711, 25228711]      +   |           G           G
##       [7]    chr20 [25228712, 25228712]      +   |           C           C
##       [8]    chr20 [25228713, 25228713]      +   |           C           C
##       [9]    chr20 [25228714, 25228714]      +   |           G           G
##       ...      ...                  ...    ... ...         ...         ...
##   [48890]    chr20 [25278639, 25278639]      +   |           G           G
##   [48891]    chr20 [25278640, 25278640]      +   |           G           G
##   [48892]    chr20 [25278641, 25278641]      +   |           T           T
##   [48893]    chr20 [25278642, 25278642]      +   |           T           T
##   [48894]    chr20 [25278643, 25278643]      +   |           G           G
##   [48895]    chr20 [25278644, 25278644]      +   |           G           G
##   [48896]    chr20 [25278645, 25278645]      +   |           C           C
##   [48897]    chr20 [25278646, 25278646]      +   |           T           T
##   [48898]    chr20 [25278647, 25278647]      +   |           G           G
##               count     depth
##           <integer> <numeric>
##       [1]         1         1
##       [2]         1         1
##       [3]         1         1
##       [4]         1         1
##       [5]         1         1
##       [6]         1         1
##       [7]         1         1
##       [8]         1         1
##       [9]         1         1
##       ...       ...       ...
##   [48890]         4         4
##   [48891]         4         4
##   [48892]         4         4
##   [48893]         4         4
##   [48894]         4         4
##   [48895]         4         4
##   [48896]         4         4
##   [48897]         4         4
##   [48898]         4         4
##                                                                                     bam
##                                                                             <character>
##       [1] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [2] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [3] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [4] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [5] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [6] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [7] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [8] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       [9] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##       ...                                                                           ...
##   [48890] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48891] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48892] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48893] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48894] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48895] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48896] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48897] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##   [48898] ~/Datas/1000genome/HG00096.chrom20.ILLUMINA.bwa.GBR.low_coverage.20101123.bam
##               match
##           <logical>
##       [1]      TRUE
##       [2]      TRUE
##       [3]      TRUE
##       [4]      TRUE
##       [5]      TRUE
##       [6]      TRUE
##       [7]      TRUE
##       [8]      TRUE
##       [9]      TRUE
##       ...       ...
##   [48890]      TRUE
##   [48891]      TRUE
##   [48892]      TRUE
##   [48893]      TRUE
##   [48894]      TRUE
##   [48895]      TRUE
##   [48896]      TRUE
##   [48897]      TRUE
##   [48898]      TRUE
##   ---
##   seqlengths:
##    chr20
##       NA
{% endhighlight %}



{% highlight r %}
p.v
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/tOBuV.png) 

{% highlight r %}
p1 <- ggplot() + stat_mismatch(pgr.match, show.coverage = TRUE) + 
    coord_cartesian(ylim = c(0, 10), wise = TRUE)
p.v
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/NrLSk.png) 

{% highlight r %}
p3 <- autoplot(Hsapiens, which = rng.ori) + opts(legend.position = "none")
p3
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/stqRd.png) 

{% highlight r %}
obj <- tracks(p1, p.v, p3, heights = c(4, 0.9, 
    1), xlim = c(25235400, 25236100))

obj <- tracks(p1, p.v, p3, heights = c(4, 0.9, 
    1), xlim = c(25235720, 25235850))
obj
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/WrDmR.png) 

{% highlight r %}
pdf("~/Desktop/mismatch.pdf", 18.3, 5.98)
obj
dev.off()
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/HfMFE.png) 

{% highlight text %}
## pdf 
##   2 
{% endhighlight %}



{% highlight r %}

obj
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/zGvbT.png) 

{% highlight r %}
update(obj, xlim = c(25238120, 25238470))
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/v4Aki.png) 

{% highlight text %}
## NULL
{% endhighlight %}



{% highlight r %}
update(obj, xlim = c(25235400, 25236100))
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/NHA0V.png) 

{% highlight text %}
## NULL
{% endhighlight %}



{% highlight r %}
ggbio:::reset(obj)
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/pATAZ.png) 

{% highlight r %}

p2 <- autoplot(Hsapiens, which = gr.t, geom = "text") + 
    theme_bw() + opts(legend.position = "none") + xlim(c(30080800 + 
    20, 30080800 + 100))

pdf("~/Desktop/mismatch.pdf", 13.1, 5.8)
tracks(p0, p1, p2, heights = c(4, 4, 1.5), xlim = c(30080800 + 
    20, 30080800 + 100))
{% endhighlight %}



{% highlight text %}
## Error: object 'p0' not found
{% endhighlight %}



{% highlight r %}
dev.off()
{% endhighlight %}

![plot of chunk pag_v](http://i.imgur.com/s3uQc.png) 

{% highlight text %}
## pdf 
##   2 
{% endhighlight %}



{% highlight r %}
pg.sub <- pgr.match[!values(pgr.match)$match]
pg.s <- start(pg.sub)
vcf.s <- start(alt(vcf))
ss <- pg.s[!is.na(match(pg.s, vcf.s))]
as.character(unlist(values(alt(vcf)[start(alt(vcf)) == 
    25238173])$ALT))
{% endhighlight %}



{% highlight text %}
## [1] "C"
{% endhighlight %}



{% highlight r %}
as.character(unlist(values(alt(vcf)[match(ss, 
    vcf.s)])$ALT)) == values(pg.sub[!is.na(match(pg.s, vcf.s)), 
    "read"])[, 1]
{% endhighlight %}



{% highlight text %}
##  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [12]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [23]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [34]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [45]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [56]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [67]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [78]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
## [89]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
{% endhighlight %}



{% highlight r %}
as.data.frame(pg.sub[!is.na(match(pg.s, vcf.s)), 
    "read"])
{% endhighlight %}



{% highlight text %}
##    seqnames    start      end width strand read
## 1     chr20 25229564 25229564     1      +    A
## 2     chr20 25231858 25231858     1      +    A
## 3     chr20 25232097 25232097     1      +    C
## 4     chr20 25232300 25232300     1      +    G
## 5     chr20 25232406 25232406     1      +    G
## 6     chr20 25232604 25232604     1      +    G
## 7     chr20 25233985 25233985     1      +    T
## 8     chr20 25234166 25234166     1      +    T
## 9     chr20 25234812 25234812     1      +    A
## 10    chr20 25235129 25235129     1      +    A
## 11    chr20 25235736 25235736     1      +    T
## 12    chr20 25236399 25236399     1      +    C
## 13    chr20 25236785 25236785     1      +    G
## 14    chr20 25237145 25237145     1      +    A
## 15    chr20 25237338 25237338     1      +    G
## 16    chr20 25237390 25237390     1      +    G
## 17    chr20 25238173 25238173     1      +    C
## 18    chr20 25238309 25238309     1      +    T
## 19    chr20 25239551 25239551     1      +    A
## 20    chr20 25239667 25239667     1      +    A
## 21    chr20 25240189 25240189     1      +    T
## 22    chr20 25241083 25241083     1      +    G
## 23    chr20 25241155 25241155     1      +    C
## 24    chr20 25241295 25241295     1      +    G
## 25    chr20 25241345 25241345     1      +    G
## 26    chr20 25241669 25241669     1      +    C
## 27    chr20 25242491 25242491     1      +    T
## 28    chr20 25242804 25242804     1      +    A
## 29    chr20 25242910 25242910     1      +    A
## 30    chr20 25243305 25243305     1      +    T
## 31    chr20 25243310 25243310     1      +    T
## 32    chr20 25244101 25244101     1      +    G
## 33    chr20 25244120 25244120     1      +    A
## 34    chr20 25244544 25244544     1      +    G
## 35    chr20 25245950 25245950     1      +    T
## 36    chr20 25246562 25246562     1      +    G
## 37    chr20 25246733 25246733     1      +    C
## 38    chr20 25246734 25246734     1      +    A
## 39    chr20 25246766 25246766     1      +    T
## 40    chr20 25247449 25247449     1      +    A
## 41    chr20 25248725 25248725     1      +    T
## 42    chr20 25248744 25248744     1      +    G
## 43    chr20 25248854 25248854     1      +    G
## 44    chr20 25249734 25249734     1      +    G
## 45    chr20 25250577 25250577     1      +    C
## 46    chr20 25255205 25255205     1      +    T
## 47    chr20 25256106 25256106     1      +    T
## 48    chr20 25256266 25256266     1      +    T
## 49    chr20 25256598 25256598     1      +    G
## 50    chr20 25258608 25258608     1      +    A
## 51    chr20 25259200 25259200     1      +    C
## 52    chr20 25259807 25259807     1      +    A
## 53    chr20 25259910 25259910     1      +    G
## 54    chr20 25260641 25260641     1      +    G
## 55    chr20 25260931 25260931     1      +    G
## 56    chr20 25261784 25261784     1      +    A
## 57    chr20 25262291 25262291     1      +    A
## 58    chr20 25262396 25262396     1      +    C
## 59    chr20 25262403 25262403     1      +    T
## 60    chr20 25263062 25263062     1      +    G
## 61    chr20 25263136 25263136     1      +    C
## 62    chr20 25263277 25263277     1      +    G
## 63    chr20 25263457 25263457     1      +    G
## 64    chr20 25264196 25264196     1      +    G
## 65    chr20 25264221 25264221     1      +    C
## 66    chr20 25264664 25264664     1      +    C
## 67    chr20 25264814 25264814     1      +    C
## 68    chr20 25265554 25265554     1      +    G
## 69    chr20 25265984 25265984     1      +    T
## 70    chr20 25266450 25266450     1      +    C
## 71    chr20 25266476 25266476     1      +    T
## 72    chr20 25266513 25266513     1      +    G
## 73    chr20 25267301 25267301     1      +    A
## 74    chr20 25267893 25267893     1      +    G
## 75    chr20 25268617 25268617     1      +    G
## 76    chr20 25268920 25268920     1      +    C
## 77    chr20 25269630 25269630     1      +    G
## 78    chr20 25269743 25269743     1      +    T
## 79    chr20 25270339 25270339     1      +    C
## 80    chr20 25271086 25271086     1      +    C
## 81    chr20 25271286 25271286     1      +    G
## 82    chr20 25271326 25271326     1      +    T
## 83    chr20 25272490 25272490     1      +    C
## 84    chr20 25272633 25272633     1      +    C
## 85    chr20 25272846 25272846     1      +    T
## 86    chr20 25272878 25272878     1      +    T
## 87    chr20 25273362 25273362     1      +    A
## 88    chr20 25273435 25273435     1      +    T
## 89    chr20 25273929 25273929     1      +    G
## 90    chr20 25274318 25274318     1      +    T
## 91    chr20 25274951 25274951     1      +    A
## 92    chr20 25274953 25274953     1      +    G
## 93    chr20 25275843 25275843     1      +    T
## 94    chr20 25275870 25275870     1      +    G
## 95    chr20 25275890 25275890     1      +    A
## 96    chr20 25276297 25276297     1      +    A
## 97    chr20 25276343 25276343     1      +    A
## 98    chr20 25277244 25277244     1      +    G
## 99    chr20 25278600 25278600     1      +    A
{% endhighlight %}






