---
layout: portfolio
title: How to process your data and manupulate a GRanges object
category: portfolio
---




### Introduction
Processing and manipulation of our raw data into objects that *ggbio* expected
is important, not only for visualization purpose but also for analysis in a
pipeline. It's worth writing a simple tutorial for most useful cases. So in this
section, we are going to go over a case study about manipulation of your raw data.

## Step 1: Import 
Most time the data you get could be in various file format, including csv, txt,
maf, xlsx etc. So the first step is to import your raw data into format that
*ggbio* expected, which is *GRanges* in most cases. 

Let's use an example data sets in package *biovizBase* as our raw data, this
data set contains a subset of somatic mutation, and it is stored in "csv"
format, and in *R* , `read.csv` help you import a "csv" file as `data.frame`
object. The data used in this study is from this
[paper](http://www.nature.com.proxy.lib.iastate.edu:2048/ng/journal/v43/n10/full/ng.936.html)
, and original data is
[here](http://www.broadinstitute.org/~lawrence/crc/CRC9.genomic.v3.maf).


{% highlight r %}
crc1 <- system.file("extdata", "crc1-missense.csv", package = "biovizBase")
crc1 <- read.csv(crc1)
head(crc1)
{% endhighlight %}



{% highlight text %}
##   Hugo_Symbol Entrez_Gene_Id Center NCBI_Build Chromosome Start_position
## 1      TARDBP          23435  Broad         36       chr1       11003085
## 2       INADL          10207  Broad         36       chr1       62352395
## 3         CFH           3075  Broad         36       chr1      194960885
## 4        CYS1         192668  Broad         36       chr2       10116508
## 5     RASGRP3          25780  Broad         36       chr2       33617747
## 6     C2orf78         388960  Broad         36       chr2       73894280
##   End_position Strand Variant_Classification Variant_Type Reference_Allele
## 1     11003085      +               Missense          SNP                G
## 2     62352395      +               Missense          SNP                T
## 3    194960885      +               Missense          SNP                G
## 4     10116508      -               Missense          SNP                C
## 5     33617747      +               Missense          SNP                C
## 6     73894280      +               Missense          SNP                T
##   Tumor_Seq_Allele1 Tumor_Seq_Allele2
## 1                 G                 A
## 2                 T                 G
## 3                 G                 A
## 4                 C                 T
## 5                 C                 T
## 6                 T                 C
{% endhighlight %}




Please also check `read.table`, `scan` in *R* base, and `read.xlsx` in package
*xlsx*, to make sure you are using the most proper tools for importing your raw data.

## Step 2: Coercion to `GRanges` object.
Next we coerce a `data.frame` to `GRanges` object. The column names of your data
could be different from what `GRanges` required:

 - `seqnames` for chromosome names
 - `strand` for strand direction. including "+", "-", "*"
 - `start` for start position of an interval
 - `end` for end position of an interval
 - `width` for width of an interval
 
The first method is to specify those by hand  and use function `GRanges` to
construct an instance and add extra information as` elementMetadata` or `values`
to it.


{% highlight r %}
library(GenomicRanges)
mut.gr <- with(crc1, GRanges(Chromosome, IRanges(Start_position, 
    End_position), strand = Strand))
values(mut.gr) <- subset(crc1, select = -c(Start_position, End_position, 
    Chromosome))
{% endhighlight %}




The second way is to use a function called `transformDfToGr` to transform a
`data.frame` to a `GRanges`, you specify the alternative name in your original
data to tell the function how to map it to reserved names in `GRanges`, then
extra columns will be automatically stored as element meta data. Notice if names
in your original data are expected names you don't need to specify them and all
happens as default.


{% highlight r %}
library(biovizBase)
mut.gr <- transformDfToGr(crc1, seqnames = "Chromosome", start = "Start_position", 
    end = "End_position", strand = "Strand")
{% endhighlight %}





## Step 3: Getting seqlegnths for genome
`seqlengths` tell the lengths of each chromosomes, if you didn't specify it in
the `GRanges` function, you will get just `NA` for all of them.  This fine for
most analysis, but for visualization it's **important** to add this information to
your data, this tells *ggbio* how to create graphics on an accurate genome
space. Other wise what you see may not reflect a real genome space.


{% highlight r %}
seqlengths(mut.gr)
{% endhighlight %}



{% highlight text %}
##  chr1 chr10 chr11 chr12 chr13 chr15 chr16 chr17 chr18 chr19  chr2 chr20 
##    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA 
## chr21 chr22  chr3  chr4  chr5  chr6  chr7  chr9  chrX 
##    NA    NA    NA    NA    NA    NA    NA    NA    NA 
{% endhighlight %}




You can parse your genome information from any source and stored the legnths in
a vector in *R*, and use names(required) of the vector to indicate chromosomes
names. Inside *R*, we have some ways to help you get this information fairly easy.

The first method is to used utilities in package *rtracklayer*. To check
available supported genome, please run function `ucscGenomes`, and use
`GRangesForUCSCGenome` for getting genome information, this will return a
`GRanges` object, and you can just run `seqlengths` to parse what you want.


{% highlight r %}
## supported genomes, use db column
library(rtracklayer)
head(ucscGenomes())
{% endhighlight %}



{% highlight text %}
##        db species      date                               name
## 1    hg19   Human Feb. 2009 Genome Reference Consortium GRCh37
## 2    hg18   Human Mar. 2006                    NCBI Build 36.1
## 3    hg17   Human  May 2004                      NCBI Build 35
## 4    hg16   Human Jul. 2003                      NCBI Build 34
## 5 felCat4     Cat Dec. 2008                   NHGRI catChrV17e
## 6 felCat3     Cat Mar. 2006          Broad Institute Release 3
{% endhighlight %}



{% highlight r %}
seqs.gr <- GRangesForUCSCGenome("hg19")
seqs <- seqlengths(seqs.gr)
{% endhighlight %}




Another way is to use a wrapper for those utilities in package *biovizBase*, the
function is called `getIdeogram`, you just need to specify the argument
`cytoband` to `FALSE`, otherwise you get cytoband information with no
seqlengths. 


{% highlight r %}
seqs.gr <- getIdeogram("hg19", cytoband = FALSE)
seqs <- seqlengths(seqs.gr)
{% endhighlight %}




simple way for human hg19 genome, we have a built-in data you can use




## Step 4: Subset your data based on chromosomes you need
For visualization purpose, it's **important** that you just keep the chromosomes
you need to a reasonable number, otherwise it's going to be less informative if
you try to plot too many chromosomes at one time. Pay attention to that **ggbio
use seqlevels to tell how many chromosome to plot, not based on data**.

For instance, we just want to visualize chromosome 1-22, and let's name it
following the routine "chr11".


{% highlight r %}
chr.sub <- paste("chr", 1:22, sep = "")
chr.sub
{% endhighlight %}



{% highlight text %}
##  [1] "chr1"  "chr2"  "chr3"  "chr4"  "chr5"  "chr6"  "chr7"  "chr8" 
##  [9] "chr9"  "chr10" "chr11" "chr12" "chr13" "chr14" "chr15" "chr16"
## [17] "chr17" "chr18" "chr19" "chr20" "chr21" "chr22"
{% endhighlight %}




`keepSeqlevels` in package *GenomicRanges* is a very useful tool to subset
seqlevels or reordering your chromosomes. The following example shows how we
keep and subset what we need. *ggbio* always use chromosome levels orders to
plot.


{% highlight r %}
head(seqlengths(hg19Ideogram), 22)
{% endhighlight %}



{% highlight text %}
##                 chr1 chr1_gl000191_random chr1_gl000192_random 
##            249250621               106433               547496 
##                 chr2                 chr3                 chr4 
##            243199373            198022430            191154276 
##       chr4_ctg9_hap1 chr4_gl000193_random chr4_gl000194_random 
##               590426               189789               191469 
##                 chr5                 chr6        chr6_apd_hap1 
##            180915260            171115067              4622290 
##        chr6_cox_hap2        chr6_dbb_hap3       chr6_mann_hap4 
##              4795371              4610396              4683263 
##        chr6_mcf_hap5        chr6_qbl_hap6       chr6_ssto_hap7 
##              4833398              4611984              4928567 
##                 chr7 chr7_gl000195_random                 chr8 
##            159138663               182896            146364022 
## chr8_gl000196_random 
##                38914 
{% endhighlight %}



{% highlight r %}
head(seqlengths(keepSeqlevels(hg19Ideogram, chr.sub)), 22)
{% endhighlight %}



{% highlight text %}
##      chr1      chr2      chr3      chr4      chr5      chr6      chr7 
## 249250621 243199373 198022430 191154276 180915260 171115067 159138663 
##      chr8      chr9     chr10     chr11     chr12     chr13     chr14 
## 146364022 141213431 135534747 135006516 133851895 115169878 107349540 
##     chr15     chr16     chr17     chr18     chr19     chr20     chr21 
## 102531392  90354753  81195210  78077248  59128983  63025520  48129895 
##     chr22 
##  51304566 
{% endhighlight %}




In this case study, `mut.gr` happen to miss some chromosomes in the data, like
"chr8", before we assign the `seqlengths` to it, we need to make sure all
chromosomes we need present in the `seqlevels`


{% highlight r %}
seqlevels(mut.gr) <- c(chr.sub, "chrX")
mut.gr <- keepSeqlevels(mut.gr, chr.sub)
seqs.sub <- seqs[chr.sub]
seqs.sub
{% endhighlight %}



{% highlight text %}
##      chr1      chr2      chr3      chr4      chr5      chr6      chr7 
## 249250621 243199373 198022430 191154276 180915260 171115067 159138663 
##      chr8      chr9     chr10     chr11     chr12     chr13     chr14 
## 146364022 141213431 135534747 135006516 133851895 115169878 107349540 
##     chr15     chr16     chr17     chr18     chr19     chr20     chr21 
## 102531392  90354753  81195210  78077248  59128983  63025520  48129895 
##     chr22 
##  51304566 
{% endhighlight %}




We also need to check if you data contains some value that are not in the genome
space or exceed the limits of certain chromosomes.


{% highlight r %}
bidx <- end(mut.gr) <= seqs.sub[match(as.character(seqnames(mut.gr)), 
    names(seqs.sub))]
mut.gr <- mut.gr[which(bidx)]
{% endhighlight %}




Ok, now it's time to set `seqlengths` to it


{% highlight r %}
seqlengths(mut.gr) <- seqs.sub
mut.gr
{% endhighlight %}



{% highlight text %}
## GRanges with 60 ranges and 9 elementMetadata cols:
##        seqnames                 ranges strand   | Hugo_Symbol
##           <Rle>              <IRanges>  <Rle>   |    <factor>
##    [1]     chr1 [ 11003085,  11003085]      +   |      TARDBP
##    [2]     chr1 [ 62352395,  62352395]      +   |       INADL
##    [3]     chr1 [194960885, 194960885]      +   |         CFH
##    [4]     chr2 [ 10116508,  10116508]      -   |        CYS1
##    [5]     chr2 [ 33617747,  33617747]      +   |     RASGRP3
##    [6]     chr2 [ 73894280,  73894280]      +   |     C2orf78
##    [7]     chr2 [ 96732769,  96732769]      +   |      FER1L5
##    [8]     chr2 [179160267, 179160267]      -   |         TTN
##    [9]     chr2 [217251189, 217251189]      -   |      IGFBP5
##    ...      ...                    ...    ... ...         ...
##   [52]    chr19   [57407795, 57407795]      +   |     PPP2R1A
##   [53]    chr20   [23298287, 23298287]      +   |        GZF1
##   [54]    chr20   [31012946, 31012946]      +   |      EFCAB8
##   [55]    chr20   [40223536, 40223536]      -   |       PTPRT
##   [56]    chr20   [54467136, 54467136]      +   |       CASS4
##   [57]    chr20   [60201983, 60201983]      +   |      GTPBP5
##   [58]    chr21   [36688774, 36688774]      +   |      CHAF1B
##   [59]    chr21   [39699770, 39699770]      -   |       LCA5L
##   [60]    chr22   [27437953, 27437953]      -   |       CHEK2
##        Entrez_Gene_Id   Center NCBI_Build Variant_Classification
##             <integer> <factor>  <integer>               <factor>
##    [1]          23435    Broad         36               Missense
##    [2]          10207    Broad         36               Missense
##    [3]           3075    Broad         36               Missense
##    [4]         192668    Broad         36               Missense
##    [5]          25780    Broad         36               Missense
##    [6]         388960    Broad         36               Missense
##    [7]          90342    Broad         36               Missense
##    [8]           7273    Broad         36               Missense
##    [9]           3488    Broad         36               Missense
##    ...            ...      ...        ...                    ...
##   [52]           5518    Broad         36               Missense
##   [53]          64412    Broad         36               Missense
##   [54]         388795    Broad         36               Missense
##   [55]          11122    Broad         36               Missense
##   [56]          57091    Broad         36               Missense
##   [57]          26164    Broad         36               Missense
##   [58]           8208    Broad         36               Missense
##   [59]         150082    Broad         36               Missense
##   [60]          11200    Broad         36               Missense
##        Variant_Type Reference_Allele Tumor_Seq_Allele1 Tumor_Seq_Allele2
##            <factor>         <factor>          <factor>          <factor>
##    [1]          SNP                G                 G                 A
##    [2]          SNP                T                 T                 G
##    [3]          SNP                G                 G                 A
##    [4]          SNP                C                 C                 T
##    [5]          SNP                C                 C                 T
##    [6]          SNP                T                 T                 C
##    [7]          SNP                T                 T                 G
##    [8]          SNP                C                 C                 T
##    [9]          SNP                C                 C                 T
##    ...          ...              ...               ...               ...
##   [52]          SNP                G                 G                 A
##   [53]          SNP                A                 A                 C
##   [54]          SNP                C                 C                 T
##   [55]          SNP                C                 C                 T
##   [56]          SNP                G                 G                 A
##   [57]          SNP                C                 C                 T
##   [58]          SNP                C                 C                 T
##   [59]          SNP                T                 T                 G
##   [60]          SNP                C                 C                 A
##   ---
##   seqlengths:
##         chr1      chr2      chr3      chr4 ...     chr20     chr21     chr22
##    249250621 243199373 198022430 191154276 ...  63025520  48129895  51304566
{% endhighlight %}




basically the data `mut.gr` is now ready to be plotted, if you want to rename
your chromosomes, you can use `renameSeqlevels` in package *GenomicRanges* to
rename it, just need to create a vector, whose value is new names and whose
names are old matched chromosomes names to be replace.


{% highlight r %}
new.names <- as.character(1:22)
names(new.names) <- paste("chr", new.names, sep = "")
new.names
{% endhighlight %}



{% highlight text %}
##  chr1  chr2  chr3  chr4  chr5  chr6  chr7  chr8  chr9 chr10 chr11 chr12 
##   "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"  "10"  "11"  "12" 
## chr13 chr14 chr15 chr16 chr17 chr18 chr19 chr20 chr21 chr22 
##  "13"  "14"  "15"  "16"  "17"  "18"  "19"  "20"  "21"  "22" 
{% endhighlight %}



{% highlight r %}
mut.gr.new <- renameSeqlevels(mut.gr, new.names)
mut.gr.new
{% endhighlight %}



{% highlight text %}
## GRanges with 60 ranges and 9 elementMetadata cols:
##        seqnames                 ranges strand   | Hugo_Symbol
##           <Rle>              <IRanges>  <Rle>   |    <factor>
##    [1]        1 [ 11003085,  11003085]      +   |      TARDBP
##    [2]        1 [ 62352395,  62352395]      +   |       INADL
##    [3]        1 [194960885, 194960885]      +   |         CFH
##    [4]        2 [ 10116508,  10116508]      -   |        CYS1
##    [5]        2 [ 33617747,  33617747]      +   |     RASGRP3
##    [6]        2 [ 73894280,  73894280]      +   |     C2orf78
##    [7]        2 [ 96732769,  96732769]      +   |      FER1L5
##    [8]        2 [179160267, 179160267]      -   |         TTN
##    [9]        2 [217251189, 217251189]      -   |      IGFBP5
##    ...      ...                    ...    ... ...         ...
##   [52]       19   [57407795, 57407795]      +   |     PPP2R1A
##   [53]       20   [23298287, 23298287]      +   |        GZF1
##   [54]       20   [31012946, 31012946]      +   |      EFCAB8
##   [55]       20   [40223536, 40223536]      -   |       PTPRT
##   [56]       20   [54467136, 54467136]      +   |       CASS4
##   [57]       20   [60201983, 60201983]      +   |      GTPBP5
##   [58]       21   [36688774, 36688774]      +   |      CHAF1B
##   [59]       21   [39699770, 39699770]      -   |       LCA5L
##   [60]       22   [27437953, 27437953]      -   |       CHEK2
##        Entrez_Gene_Id   Center NCBI_Build Variant_Classification
##             <integer> <factor>  <integer>               <factor>
##    [1]          23435    Broad         36               Missense
##    [2]          10207    Broad         36               Missense
##    [3]           3075    Broad         36               Missense
##    [4]         192668    Broad         36               Missense
##    [5]          25780    Broad         36               Missense
##    [6]         388960    Broad         36               Missense
##    [7]          90342    Broad         36               Missense
##    [8]           7273    Broad         36               Missense
##    [9]           3488    Broad         36               Missense
##    ...            ...      ...        ...                    ...
##   [52]           5518    Broad         36               Missense
##   [53]          64412    Broad         36               Missense
##   [54]         388795    Broad         36               Missense
##   [55]          11122    Broad         36               Missense
##   [56]          57091    Broad         36               Missense
##   [57]          26164    Broad         36               Missense
##   [58]           8208    Broad         36               Missense
##   [59]         150082    Broad         36               Missense
##   [60]          11200    Broad         36               Missense
##        Variant_Type Reference_Allele Tumor_Seq_Allele1 Tumor_Seq_Allele2
##            <factor>         <factor>          <factor>          <factor>
##    [1]          SNP                G                 G                 A
##    [2]          SNP                T                 T                 G
##    [3]          SNP                G                 G                 A
##    [4]          SNP                C                 C                 T
##    [5]          SNP                C                 C                 T
##    [6]          SNP                T                 T                 C
##    [7]          SNP                T                 T                 G
##    [8]          SNP                C                 C                 T
##    [9]          SNP                C                 C                 T
##    ...          ...              ...               ...               ...
##   [52]          SNP                G                 G                 A
##   [53]          SNP                A                 A                 C
##   [54]          SNP                C                 C                 T
##   [55]          SNP                C                 C                 T
##   [56]          SNP                G                 G                 A
##   [57]          SNP                C                 C                 T
##   [58]          SNP                C                 C                 T
##   [59]          SNP                T                 T                 G
##   [60]          SNP                C                 C                 A
##   ---
##   seqlengths:
##            1         2         3         4 ...        20        21        22
##    249250621 243199373 198022430 191154276 ...  63025520  48129895  51304566
{% endhighlight %}




sessionInfo









