---
layout: post
title: How to process your data and manupulate a GRanges object
category: blog
---
<!--begin.rcode setup, message = FALSE, echo = FALSE, warning = FALSE
    render_jekyll()
    opts_knit$set(base.url='http://tengfei.github.com/ggbio/_posts/')
    dir.path <- "/home/tengfei/Codes/svnrepos/devel/ggbio/inst/examples/_posts"
    fl<- file.path(dir.path, "processing.R")
    read_chunk(fl)
end.rcode-->

- [Step 0: Introduction](#s0)
- [Step 1: Import](#s1)
- [Step 2: Coercion to `GRanges` object](#s2)
- [Step 3: Getting seqlegnths for genome](#s3)
- [Step 4: Subset your data based on chromosomes you need](#s4)

## Step 0: Introduction <a id = "s0"></a>
Processing and manipulation of our raw data into objects that *ggbio* expected
is important, not only for visualization purpose but also for analysis in a
pipeline. It's worth writing a simple tutorial for most useful cases. So in this
section, we are going to go over a case study about manipulation of your raw data.

## Step 1: Import <a id = "s1"></a>
Most time the data you get could be in various file format, including csv, txt,
maf, xlsx etc. So the first step is to import your raw data into format that
*ggbio* expected, which is *GRanges* in most cases. 

Let's use an example data sets in package *biovizBase* as our raw data, this
data set contains a subset of somatic mutation, and it is stored in "csv"
format, and in *R* , `read.csv` help you import a "csv" file as `data.frame`
object. The data used in this study is from this
[paper](http://www.nature.com/ng/journal/v43/n10/full/ng.936.html)
, and original data is
[here](http://www.broadinstitute.org/~lawrence/crc/CRC9.genomic.v3.maf).
<!--begin.rcode load_mut, message = FALSE, warning = FALSE
end.rcode-->

Please also check `read.table`, `scan` in *R* base, and `read.xlsx` in package
*xlsx*, to make sure you are using the most proper tools for importing your raw data.

## Step 2: Coercion to `GRanges` object.<a id = "s2"></a>
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
<!--begin.rcode mut.gr  , message = FALSE, warning = FALSE
end.rcode-->

The second way is to use a function called `transformDfToGr` to transform a
`data.frame` to a `GRanges`, you specify the alternative name in your original
data to tell the function how to map it to reserved names in `GRanges`, then
extra columns will be automatically stored as element meta data. Notice if names
in your original data are expected names you don't need to specify them and all
happens as default.
<!--begin.rcode transformDfToGr  , message = FALSE, warning = FALSE
end.rcode-->


## Step 3: Getting seqlegnths for genome <a id = "s3"></a>
`seqlengths` tell the lengths of each chromosomes, if you didn't specify it in
the `GRanges` function, you will get just `NA` for all of them.  This fine for
most analysis, but for visualization it's **important** to add this information to
your data, this tells *ggbio* how to create graphics on an accurate genome
space. Other wise what you see may not reflect a real genome space.
<!--begin.rcode seqlengths  , message = FALSE, warning = FALSE
end.rcode-->

You can parse your genome information from any source and stored the legnths in
a vector in *R*, and use names(required) of the vector to indicate chromosomes
names. Inside *R*, we have some ways to help you get this information fairly easy.

The first method is to used utilities in package *rtracklayer*. To check
available supported genome, please run function `ucscGenomes`, and use
`GRangesForUCSCGenome` for getting genome information, this will return a
`GRanges` object, and you can just run `seqlengths` to parse what you want.
<!--begin.rcode get_seqlengths_ucsc  , message = FALSE,  warning = FALSE
end.rcode-->

Another way is to use a wrapper for those utilities in package *biovizBase*, the
function is called `getIdeogram`, you just need to specify the argument
`cytoband` to `FALSE`, otherwise you get cytoband information with no
seqlengths. 
<!--begin.rcode get_seqlengths_ideo  , message = FALSE,  warning = FALSE
end.rcode-->

simple way for human hg19 genome, we have a built-in data you can use
<!--begin.rcode get_seqlengths_data, message = FALSE,  warning = FALSE
end.rcode-->


## Step 4: Subset your data based on chromosomes you need <a id = "s4"></a>
For visualization purpose, it's **important** that you just keep the chromosomes
you need to a reasonable number, otherwise it's going to be less informative if
you try to plot too many chromosomes at one time. Pay attention to that **ggbio
use seqlevels to tell how many chromosome to plot, not based on data**.

For instance, we just want to visualize chromosome 1-22, and let's name it
following the routine "chr11".
<!--begin.rcode subset_chr  , message = FALSE,  warning = FALSE
end.rcode-->

`keepSeqlevels` in package *GenomicRanges* is a very useful tool to subset
seqlevels or reordering your chromosomes. The following example shows how we
keep and subset what we need. *ggbio* always use chromosome levels orders to
plot.
<!--begin.rcode keep_seqlevels  , message = FALSE,  warning = FALSE
end.rcode-->

In this case study, `mut.gr` happen to miss some chromosomes in the data, like
"chr8", before we assign the `seqlengths` to it, we need to make sure all
chromosomes we need present in the `seqlevels`
<!--begin.rcode levels_mut, message = FALSE,  warning = FALSE
end.rcode-->

We also need to check if you data contains some value that are not in the genome
space or exceed the limits of certain chromosomes.
<!--begin.rcode remove_wrong  , message = FALSE,  warning = FALSE
end.rcode-->

Ok, now it's time to set `seqlengths` to it
<!--begin.rcode assign_seqlengths  , message = FALSE,  warning = FALSE
end.rcode-->

basically the data `mut.gr` is now ready to be plotted, if you want to rename
your chromosomes, you can use `renameSeqlevels` in package *GenomicRanges* to
rename it, just need to create a vector, whose value is new names and whose
names are old matched chromosomes names to be replace.
<!--begin.rcode rename_seqlevels  , message = FALSE,  warning = FALSE
end.rcode-->

sessionInfo
<!--begin.rcode sessionInfo, message = FALSE,  warning = FALSE 
end.rcode-->







