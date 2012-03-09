---
layout: base
title: Support
---
- [Installation](#install)
- [FAQ](#FAQ)
- [Bug report](#bug)

## Installation <a id = "install"></a>

#### For R 2.15 with git version *ggbio*(latest)
_This is for developers, notice I am not using bioc-devel svn as major version control system. If you want to play with latest_ **ggbio** _you have to stick with this method._

Install of developmental version of *ggbio* require developmental R now. 
Simply open *R* session and run the following code:

    source("http://www.tengfei.name/projects/ggbio/utils/installer.R")
    
This will install ggplot2/ggbio github version, and bioc-dev version 
denpendencies.

For all users, make sure to install R package *RCurl* to use this command

For Windows users: if you came across buiding errors, please 
install [Rtools](http://cran.r-project.org/bin/windows/Rtools/), then run the
command line again.

#### For R 2.15 with bioc-dev version *ggbio*

    source("http://bioconductor.org/biocLite.R")
    biocLite("ggbio")

__This is currently now working due to unsycronized ggbio in bioc-dev__

## FAQ<a id = "FAQ"></a>
- Test question

> a
> b
 
- test 2

>3
>4

## Bug report<a id = "bug"></a>
For any issues please contact yintengfei at gmail.com or file a issue in
**ggbio** [github issue page](https://github.com/tengfei/ggbio/issues).

