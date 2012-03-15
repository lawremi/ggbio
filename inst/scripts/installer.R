thisVer <- getRversion()
if(!compareVersion(as.character(thisVer), "2.15.0") >=0)
  stop("R dev(2.15) is required")
cran.nms <- c("grid", "gridExtra", "scales", "Hmisc", "reshape2", "plyr", "digest", "memoise", "proto", "MASS")
bioc.nms <- c("BiocGenerics", "Biobase", "IRanges", "GenomicRanges", "GenomicFeatures", "Rsamtools", "BSgenome", "biovizBase")
##  cran
install.packages(cran.nms)
source("http://bioconductor.org/biocLite.R")
biocLite(bioc.nms)
## use devtools to install other dependencies from GitHub
if (!require('devtools'))
  install.packages('devtools')  # needs Curl for RCurl
library(devtools)
pkgs <- list(hadley = c("ggplot2"), 
             tengfei = c("ggbio", "biovizBase"))
for (repo in names(pkgs)) {
  for (pkg in pkgs[[repo]]) install_github(pkg, repo)
}


