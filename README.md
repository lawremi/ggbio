# INSTALL
### For R 2.15 with git version *ggbio*(latest)
#### This is for developers, notice I am not using bioc-devel svn as major version control system. If you want to play with latest *ggbio* you have to stick with this method.

Install of developmental version of *ggbio* require developmental R now. 
Simply open *R* session and run the following code:

    source("http://www.tengfei.name/projects/ggbio/utils/installer.R")
    
This will install ggplot2/ggbio github version, and bioc-dev version 
denpendencies.

For all users, make sure to install R package *RCurl* to use this command

For Windows users: if you came across buiding errors, please 
install [Rtools](http://cran.r-project.org/bin/windows/Rtools/), then run the
command line again.

### For R 2.15 with bioc-dev version *ggbio*

    source("http://bioconductor.org/biocLite.R")
    biocLite("ggbio")

This will automatically install the right bioc-dev version of *ggbio*

__This is currently now working due to the new-yet-unreleased ggplot2 0.9__

### For R 2.14

    source("http://bioconductor.org/biocLite.R")
    biocLite("ggbio")
    
__not supported yet for git hub version ggbio__



