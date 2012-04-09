## @knitr load
library(ggbio)
library(GenomicRanges)
set.seed(1)
N <- 200

## @knitr simul
## ======================================================================
##  simmulated GRanges
## ======================================================================
ir <-  IRanges(start = sample(1:300, size = N, replace = TRUE),
               width = sample(70:75, size = N,replace = TRUE))
## add meta data 
df <- DataFrame(value = rnorm(N, 10, 3), score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"), 
                size = N, replace = TRUE),
              pair = sample(letters, size = N, 
                replace = TRUE))
values(ir) <- df

## @knitr exp
autoplot(ir)
autoplot(ir, aes(fill = pair))
autoplot(ir, stat = "coverage", geom = "line")
autoplot(ir, stat = "coverage", geom = "point")
autoplot(ir, stat = "coverage", geom = "line", facets = sample ~. )
autoplot(ir, stat = "boxplot", aes(y = score, x = sample))
