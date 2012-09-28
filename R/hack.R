###  hack at automaticaly generating method for IRanges and Granges
### to avoid get global .method, use a closure.
setOldClass("uneval")
.geoms <- paste0("geom_", .ggbio.geom)
.stats <- paste0("stat_", .ggbio.stat)
.layouts <-  c("layout_circle", "layout_karyogram")

.gr.name <- c(.geoms, .stats, .layouts)


for(method in .gr.name){
  ## for IRanges
  ifun <- function(method){
    .method <- method
    if(hasMethod(.method, "GRanges") && !hasMethod(.method, "IRanges")){
      setMethod(.method, "IRanges", function(data, ...){
        .fun <- selectMethod(.method, sig = "GRanges")
        df <- values(data)
        values(data) <- NULL
        gr <- GRanges("chr_non", data)
        values(gr) <- df
        .fun(gr, ...)
      })     
    }
  }
  ifun(method)

  ## for GRangesList

  gfun <- function(method){
    .method <- method
    if(hasMethod(.method, "GRanges") && !hasMethod(.method, "GRangesList")){
      setMethod(.method, "GRangesList", function(data, ...){
        .fun <- selectMethod(.method, sig = "GRanges")      
        gr <- biovizBase:::flatGrl(data)
        .fun(gr, ...)
      })
    }
  }
  gfun(method)

  ## hacking for ggplot2-like API without using proto
  mfun <- function(method){
    .method <- method
      setMethod(.method, "missing", function(data, ...){
        return(match.call())        
      })
  }
  mfun(method)

  ufun <- function(method){
    .method <- method
    setMethod(.method, "uneval", function(data, ...){
      lst <- as.list(match.call())
      idx <- names(lst) != "data"
      aes.u <- unname(lst[!idx])
      res <- lst[idx]
      res <- c(res, aes.u)
      return(as.call(res))
    })
  }
  ufun(method)
  
}
