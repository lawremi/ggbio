###  hack at automaticaly generating method for IRanges and Granges
### to avoid get global .method, use a closure.
setOldClass("uneval")
.geoms.ggbio <- paste0("geom_", .ggbio.geom)
.stats.ggbio <- paste0("stat_", .ggbio.stat)
.geoms.ggplot <- paste0("geom_", .ggplot.geom)
.stats.ggplot <- paste0("stat_", .ggplot.stat)

.layouts <-  c("layout_circle", "layout_karyogram")

.gr.name.ggbio <- c(.geoms.ggbio, .stats.ggbio, .layouts)
.gr.name.ggbio <- setdiff(.gr.name.ggbio, c(.geoms.ggplot, .stats.ggplot))
.gr.name.ggplot <- c(.geoms.ggplot, .stats.ggplot)


for(method in .gr.name.ggbio){
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
      setMethod(.method, "missing", function(data,...){
          mc <- match.call()
        return(mc)        
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



for(method in .gr.name.ggplot){
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
          method0 <- getFromNamespace(method, "ggplot2")
          tm <- try({res <- method0(...)}, silent = TRUE)
          if(inherits(tm, "try-error")){
              res <-  match.call()
          }else{
              mc <- match.call()
              attr(res, "call") <- TRUE
              attr(res, "mc") <- mc
          }
        return(res)        
      })
  }
  mfun(method)

  ufun <- function(method){
      .method <- method      
      setMethod(.method, "uneval", function(data, ...){
          method0 <- getFromNamespace(method, "ggplot2")
          tm <- try({res <- method0(data, ...)}, silent = TRUE)
          if(inherits(tm, "try-error")){
              lst <- as.list(match.call())
              idx <- names(lst) != "data"
              aes.u <- unname(lst[!idx])
              res <- lst[idx]
              res <- c(res, aes.u)
              res <- as.call(res)
          }else{
              lst <- as.list(match.call())
              idx <- names(lst) != "data"
              aes.u <- unname(lst[!idx])
              res <- lst[idx]
              res <- c(res, aes.u)
              mc <- as.call(res)
              attr(res, "call") <- TRUE
              attr(res, "mc") <- mc
          }
          return(res)
      })
  }
  ufun(method)
}
