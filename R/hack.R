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
  ## is data is missing, return a call and parse the data
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


## add a new geom text
btextGrob <- function (label,x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                       just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
                       default.units = "npc", name = NULL, gp = gpar(), vp = NULL,  f=1.5, 
                       fc = "white", alp = 1) {
  if (!is.unit(x)) 
    x <- unit(x, default.units)
  if (!is.unit(y)) 
    y <- unit(y, default.units)
  grob(label = label, x = x, y = y, just = just, hjust = hjust, 
       vjust = vjust, rot = rot, check.overlap = check.overlap, 
       name = name, gp = gp, vp = vp, cl = "text")
  tg <- textGrob(label = label, x = x, y = y, just = just, hjust = hjust, 
                 vjust = vjust, rot = rot, check.overlap = check.overlap)
  w <- unit(rep(1, length(label)), "strwidth", as.list(label))
  h <- unit(rep(1, length(label)), "strheight", as.list(label))
  rg <- rectGrob(x=x, y=y, width=f*w, height=f*h,
                 gp=gpar(fill=fc, alpha=alp,  col=NA))
  
  gTree(children=gList(rg, tg), vp=vp, gp=gp, name=name)
}

GeomText2 <- proto(ggplot2:::GeomText, {
  objname <- "text2"
  
  draw <- function(., data, scales, coordinates, ..., fc = "white", alp = 1, 
                   parse = FALSE, na.rm = FALSE) {
    data <- remove_missing(data, na.rm, 
                           c("x", "y", "label"), name = "geom_text2")
    
    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }
    
    with(coord_transform(coordinates, data, scales),
         btextGrob(lab, x, y, default.units="native", 
                   hjust=hjust, vjust=vjust, rot=angle, 
                   gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt,
                             fontfamily = family, fontface = fontface, lineheight = lineheight),
                   fc = fc, alp = alp)
    )
  }
  
})

geom_text2 <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                        parse = FALSE,  ...) { 
  GeomText2$new(mapping = mapping, data = data, stat = stat,position = position, 
                parse = parse, ...)
}
