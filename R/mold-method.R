setGeneric("mold", function(data, ...) standardGeneric("mold"))

setMethod("mold", c("eSet"), function(data){
  object <- data
  pdata <- phenoData(object)
  df <- as(object, "data.frame")
  df$sampleNames <- row.names(df)
  df.m <- melt(df, id.vars =  c(varLabels(pdata), "sampleNames"))
  df.m
})

setMethod("mold", c("GRanges"), function(data){
  names(data) <- NULL
  df <- as.data.frame(data)
  df$midpoint <- (df$start+df$end)/2
  df
})

setMethod("mold", c("GRangesList"), function(data, indName = "grl_name"){
  gr <- biovizBase:::flatGrl(data, indName)
  df <- mold(gr)
  df
})

setMethod("mold", c("IRanges"), function(data){
  df <- as.data.frame(data)
  df$midpoint <- (df$start+df$end)/2
  df
})

setMethod("mold", c("Seqinfo"), function(data){
  df <- .transformSeqinfo(data)
  df <- mold(df)  
  df <- cbind(df, as.data.frame(data))
  df
})


setMethod("mold", c("matrix"), function(data){
  x <- seq_len(ncol(data))
  y <- seq_len(nrow(data))
  cnms <- colnames(data)
  rnms <- rownames(data)
  df <- expand.grid(x = x, y = y)
  df$value <- as.vector(t(data))
  df$row <- df$y
  df$col <- df$x  
  df$colnames <- cnms[df$x]
  df$rownames <- rnms[df$y]
  df
})


setMethod("mold", c("ExpressionSet"), function(data){
  df <- mold(exprs(data))
  pd <- pData(data)
  idx <- seq_len(ncol(exprs(data)))
  res <- cbind(df, pd[df$x, ])
  res
})

setMethod("mold", c("SummarizedExperiment"), function(data, assay.id = 1){
  ays <- assays(data)
  stopifnot(length(assay.id) == 1 || length(assay.id) <= length(ays))
  if(length(ays) > 1)
    message("Assay index: ", assay.id, " used")
  cd <- as.data.frame(colData(data))
  rd <- as.data.frame(rowData(data))
  df <- mold(ays[[assay.id]])
  cd.e <- cd[df$x, ]
  rd.e <- rd[df$y, ]
  res <- cbind(df, cd.e, rd.e)
  res
})

## setMethod("mold", c("VCF"), function(data,
##                                                    type = c("geno", "info", "fixed"),
##                                                    assay.id = 1){
##   type <- match.arg(type)  
##   hdr <- exptData(object)[["header"]]
##   nms <- rownames(geno(hdr))
##   res <- switch(type,
##                 geno = {
##                   nms <- rownames(geno(hdr))
##                   if(is.numeric(assay.id)){
##                     nm <- nms[assay.id]  
##                   }else if(is.character(assay.id) && assay.id %in% nms){
##                     nm <- assay.id
##                   }else{
##                     stop("wrong assay.id specified, only numeric value which indicates
##                           index of data set under each type or name for that data set.")
##                   }
##                   res <- geno(object)[[nm]]

##                 },
##                 info = {
                  
##                 },
##                 fixed = {
                  
##                 })
  
## })


setMethod("mold", c("Views"), function(data){
  df.r <- as.data.frame(ranges(data))
  nms <- names(data)
  if(!is.null(nms))
    names(data) <- NULL
  mx <- suppressWarnings(as.matrix(data))
  df <- mold(mx)
  df$start <- rep(df.r$start, each = ncol(mx))
  df$end <- rep(df.r$end, each = ncol(mx))
  df$width <- rep(df.r$width, each = ncol(mx))
  df$midpoint <- (df$start + df$end)/2
  if(!is.null(nms))
    df$rowname <- rep(nms, each = ncol(mx))
  df$row <- factor(df$y)
  df$col <- factor(df$x)  
  df
})

## setMethod("mold", c("BamFile"), function(data){

## })

## setMethod("mold", c("GAlignments"), function(data){

## })

## setMethod("mold", c("TranscriptDb"), function(data){
  
## })

setMethod("mold", c("Rle"), function(data){
  df <- as.data.frame(data)
  df$x <- as.numeric(rownames(df))
  df$y <- 1
  df$row <- factor(df$y)
  df$col <- factor(df$x)
  df
})

setMethod("mold", c("RleList"), function(data){
  data <- xRleList
  nms <- names(data)
  if(is.null(nms))
    nms <- seq_len(length(data))
  lst <- lapply(1:length(data), function(i){
    x <- data[[i]]
    df <- mold(x)
    df$group <- nms[i]
    df
  })
  res <- do.call(rbind, lst)
  res
})

## setMethod("mold", c("BSgenome"), function(data){

## })














