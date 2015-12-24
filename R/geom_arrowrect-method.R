setGeneric("geom_arrowrect", function(data, ...) standardGeneric("geom_arrowrect"))

setMethod("geom_arrowrect", "GRanges", function(data, ...,
                                                  xlab, ylab, main,
                                                facets = NULL,
                                                  stat = c("stepping", "identity"),
                                                  rect.height = NULL,
                                                  arrow.head = 0.06,
                                                  arrow.head.rate = arrow.head,
                                                 arrow.head.fix = NULL,
                                                  group.selfish = TRUE){

  stat <- match.arg(stat)
  ## shape <- match.arg(shape)
  args <- list(...)
  args$facets <- facets

  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)

  args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
  facet <- .buildFacetsFromArgs(data, args.facets)
  if(length(data)){  
    if(stat == "stepping"){
    if(is.null(rect.height))
      rect.height <- 0.4
      
      grl <- splitByFacets(data, facets)
      res <- endoapply(grl,
                       function(dt){
                         if("group" %in% names(args.aes))
                           dt <- addStepping(dt, group.name = as.character(args.aes$group),
                                              group.selfish = group.selfish)
                         else
                           dt <- addStepping(dt)
                       })
      res <- unlist(res)
      df <- breakGrTo5polyDf(res, y = "stepping", rect.height = rect.height,
                             arrow.head = arrow.head, arrow.head.rate = arrow.head.rate, arrow.head.fix = arrow.head.fix)
      args.aes$x <- as.name(".temp.x")
      args.aes$y <- as.name(".temp.y")
      args.aes$group <- as.name(".id")
      aes.temp <- do.call(aes, args.aes)                    
      p <- do.ggcall(geom_polygon, c(list(data = df), list(aes.temp), args.non))
      }
    if(stat == "identity"){
      if(!"y" %in% names(args.aes))
        stop("aes(y = ) is requried for stat identity")
      if(is.null(rect.height)){
         rect.height <- diff(range(values(data)[,as.character(args.aes$y)]))/20
      }
      df <- breakGrTo5polyDf(data, y = as.character(args.aes$y), rect.height = rect.height,
                             arrow.head = arrow.head, arrow.head.rate = arrow.head.rate, arrow.head.fix = arrow.head.fix)
      args.aes$x <- as.name(".temp.x")
      args.aes$y <- as.name(".temp.y")
      args.aes$group <- as.name(".id")      
      aes.temp <- do.call(aes, args.aes)                    
      p <- do.ggcall(geom_polygon, c(list(data = df), list(aes.temp), args.non))
    }}else{
      p <- NULL
    }
    p <- c(list(p) , list(facet))


  if(missing(xlab)) 
    xlab <- ""
  p <- c(p, list(ggplot2::xlab(xlab)))
  
  if(missing(ylab))
    ylab <- ""
  
  p <- c(p, list(ggplot2::ylab(ylab)))

  if(!missing(main))
    p <- c(p, list(labs(title = main)))
p  
})


getArrowLen <- function(object, arrow.head.rate = 0.4){
  width(range(ranges(object))) * arrow.head.rate
}

breakGrTo5polyDf <- function(object, arrow.head = 0.02,
                             arrow.head.rate = arrow.head,
                             arrow.head.fix = NULL,
                             rect.height = 0.4, y){
  if(!length(arrow.head.fix)){
    ah <- getArrowLen(object, arrow.head.rate)
  }else{
    ah <- arrow.head.fix
  }
  df <- mold(object)
  df$.id <- seq_len(nrow(df))
  res <- do.call(rbind,lapply(1:5, function(i) df))
  res <- res[order(res$.id), ]
  lst <- lapply(1:nrow(df), function(i){
    x <- df[i,, drop = FALSE]
    std <- x$strand
    if(x$width > ah){
      if(std == "+"){
        .x <- c(x$start, x$end-ah, x$end)
        .x <- c(.x, rev(.x)[-1])
        .y <- c(rep(x[, y] - rect.height, 2), x[, y], rep(x[, y] + rect.height, 2))
      }
      if(std == "-"){
        .x <- c(x$start, x$start+ah, x$end)
        .x <- c(.x, rev(.x)[-3])
        .y <- c(x[, y], rep(x[, y] - rect.height, 2), rep(x[, y] + rect.height, 2))
      }
      if(std == "*"){
        .x <- c(x$start, x$end, x$end)
        .x <- c(.x, rev(.x)[-1])
        .y <- c(rep(x[, y] - rect.height, 2), x[, y], rep(x[, y] + rect.height, 2))
      }
  }else{
      if(std == "+"){
        .x <- c(x$start, x$start, x$end)
        .x <- c(.x, rev(.x)[-1])
        .y <- c(rep(x[, y] - rect.height, 2), x[, y], rep(x[, y] + rect.height, 2))
      }
      if(std == "-"){
        .x <- c(x$start, x$end, x$end)
        .x <- c(.x, rev(.x)[-3])
        .y <- c(x[, y], rep(x[, y] - rect.height, 2), rep(x[, y] + rect.height, 2))
      }
      if(std == "*"){
        .x <- c(x$start, x$end, x$end)
        .x <- c(.x, rev(.x)[-1])
        .y <- c(rep(x[, y] - rect.height, 2), x[, y], rep(x[, y] + rect.height, 2))
      }
  }
    data.frame(.temp.x = .x, .temp.y = .y)
  })
  temp <- do.call(rbind, lst)
  res <- cbind(res, temp)
  res
}

