.changeStrandColor <- function(p, args, fill = TRUE){
  strandColor <- getOption("biovizBase")$strandColor
  isStrand.color <- FALSE
  isStrand.fill <- FALSE
  ## default with no color
  idx <- c("color", "colour") %in% names(args)
  if((any(idx))){
    nms <- c("color", "colour")[idx][1]
    if(as.character(args[[nms]]) == "strand")
      isStrand.color <- TRUE
  }
  if(("fill" %in% names(args))){
    if(as.character(args$fill) == "strand")
      isStrand.fill <- TRUE
  }
  if(isStrand.color)
    p <- c(list(p), list(scale_color_manual(values = strandColor)))
  if(fill){
    if(isStrand.fill)
      p <- c(p, list(scale_fill_manual(values = strandColor)))
  }
  p
}

.geom_rect <- function(data, args, rect.height = 0.4){
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  rect.height <- force(rect.height)
  args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "data"))]
  args.non <- args.non[!(names(args.non) %in% c("x", "y", "data"))]
  if("group" %in% names(args.aes))
    gpn <- as.character(args$group)
  else
    gpn <- ".levels"
  args.aes <- args.aes[names(args.aes) != "group"]
  args.aes <- c(args.aes, list(xmin = substitute(start),
                       xmax = substitute(end),
                       ymin = substitute(.levels - rect.height),
                       ymax = substitute(.levels + rect.height)))
  args.aes <- args.aes[names(args.aes) != "size"]
  aes <- do.call(aes, args.aes)
  args.res <- c(list(data = data), list(aes),
                   args.non)
  p <- list(do.call(geom_rect,args.res))
  p <- .changeStrandColor(p, args.aes)
  .df.lvs <- unique(data$.levels)
  .df.sub <- data[, c(".levels", gpn)]
  .df.sub <- .df.sub[!duplicated(.df.sub),]
  if(gpn != ".levels")
    p <- c(p , list(scale_y_continuous(breaks = .df.sub$.levels,
                                labels = as.character(.df.sub[, gpn]))))
  else
    p <- c(p, list(scale_y_continuous(breaks = NA)))
   p
}


.geom_segment <- function(data, args, stat = c("stepping", "identity")){
  stat <- match.arg(stat)
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  if(stat == "stepping"){  
    args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("x", "y", "data"))]
    if("group" %in% names(args.aes))
      gpn <- as.character(args$group)
    else
      gpn <- ".levels"
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- c(args.aes, list(x = substitute(start),
                                 xend = substitute(end),
                                 y = substitute(.levels),
                                 yend = substitute(.levels)))
    args.aes <- args.aes[names(args.aes) != "size"]
    aes <- do.call(aes, args.aes)
    args.res <- c(list(data = data), list(aes),
                  args.non)
    p <- list(do.call(geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
    .df.lvs <- unique(data$.levels)
    .df.sub <- data[, c(".levels", gpn)]
    .df.sub <- .df.sub[!duplicated(.df.sub),]
    if(gpn != ".levels")
      p <- c(p , list(scale_y_continuous(breaks = .df.sub$.levels,
                                         labels = as.character(.df.sub[, gpn]))))
    else
      p <- c(p, list(scale_y_continuous(breaks = NA)))
  }
  if(stat == "identity"){
    args.aes <- args.aes[!(names(args.aes) %in% c("x", "data"))]
    args.non <- args.non[!(names(args.non) %in% c("x", "data"))]
    if("group" %in% names(args.aes))
      gpn <- as.character(args$group)
    else
      gpn <- ".levels"
    args.aes <- args.aes[names(args.aes) != "group"]
    args.aes <- c(args.aes, list(x = substitute(start),
                                 xend = substitute(end)))
    args.aes$yend <- args$y                               
    args.aes <- args.aes[names(args.aes) != "size"]
    aes <- do.call(aes, args.aes)
    args.res <- c(list(data = data), list(aes),
                  args.non)
    p <- list(do.call(geom_segment,args.res))
    p <- .changeStrandColor(p, args.aes)
  }
  p
}


