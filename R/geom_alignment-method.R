setGeneric("geom_alignment", function(data, ...) standardGeneric("geom_alignment"))

setMethod("geom_alignment", "data.frame", function(data, data.ori, args,rect.height = 0.4){
  args.aes <- parseArgsForAes(args)
  args.non <- parseArgsForNonAes(args)
  rect.height <- force(rect.height)
  args.aes <- args.aes[!(names(args.aes) %in% c("x", "y", "data"))]
  args.non <- args.non[!(names(args.non) %in% c("x", "y", "data"))]
  if("group" %in% names(args.aes))
    gpn <- as.character(args.aes$group)
  else
    gpn <- ".levels"
  args.aes <- args.aes[names(args.aes) != "group"]
  gps <- suppressWarnings(getGap(data.ori, group.name = gpn))
  gps <- keepSeqlevels(gps, names(seqlengths(data.ori)))
  args.gaps <- args.aes[!names(args.aes) %in% c("x", "y",
                                        "xend", "yend",
                                        "label.type",
                                        "label.size",
                                        "label.color",
                                        "size",
                                        "fill",
                                        "color",
                                        "colour")]

  args.gaps.extra <- args[names(args) %in%
                          c("offset", "chevron.height")]
  aes.lst <- do.call("aes", args.gaps)
  gps.lst <- c(list(aes.lst), list(data = gps),
               args.gaps.extra)
  p <- list(do.call(geom_chevron, gps.lst))
  
  args.aes <- c(args.aes, list(xmin = substitute(start),
                       xmax = substitute(end),
                       ymin = substitute(.levels - rect.height),
                       ymax = substitute(.levels + rect.height)))
  args.aes <- args.aes[names(args.aes) != "size"]
  aes <- do.call(aes, args.aes)
  args.res <- c(list(data = data), list(aes),
                   args.non)
  p <- c(p, list(do.call(geom_rect,args.res)))
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
})


setMethod("geom_alignment", "GRanges", function(data, ...){
  stat_stepping(data, ..., geom = "alignment")
})
