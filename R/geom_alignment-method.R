## For transcripts support three mode?
setGeneric("geom_alignment", function(data, ...) standardGeneric("geom_alignment"))


## alignment should be convenient toggle with chevron...
setMethod("geom_alignment", "GRanges", function(data,...,
                                                xlab, ylab, main,
                                                facets = NULL,
                                                stat = c("stepping", "identity"),
                                                range.geom = c("rect", "arrowrect"),
                                                gap.geom = c("chevron", "arrow", "segment"),
                                                rect.height = NULL,
                                                group.selfish = TRUE,
                                                label = TRUE){


    stat <- match.arg(stat)
    args <- list(...)
    args$facets <- facets
    args.aes <- parseArgsForAes(args)
    args.non <- parseArgsForNonAes(args)
    args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
    facet <- .buildFacetsFromArgs(data, args.facets)

    if("extend.size" %in% names(args.non))
        es <- args.non$extend.size
    else
        es <- 0

    if(is.null(rect.height))
        rect.height <- 0.4
    args.non$rect.height <- rect.height
    range.geom <- match.arg(range.geom)
    gap.geom <- match.arg(gap.geom)

    main.fun <- switch(range.geom,
                       rect = {
                           geom_rect
                       },
                       "arrowrect" = {
                           geom_arrowrect
                       })

    gap.fun <- switch(gap.geom,
                      chevron = {
                          geom_chevron
                      },
                      arrow = {
                          geom_arrow
                      },

                      segment = {
                          geom_segment
                      }
                      )

    if(length(data)){
        if(stat == "stepping"){
            args.aes <- args.aes[!(names(args.aes) %in% c("xmin", "xmax", "ymin", "ymax", "data"))]
            args.non <- args.non[!(names(args.non) %in% c("xmin", "xmin", "ymin", "ymax", "data"))]
            grl <- splitByFacets(data, facets)
            res <- endoapply(grl,
                             function(dt){
                                 if("group" %in% names(args.aes)){
                                     dt <- addStepping(dt, group.name = as.character(args.aes$group),
                                                       group.selfish = group.selfish,
                                                       extend.size = es)
                                 }else{
                                     dt <- addStepping(dt,extend.size = es)
                                 }
                             })

            res <- unlist(res)
            df <- mold(res)

            if("group" %in% names(args.aes))
                gpn <- as.character(args.aes$group)
            else
                gpn <- "stepping"

            args.aes <- args.aes[names(args.aes) != "group"]
            ## plot gaps

            gps <- getGaps(res, group.name = gpn, facets)
            if(length(gps)){
                gps <- keepSeqlevels(gps, names(seqlengths(res)))
                args.gaps <- args.aes[!names(args.aes) %in% c("x", "y",
                                                              "xend", "yend",
                                                              "label.type",
                                                              "label.size",
                                                              "label.color",
                                                              "size",
                                                              "fill",
                                                              "color",
                                                              "colour")]

                args.gaps.extra <- args.non[names(args.non) %in%
                                            c("offset", "chevron.height",
                                              "inherit.aes")]
                args.gaps$y <- as.name("stepping")
                aes.lst <- do.call("aes", args.gaps)
                gps.lst <- c(list(aes.lst), list(data = gps, stat = "identity"),
                             args.gaps.extra)

                p <- list(do.ggcall(gap.fun, gps.lst))
            }else{
                p <- NULL
            }
            ## plot main
            args.aes$y <- as.name("stepping")
            args.aes <- args.aes[names(args.aes) != "size"]
            args.non$stat = "identity"
            aes <- do.call(ggplot2::aes, args.aes)
            args.res <- c(list(data = res), list(aes),
                          args.non)

            p <- c(p, list(do.call(main.fun,args.res)))
            p <- .changeStrandColor(p, args.aes)
            .df.lvs <- unique(df$stepping)
            .df.sub <- df[, c("stepping", gpn)]
            .df.sub <- .df.sub[!duplicated(.df.sub$stepping),]
            if(gpn != "stepping" & group.selfish)
                p <- c(p , list(scale_y_continuous(breaks = .df.sub$stepping,
                                                   labels = as.character(.df.sub[, gpn]))))
            else
                p <- c(p, list(scale_y_continuous(breaks = NULL)))
        }

        if(stat == "identity"){
            stop("stat identity is nor supported for geom alignment")
        }}else{
            p <- NULL
        }
    p <- c(list(p) , list(ggplot2::ylab("")), list(facet))
    if(missing(xlab))
        xlab <- ""
    p <- c(p, list(ggplot2::xlab(xlab)))
    if(!missing(ylab))
        p <- c(p, list(ggplot2::ylab(ylab)))
    if(!missing(main))
        p <- c(p, list(labs(title = main)))

    p
})




setMethod("geom_alignment", "TxDbOREnsDb",
          function(data, ..., which,
                   columns = c("tx_id", "tx_name", "gene_id"),
                   names.expr = "tx_name",
                   facets = NULL, truncate.gaps = FALSE,
                   truncate.fun = NULL, ratio = 0.0025){

    args <- list(...)
    ## args$facets <- facets
    args.aes <- parseArgsForAes(args)
    args.non <- parseArgsForNonAes(args)
    aes.args <- do.call(aes, args.aes)

    if(is(data, "EnsDb")){
        columns <- sub(columns, pattern="tx_name",
                       replacement="gene_name", fixed=TRUE)
    }

    if(missing(which)){
        ## stop("missing which is not supported yet")
        p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
        return(p)
    }


    gr <- crunch(data, which, truncate.gaps = truncate.gaps,
                 truncate.fun = truncate.fun, ratio = ratio,
                 columns = columns)

    grl <- split(gr, gr$tx_id)

    ## getting label
    df <- mold(gr)
    .df.sub <- do.call(rbind, lapply(grl, function(g){
        ## FIXME: check if the id is unique
        mold(g)[1, columns]
    }))
    rownames(.df.sub) <- NULL

    if(is.expression(names.expr)){
        .labels <- eval(names.expr, .df.sub)
    }else if(is.character(names.expr)){

        if(length(names.expr) == nrow(.df.sub) &&
           !(names.expr %in% colnames(.df.sub))){
            .labels <- names.expr
        }else{
            .labels <- sub_names(.df.sub, names.expr)
        }
    }else{
        .labels <- sub_names(.df.sub, names.expr)
    }

    names(grl) <- .labels

    p <- do.call(geom_alignment, c(list(data = grl),
                                   args.non,
                                 list(aes.args)))

    ## if(is_coord_truncate_gaps(gr)){
    ##     gr <- gr[values(gr)$type %in% c("utr", "cds")]
    ##     ss <- getXScale(gr)
    ##     p <- c(p, list(scale_x_continuous(breaks = ss$breaks,
    ##                                       labels = ss$labels)))
    ## }else{
    ##     if(is.null(.xlim)){
    ##         if(length(which) && is(which, "GRagnes")){
    ##             .xlim <- c(start(range(which, ignore.strand = TRUE)),
    ##                        end(range(which, ignore.strand = TRUE)))
    ##             p <- c(p, list(scale_by_xlim(.xlim)))
    ##         }
    ##     }else{
    ##         p <- c(p, list(scale_by_xlim(.xlim)))
    ##     }
    ##     if(missing(xlim)){
    ##         xlim <- .xlim
    ##     }
    ##     p <- c(p, list(coord_cartesian(xlim = xlim)))
    ## }
    ## p <- setStat(p)
    p
})

## setMethod("geom_alignment", "TxDb",
##           function(data, ..., which,
##                    columns = c("tx_id", "tx_name", "gene_id"),
##                    names.expr = "tx_name",
##                    facets = NULL, truncate.gaps = FALSE,
##                    truncate.fun = NULL, ratio = 0.0025){

##     args <- list(...)
##     ## args$facets <- facets
##     args.aes <- parseArgsForAes(args)
##     args.non <- parseArgsForNonAes(args)
##     aes.args <- do.call(aes, args.aes)

##     if(missing(which)){
##         ## stop("missing which is not supported yet")
##         p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
##                list(ggplot2::xlim(c(0, 1))))
##         return(p)
##     }


##     gr <- crunch(data, which, truncate.gaps = truncate.gaps,
##                  truncate.fun = truncate.fun, ratio = ratio,
##                  columns = columns)

##     grl <- split(gr, gr$tx_id)

##     ## getting label
##     df <- mold(gr)
##     .df.sub <- do.call(rbind, lapply(grl, function(g){
##         ## FIXME: check if the id is unique
##         mold(g)[1, columns]
##     }))
##     rownames(.df.sub) <- NULL

##     if(is.expression(names.expr)){
##         .labels <- eval(names.expr, .df.sub)
##     }else if(is.character(names.expr)){

##         if(length(names.expr) == nrow(.df.sub) &&
##            !(names.expr %in% colnames(.df.sub))){
##             .labels <- names.expr
##         }else{
##             .labels <- sub_names(.df.sub, names.expr)
##         }
##     }else{
##         .labels <- sub_names(.df.sub, names.expr)
##     }

##     names(grl) <- .labels

##     p <- do.call(geom_alignment, c(list(data = grl),
##                                    args.non,
##                                  list(aes.args)))

##     ## if(is_coord_truncate_gaps(gr)){
##     ##     gr <- gr[values(gr)$type %in% c("utr", "cds")]
##     ##     ss <- getXScale(gr)
##     ##     p <- c(p, list(scale_x_continuous(breaks = ss$breaks,
##     ##                                       labels = ss$labels)))
##     ## }else{
##     ##     if(is.null(.xlim)){
##     ##         if(length(which) && is(which, "GRagnes")){
##     ##             .xlim <- c(start(range(which, ignore.strand = TRUE)),
##     ##                        end(range(which, ignore.strand = TRUE)))
##     ##             p <- c(p, list(scale_by_xlim(.xlim)))
##     ##         }
##     ##     }else{
##     ##         p <- c(p, list(scale_by_xlim(.xlim)))
##     ##     }
##     ##     if(missing(xlim)){
##     ##         xlim <- .xlim
##     ##     }
##     ##     p <- c(p, list(coord_cartesian(xlim = xlim)))
##     ## }
##     ## p <- setStat(p)
##     p
## })


setMethod("geom_alignment", "OrganismDb",
          function(data, ..., which,
                   columns = c("TXNAME", "SYMBOL", "TXID", "GENEID"),
                   names.expr = "SYMBOL",
                   facets = NULL,
                   truncate.gaps = FALSE,
                   truncate.fun = NULL, ratio = 0.0025
                   ){


    .cols <- c("TXNAME", "SYMBOL", "TXID", "GENEID")
    columns <- unique(c(.cols, columns))
    args <- list(...)
    args.aes <- parseArgsForAes(args)
    args.non <- parseArgsForNonAes(args)
    aes.args <- do.call(aes, args.aes)

    if(missing(which)){
        p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
        return(p)
    }else{
        which <- range(which, ignore.strand = TRUE)
    }

    txdb <- OrganismDbi:::.getTxDb(data)

    gr <- crunch(txdb, which, truncate.gaps = truncate.gaps,
                     truncate.fun = truncate.fun, ratio = ratio,
                 columns = c("tx_id", "tx_name", "gene_id"))


    grl <- split(gr, gr$tx_id)

    ks <- names(grl)

    lbs <- select(data, ks, columns, "TXID")


    ## getting label
    df <- mold(gr)
    values(gr) <- cbind(values(gr), lbs[match(gr$tx_id, lbs$TXID), ])
    grl <- split(gr, gr$tx_id)

    .df.sub <- do.call(rbind, lapply(grl, function(g){
        ## FIXME: check if the id is unique
        mold(g)[1, c("tx_id", "tx_name", "gene_id")]
    }))
    rownames(.df.sub) <- NULL
    .df.new <- lbs[match(.df.sub$tx_id, lbs$TXID), ]
    .df.sub <- cbind(.df.sub, .df.new)

    if(is.expression(names.expr)){
        .labels <- eval(names.expr, .df.sub)
    }else if(is.character(names.expr)){

        if(length(names.expr) == nrow(.df.sub) &&
           !(names.expr %in% colnames(.df.sub))){
            .labels <- names.expr
        }else{
            .labels <- sub_names(.df.sub, names.expr)
        }
    }else{
        .labels <- sub_names(.df.sub, names.expr)
    }

    names(grl) <- .labels
    p <- do.call(geom_alignment, c(list(data = grl,
                                        names.expr = names.expr),
                                   args.non,
                                 list(aes.args)))

})


.transformTextToSpace <- function(x = character(), limits = NULL,
                                  fixed = 80, size = 1 ){
    if(!length(limits))
        stop("pleast provide your limits of current viewed coordinate")
    nchar(x) * size / fixed * diff(limits)
}


## For bam file show individual aligments

setMethod("geom_alignment", "BamFile", function(data,..., which,
                                                what = c("rname", "strand", "pos", "qwidth", "seq"),
                                                xlab, ylab, main,
                                                facets = NULL){
    bi <- biovizBase:::scanBamGRanges(fl, which = which, what = what)
    bt <- biovizBase::addStepping(bt)
    names(bt) <- NULL

    d <- as.data.frame(bt)

    qplot(x = start, y = stepping, label = df, geom = "text", data = d)

})

setMethod("geom_alignment", "GRangesList", function(data, ..., which = NULL,
                                                    cds.rect.h = 0.25,
                                                    exon.rect.h = cds.rect.h,
                                                    utr.rect.h = cds.rect.h/2,
                                                    xlab, ylab, main,
                                                    facets = NULL,
                                                    geom = "alignment",
                                                    stat = c("identity", "reduce"),
                                                    range.geom = "rect",
                                                    gap.geom = "arrow",
                                                    utr.geom = "rect",
                                                    names.expr = NULL,
                                                    label = TRUE,
                                                    label.color = "gray40",
                                                    arrow.rate = 0.015,
                                                    length = unit(0.1, "cm")){



    stat <- match.arg(stat)
    gap.fun <- getGeomFun(gap.geom)
    range.fun <- getGeomFun(range.geom)
    utr.fun <- getGeomFun(utr.geom)


    args <- list(...)
    ## args$facets <- facets
    args.aes <- parseArgsForAes(args)
    args.non <- parseArgsForNonAes(args)
    ## args.facets <- subsetArgsByFormals(args, facet_grid, facet_wrap)
    ## facet <- .buildFacetsFromArgs(data, args.facets)

    if("type" %in% names(args.aes)){
        .type <- as.character(args.aes$type)
    }else{
        .type <- NULL
    }
    if(!isGenemodel(data, type = .type)){
        gr <- flatGrl(data)
        if(!"group" %in% names(args.aes))
            args.aes$group <- as.name("grl_name")
        if("type" %in% names(args.aes)){
            args.aes <- args.aes[names(args.aes) != "type"]
        }
        aes.res <- do.call(aes, args.aes)

        p <- do.call(geom_alignment, c(list(data = gr),
                                   args.non,
                                 list(aes.res)))
        return(p)

    }

    message("Constructing graphics...")
    .xlim <- NULL
    if(length(which) && is(which, "GRanges")){
        which <- range(which, ignore.strand = TRUE)
        data <- subsetByOverlaps(data, which)
    }

    mids <- unlist(lapply(data, function(g){
        r <- range(g, ignore.strand = TRUE)
        start(r) + width(r)/2
    }))

    gr <- stack(data, "..sample..")
    values(gr)$..inner.. <- rep(1:length(data), times = elementNROWS(data))


    if("type" %in% names(args.aes)){
        .type <- as.character(args.aes$type)
        args.aes <- args.aes[names(args.aes) != "type"]
    }else{
        .type <- "type"
    }

    if(length(gr)){

        if(length(which) && is(which, "GRanges")){
            .xlim <- c(min(start(which)), max(end(which)))
        }else{
            .xlim <- c(min(start(gr)), max(end(gr)))
        }
        ## adding buffer between features to avoid overlap
        if("extend.size" %in% names(args.non)){
            es <- args.non$extend.size
        }else{
            es <- diff(.xlim)/1000 * 20
        }
        if(geom == "alignment" && stat == "reduce"){
            .gr.ori <- gr
            .gr <- addStepping(gr, group.name = "..inner..", group.selfish = FALSE, extend.size = es)
            message("reduce alignemnts...")
            ## compute labels
            gr <- stack(endoapply(split(gr, values(gr)[[.type]]), function(g){
                res <- reduce(g, ignore.strand = TRUE)
            }), .type)
            gr$stepping <- .lvs <- 1
        }else{
            gr <- addStepping(gr, group.name = "..inner..", group.selfish = FALSE, extend.size = es)
        }


        df <- mold(gr)


        if(label){
            if(stat == "reduce"){
                ## only transcripts with symbol or geneid
                cnms <- colnames(values(.gr.ori))
                idx <- cnms %in% c("SYMBOL", "GENEID", "gene_id")
                if(sum(idx)){
                    .df.sub <- do.call(rbind,
                                       lapply(split(.gr.ori,
                                                    values(.gr.ori)[,which(idx)[1]]),
                           function(g){
                               if (length(g) == 0L)
                                   return(NULL)
                               r <- range(g, ignore.strand = TRUE)
                               mid <- start(r) + width(r)/2
                               d <- values(g)[1, idx, drop = FALSE]
                               d$midpoint <- mid
                               d$stepping <- 1
                               d
                           }))
                    .df.sub <- as.data.frame(.df.sub)
                    if(length(names.expr)){
                        .labels <- ggbio:::sub_names(.df.sub, names.expr)
                        .df.sub$.labels <- .labels
                    }else{
                        label <- FALSE
                    }
                }else{
                    label <- FALSE
                }

            }else{
             column <- c("tx_id", "stepping", "..sample..", "..inner..")
             idx <- column %in% colnames(df)
             if(sum(idx)){
                 column <- column[idx]
             names.expr <- "..sample.."
            .df.sub <- df[, column]
            .df.sub <- .df.sub[!duplicated(.df.sub$..inner..),]
            .df.sub$midpoint <- mids[.df.sub$..inner..]
             .labels <- ggbio:::sub_names(.df.sub, names.expr)
             .lvs <- max(.df.sub$stepping)
            .df.lvs <- unique(df$stepping)
            .df.sub$.labels <- .labels
         }else{
             label <- FALSE
         }

         }
        }
        ## cds
        gr.cds <- gr[values(gr)[[.type]] %in% c("cds", "CDS")]
        ## exons
        gr.exons <- gr[values(gr)[[.type]] %in% c("exon", "EXON")]
        args.cds.non <- args.non
        args.cds.non$rect.height <- cds.rect.h
        args.exon.non <- args.cds.non
        args.exon.non$rect.height <- exon.rect.h
        args.exon <- args.aes[names(args.aes) != "y"]
        args.exon$y <- as.name("stepping")
        aes.res <- do.call(aes, args.exon)
        p <- NULL
        if (length(gr.cds) > 0L) {
            ## plot cds
            args.cds.res <- c(list(data = gr.cds),
                              list(aes.res),
                              args.cds.non,
                              list(stat = "identity"))
            p <- do.ggcall(range.fun, args.cds.res)
        }
        if (length(gr.exons) > 0L) {
            ## plot exons
            ## input gr.exons
            args.exon.res <- c(list(data = gr.exons),
                               list(aes.res),
                               args.exon.non,
                               list(stat = "identity"))
            p <- c(p, list(do.ggcall(range.fun, args.exon.res)))
        }
        ## utrs
        gr.utr <- gr[values(gr)[[.type]] == "utr"]
        args.utr <- args.aes[names(args.aes) != "y"]
        args.utr$y <- as.name("stepping")
        aes.res <- do.call(aes, args.utr)
        args.utr.non <- args.cds.non
        args.utr.non$rect.height <- utr.rect.h

        if(range.geom == "arrowrect" && utr.geom == range.geom){
            if(!"arrow.head" %in% names(args.utr.non)){
                args.utr.non$arrow.head <- 0.06
            }
            arrow.head.fix <- getArrowLen(gr.cds, arrow.head.rate = args.non$arrow.head)
            args.utr.non <- args.non[names(args.non) != "arrow.rate"]
            args.utr.non$arrow.head.fix <- arrow.head.fix
        }

        if(length(gr.utr)){
            args.utr.res <- c(list(data = gr.utr),
                              list(aes.res),
                              args.utr.non,
                              list(stat = "identity"))
            p <- c(p, list(do.ggcall(utr.fun, args.utr.res)))
        }
        if(stat == "reduce"){
            .grl <- endoapply(data, function(g){
                range(g, ignore.strand = TRUE)
            })
            grr <- reduce(unlist(.grl))
            .gr$..sample.. <- rep(subjectHits(findOverlaps(.grl, grr)),
                                  times = elementNROWS(data))
            exonic <- .gr[values(.gr)[[.type]] %in% c("utr", "cds", "exon")]
            df.gaps <- getGaps(exonic, group.name = "..sample..")
            df.gaps$stepping <- 1
            ## let's figure out strand
            stds <- unique(as.character(strand(.gr)))
            if(length(stds) == 1){
                strand(df.gaps) <- stds
            }else{
                strand(df.gaps) <- "*"
            }
            ## FIXME: fix reduced strand
          } else {
            exonic <- gr[values(gr)[[.type]] %in% c("utr", "cds", "exon")]
            df.gaps <- getGaps(exonic, group.name = "..inner..")
          }

        args.aes.gaps <- args.aes[!(names(args.aes) %in% c("x", "y", "fill"))]
        aes.res <- do.call(aes, args.aes.gaps)

        if(!"arrow.rate" %in%  names(args.non)){
            if(!is.list(which)){
                arrow.rate <- 0.018 * diff(.xlim)/
                    (end(range(ranges(df.gaps))) - start(range(ranges(df.gaps))))
                args.non$arrow.rate <- arrow.rate
            }
        }

        aes.res$y <- as.name("stepping")
        args.non.arrow <- args.non
        args.non.arrow$length <- length
        args.gaps.res <- c(list(data = df.gaps),
                           list(aes.res),
                           args.non.arrow,
                           list(stat = "identity"))
        if(length(df.gaps)){
            p <- c(p , list(do.ggcall(gap.fun, args.gaps.res)))
        }
        if(label){
            aes.label <- do.call(aes, list(label = substitute(.labels),
                                           x = substitute(midpoint),
                                           y = substitute(stepping +
                                               cds.rect.h*1.2)))
            if(!"size" %in% names(args.non))
                args.non$size <- 2.5

            args.label.res <- c(list(data = .df.sub),
                                list(vjust = 0),
                                list(aes.label),
                                list(color = label.color),
                                args.non)

            p <- c(p , list(do.ggcall(geom_text, args.label.res)))
            ggplot() + p
        }
        p <- c(p , list(scale_y_continuous(breaks = NULL)))

    }else{
        p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
               list(ggplot2::xlim(c(0, 1))))
    }

    if(missing(xlab)){
        xlab <- ""
    }
    xlab <- ggplot2::xlab(xlab)

    p <- c(p, list(xlab(xlab)))

    if(missing(ylab)){
        ylab <- ""
    }

    p <- c(p, list(ylab(ylab)))

    if(!missing(main))
        p <- c(p, labs(title = main))

##    p <- setStat(p)
    return(p)
})

setGeneric("isGenemodel", function(data, ...) standardGeneirc("isGenemodel"))
setMethod("isGenemodel", "GRanges", function(data, type = NULL){

    if(is.null(type))
        type <- "type"
    if(type %in% colnames(values(data))){
        geneFeatureTerms <- c("cds", "exon", "utr", "gap")
        idx <- tolower(values(data)[[type]]) %in% geneFeatureTerms
        if (any(!idx)) {
          message(paste0('\"', unique(values(data)[[type]][!idx]), '\"',
                         collapse=", "),
                  " not in any of the valid gene feature terms ",
                  paste0('\"', geneFeatureTerms, '\"', collapse=", "))
        }
        return(any(idx))
    }else{
        return(FALSE)
    }

})

setMethod("isGenemodel", "GRangesList", function(data, type = NULL){
    data <- stack(data)
    isGenemodel(data, type = type)
})

setMethod("isGenemodel", "ANY", function(data, type = NULL){
    return(FALSE)
})


## ####============================================================
## ##  geom_alignment method from ggbio, R/geom_alignment-method.R
## ##
## ####------------------------------------------------------------
## setMethod("geom_alignment", "EnsDb",
##           function(data, ..., which,
##                    columns = c("tx_id", "gene_name", "gene_id"),
##                    names.expr = "tx_id",
##                    facets = NULL, truncate.gaps = FALSE,
##                    truncate.fun = NULL, ratio = 0.0025){

##               args <- list(...)
##               ## args$facets <- facets
##               args.aes <- parseArgsForAes(args)
##               args.non <- parseArgsForNonAes(args)
##               aes.args <- do.call(aes, args.aes)

##               if(missing(which)){
##                   ## stop("missing which is not supported yet")
##                   p <- c(list(geom_blank()),list(ggplot2::ylim(c(0, 1))),
##                          list(ggplot2::xlim(c(0, 1))))
##                   return(p)
##               }

##               ## The crunch method for EnsDb ensures that tx_id is provided.
##               gr <- crunch(data, which, truncate.gaps = truncate.gaps,
##                            truncate.fun = truncate.fun, ratio = ratio,
##                            columns = columns)

##               grl <- split(gr, gr$tx_id)

##               ## getting label
##               df <- mold(gr)
##               .df.sub <- do.call(rbind, lapply(grl, function(g){
##                   ## FIXME: check if the id is unique
##                   mold(g)[1, columns]
##               }))
##               rownames(.df.sub) <- NULL

##               if(is.expression(names.expr)){
##                   .labels <- eval(names.expr, .df.sub)
##               }else if(is.character(names.expr)){

##                   if(length(names.expr) == nrow(.df.sub) &&
##                      !(names.expr %in% colnames(.df.sub))){
##                       .labels <- names.expr
##                   }else{
##                       .labels <- sub_names(.df.sub, names.expr)
##                   }
##               }else{
##                   .labels <- sub_names(.df.sub, names.expr)
##               }

##               names(grl) <- .labels

##               p <- do.call(geom_alignment, c(list(data = grl),
##                                              args.non,
##                                              list(aes.args)))
##               p
##           })

