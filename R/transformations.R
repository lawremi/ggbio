stepping <- new_generic("stepping", signature = "data")

method(stepping, "GRanges") <- function(data, group, extend.size = 0,
                                        fix = "center", group.selfish = TRUE,
                                        ...) {
    if(requireNamespace("biovizBase", quietly = TRUE)) {
        args <- list(data, group.selfish = group.selfish, fix = fix,
                     extend.size = extend.size)
        if (!missing(group))
            args <- c(args, group = group)
        data <- do.call(biovizBase::addStepping, args)
    } else {
        stop("Package biovizBase required to perfom stepping transform, please install it.",
             .Call = FALSE)
    }
    data
}
