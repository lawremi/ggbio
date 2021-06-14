stepping <- new_generic("stepping", signature = "data")

method(stepping, "GRanges") <- function(data, group, extend.size = 0,
                                        fix = "center", group.selfish = TRUE,
                                        ...) {
    args <- list(data, group.selfish = group.selfish, fix = fix,
                 extend.size = extend.size)
    if (!missing(group))
        args <- c(args, group = group)
    data <- do.call(biovizBase::addStepping, args)
    data
}
