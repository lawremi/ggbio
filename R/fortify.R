fortify <- new_generic("fortify", signature = "data")

method(fortify, "GRanges") <- function(data, ...) {
    # by setting names to NULL, reset's them back to 1:n
    names(data) <- NULL
    as.data.frame(data)
}
