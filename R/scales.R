trans_seq <- function(unit = c("Mb", "kb", "bp")) {
    unit <- match.arg(unit)
    function(x) {
        res <- switch(unit, Mb = {x/1e6},
                            kb = {x/1000},
                            bp = {x})
        res
    }
}

trans_seq_rev <- function(unit = c("Mb", "kb", "bp")) {
    unit <- match.arg(unit)
    function(x) {
        res <- switch(unit, Mb = {x*1e6},
                            kb = {x*1000},
                            bp = {x})
        res
    }
}

trans_seq_format <- function(unit = c("Mb", "kb", "bp")) {
    unit <- match.arg(unit)
    function(x) {
        res <- switch(unit, Mb = {x/1e6},
                            kb = {x/1000},
                            bp = {x})
        paste(res, unit)
    }
}

.append_unit <- function(unit = "") {
    function(x) paste(x, unit)
}

scale_x_sequnit <- function(unit = c("Mb", "kb", "bp"), append = NULL) {
    unit <- match.arg(unit)
    if(is.null(append)) {
        scale_x_continuous(breaks = trans_breaks(trans_seq(unit),
                           trans_seq_rev(unit)),
                           labels = trans_format(trans_seq_format(unit),
                           math_format(.x)))
    } else {
        stopifnot(is.character(append))
        scale_x_continuous(labels = trans_format(.append_unit(append),
                           math_format(.x)))
    }
}

scale_fill_giemsa <- function(fill = getOption("biovizBase")$cytobandColor) {
    list(scale_fill_manual(values = fill))
}

## matrix
scale_fill_fold_change <- function() {
    scale_fill_gradient2(low = "blue", mid = "white", high = "red")
}

scale_by_xlim <- function(xlim, by.unit = TRUE) {
    if(by.unit)
        .d <- max(xlim)
    else
        .d <- diff(xlim)

    if(.d > 1e6)
        res <- scale_x_sequnit("Mb")
    else if (.d <= 1e6 & .d > 1e3)
        res <- scale_x_sequnit("kb")
    else
        res <- scale_x_sequnit("bp")

    res
}
