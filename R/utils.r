stop.with.message <- function(...) {
    stop(do.call(paste, list(...)))
}

warning.with.message <- function(...) {
    warning(do.call(paste, list(...)))
}
