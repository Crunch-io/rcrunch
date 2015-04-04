log <- function (...) {
    logfile <- getOption("crunch.log")
    if (!is.null(logfile)) {
        cat(..., "\n", file=logfile, append=TRUE)
    }
}

