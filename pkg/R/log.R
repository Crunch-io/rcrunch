log <- function (...) {
    logfile <- getOption("crunch.log")
    if (!is.null(logfile)) {
        cat(..., "\n", file=logfile, append=TRUE)
    }
}

startLog <- function (filename, append=FALSE) {
    options(crunch.log=filename)
    if (!append && file.exists(filename)) {
        file.remove(filename)
    }
}