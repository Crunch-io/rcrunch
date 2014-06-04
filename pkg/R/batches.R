pollBatchStatus <- function (batch.url, catalog, until="imported",
                            wait=1, timeout=default.timeout()) {
    
    starttime <- Sys.time()
    while (difftime(Sys.time(), starttime, units="secs") < timeout) {
        status <- catalog[[batch.url]]$status
        if (status %in% c("failed")) {
            stop("Error on import", call.=FALSE)
        } else if (status %in% until) {
            return(status)
        }
        Sys.sleep(wait)
        catalog <- refresh(catalog)
    }
    
    stop("Timed out. Check back later.", call.=FALSE)
}

default.timeout <- function () {
    opt <- getOption("crunch.timeout")
    if (is.null(opt) || !is.numeric(opt)) opt <- 60
    return(opt)
}

formatConflicts <- function (x) {
    if (length(x)) {
        return(mapply(function (i, m) paste0(i, ": ", formatConflictMessage(m)),
            i=names(x), m=x, USE.NAMES=FALSE))
    } else {
        return("No conflicts.")
    }
}

formatConflictMessage <- function (x) {
    x <- sapply(x, function (a) paste0("Conflict: ", a$message, "; Resolution: ",
        a$resolution))
    if (length(x) > 1) {
        x <- paste0("(", seq_along(x), ") ", x, collapse="\n")
    }
    return(x)
}

