pollBatchStatus <- function (batch.url, catalog, until="imported",
                            frequency=2, timeout=60) {
    
    until <- match.arg(until)
    for (i in seq_len(ceiling(timeout / frequency))) {
        status <- catalog[[batch.url]]$status
        if (status %in% c("failed")) {
            stop("Error on import", call.=FALSE)
        } else if (status %in% until) {
            invisible(status)
        }
        Sys.sleep(frequency)
        catalog <- refresh(catalog)
    }
    
    stop("Timed out. Check back later.", call.=FALSE)
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

