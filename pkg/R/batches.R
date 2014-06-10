pollBatchStatus <- function (batch.url, catalog, until="imported",
                            wait=1, timeout=default.timeout()) {
    
    starttime <- Sys.time()
    timer <- function (since, units="secs") {
        difftime(Sys.time(), since, units=units)
    }
    status <- catalog[[batch.url]]$status
    while (status == "importing" && timer(starttime) < timeout) {
        Sys.sleep(wait)
        catalog <- refresh(catalog)
        status <- catalog[[batch.url]]$status
    }
    
    if (status %in% "importing") {
        stop("Timed out. Check back later.", call.=FALSE)
    } else if (status %in% c(until, "conflict")) {
        return(status)
    } else {
        stop(status, call.=FALSE)
    }
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

