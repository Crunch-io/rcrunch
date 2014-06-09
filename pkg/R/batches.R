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