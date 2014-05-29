pollBatchStatus <- function (batch.url, catalog, until=c("imported", "ready"),
                            frequency=2, timeout=60) {
    
    until <- match.arg(until)
    
    for (i in seq_len(ceiling(timeout / frequency))) {
        status <- catalog[[batch.url]]$status
        if (status %in% c("conflict")) { ## is there another failure status?
            stop("Error on import", call.=FALSE)
        } else if (status %in% until) {
            invisible(status)
        }
        
        Sys.sleep(frequency)
        catalog <- refresh(catalog)
    }
    
    stop("Timed out. Check back later.", call.=FALSE)
}