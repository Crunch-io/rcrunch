pollBatchStatus <- function (batch.url, catalog, until="imported",
                            wait=1, timeout=default.timeout()) {
    
    starttime <- Sys.time()
    timer <- function (since, units="secs") {
        difftime(Sys.time(), since, units=units)
    }
    status <- catalog[[batch.url]]$status
    print(status)
    while (status %in% c("idle", "importing") && timer(starttime) < timeout) {
        Sys.sleep(wait)
        catalog <- refresh(catalog)
        status <- catalog[[batch.url]]$status
        print(status)
    }
    
    if (status %in% "idle") {
        stop("Append process failed to start on the server", call.=FALSE)
    } else if (status %in% "importing") {
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
    ## x is list, keys are variable URLs, objects are arrays of objects with keys "message" and "resolution" (in R-speak, s/array/list/, s/object/list/, s/keys/names/)
    if (length(x)) {
        x <- groupConflicts(x)
        return(vapply(x, formatConflictMessage, character(1), USE.NAMES=FALSE))
    } else {
        return("No conflicts.")
    }
}

groupConflicts <- function (x) {
    ## reshape conflicts to be by conflict-resolution, not by variable
    flat <- flattenConflicts(x)
    ## split by message, then by resolution
    return(unlist(lapply(split(flat, flat$message), 
        function (x) split(x, x$resolution)), recursive=FALSE))
}

flattenConflicts <- function (x) {
    ## flatten object to data.frame with url, message, resolution
    out <- mapply(function (i, d) {
        df <- do.call(rbind, lapply(d$conflicts, as.data.frame, stringsAsFactors=FALSE))
        df$url <- i
        df$name <- d$metadata$name
        return(df)
    }, i=names(x), d=x, SIMPLIFY=FALSE)
    return(do.call(rbind, out))
}

formatConflictMessage <- function (x) {
    ## receives a data.frame with variable URLs and common conflict and resolution
    conflict <- paste("Conflict:", unique(x$message))
    resolution <- paste("Resolution:", unique(x$resolution))
    ## those should be length 1 by construction
    
    varnames <- x$name
    if (is.null(varnames)) varnames <- x$url
    vars <- paste0(nrow(x), " variable", ifelse(nrow(x) > 1, "s", ""), ": ", 
        serialPaste(dQuote(unique(varnames))))
    return(paste(conflict, resolution, vars, sep="; "))
}

