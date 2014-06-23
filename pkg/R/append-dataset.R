appendDataset <- function (dataset1, dataset2, confirm=interactive(),
                            cleanup=TRUE) {
    
    stopifnot(is.dataset(dataset1))
    batch_url <- addBatchToDataset(dataset1, dataset2)    
    dataset <- try(acceptAppendResolutions(batch_url, dataset1, confirm=confirm),
        silent=TRUE)
    if (is.error(dataset)) {
        if (cleanup) {
            DELETE(batch_url)
        } else {
            message("Batch URL: ", batch_url) ## So you can fix and retry
        }
        rethrow(dataset)
    }
    invisible(dataset)
}

addBatchToDataset <- function (dataset1, dataset2) {
    if (!is.dataset(dataset2)) {
        ## TODO: compose batch directly, not as dataset?
        temp.ds.name <- paste("Appending to", name(dataset1), now())
        message("Creating ", sQuote(temp.ds.name), " as temporary dataset")
        dataset2 <- newDataset(dataset2, name=temp.ds.name)
    }
    
    batches_url <- dataset1@catalogs$batches
    body <- list(
        element="shoji:entity",
        body=list(
            dataset=self(dataset2),
            workflow=I(list())
        )
    )
    invisible(POST(batches_url, body=toJSON(body)))
}

acceptAppendResolutions <- function (batch_url, dataset, confirm=interactive(), ...) {
    status <- pollBatchStatus(batch_url, batches(dataset), until="ready")
    
    batch <- ShojiObject(GET(batch_url))
    resolutions <- batch@body$conflicts
    ## Report on what was done/will be done
    message(paste(formatConflicts(resolutions), collapse="\n"))
    # cat(paste(formatConflicts(resolutions), collapse="\n"))
    
    if (status == "conflict") {
        ## message(the fatal conflicts)
        err <- c("There are conflicts that cannot be resolved automatically.",
            "Please manually address them and retry.")
        stop(paste(err, collapse=" "), call.=FALSE)
    }
    
    ## Else: On success ("ready"):
    if (length(resolutions)) {
        ## If there are any resolved conflicts, seek confirmation to proceed,
        ## if required. Abort if authorization required and not obtained.
        if (confirm && !askForPermission("Accept these resolutions?")) {
            err <- c("Permission to automatically resolve conflicts not given.",
                "Aborting. Please manually resolve conflicts, or set",
                "confirm=FALSE, and try again.")
            stop(paste(err, collapse=" "), call.=FALSE)
        }
    }
    
    ## Proceed.
    batch <- setCrunchSlot(batch, "status", "importing")
    pollBatchStatus(batch_url, batches(dataset), until="imported")
    invisible(refresh(dataset))
}

askForPermission <- function (prompt="") {
    if (!interactive()) return(FALSE)
    prompt <- paste(prompt, "(y/n) ")
    proceed <- ""
    while (!(proceed %in% c("y", "n"))) {
        proceed <- tolower(readline(prompt))
    }
    return(proceed == "y")
}