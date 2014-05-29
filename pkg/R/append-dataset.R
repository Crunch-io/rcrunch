appendDataset <- function (dataset1, dataset2, confirm=interactive()) {
    stopifnot(is.dataset(dataset1))
    
    if (!is.dataset(dataset2)) {
        ## TODO: compose batch directly, not as dataset?
        temp.ds.name <- paste("Appending to", name(dataset1),
            strftime(Sys.time(), usetz=TRUE))
        message("Creating ", sQuote(temp.ds.name), " as temporary dataset")
        dataset2 <- newDataset(dataset2, name=temp.ds.name)
    }
    
    batches_url <- dataset1@catalogs$batches
    body <- list(
        element="shoji:entity",
        body=list(
            dataset=self(dataset2), ## Totally guessing about this API
            workflow=I()
        )
    )
    batch_url <- POST(batches_url, body=toJSON(body))
    status <- try(pollBatchStatus(batch_url, GET(batches_url), until="ready"),
        silent=TRUE)
    batch <- GET(batch_url)
    
    if (is.error(status)) {
        message(conflicts) ## define conflicts
        DELETE(batch_url) ## right? for now at least?
        stop("Message about manually resolving conflicts and trying again")
    }
    
    ## On success ("ready"):
    resolutions <- batch@body$conflicts
    if (length(resolutions)) {
        ## Report on what was done/will be done
        message(resolutions) ## get var names matched, iterate over resolutions and print $message and $resolution
        if (confirm) {
            if (!interactive()) {
                DELETE(batch_url) ## right? for now at least?
                stop("Message about manually resolving conflicts and trying again")
            }
            ok <- FALSE
            while (!ok) {
                proceed <- tolower(readline("Accept these resolutions? (y/n) "))
                if (proceed == "y") {
                    ok <- TRUE
                } else if (proceed == "n") {
                    DELETE(batch_url) ## right? for now at least?
                    stop("Message about manually resolving conflicts and trying again")
                }
            }
        }
    }
    
    ## Proceed.
    batch <- setCrunchSlot(batch, "status", "importing") ## correct?
    pollBatchStatus(batch_url, GET(batches_url), until="imported")
    invisible(refresh(dataset1))
}