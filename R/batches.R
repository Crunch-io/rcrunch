addBatch <- function (ds, ..., savepoint=TRUE, autorollback=savepoint, strict=TRUE) {
    batches_url <- shojiURL(ds, "catalogs", "batches")
    if (!strict) {
        ## This is apparently deprecated in favor of passing in "strict" differently
        batches_url <- paste0(batches_url, "?strict=0")
    }
    ## Not using wrapEntity because there are elements outside of body.
    body <- list(
        element="shoji:entity",
        body=list(...),
        autorollback=autorollback,
        savepoint=savepoint
    )

    if (autorollback) {
        ## Don't print "Result URL" if the job fails because the dataset will
        ## be rolled back and that URL won't exist
        do_it <- suppressMessages
    } else {
        ## Just execute and let the "Result URL" print if it fails
        do_it <- force
    }
    do_it(crPOST(batches_url, body=toJSON(body)))
    invisible(refresh(ds))
}

addBatchFile <- function (dataset, file, ...) {
    if (grepl("^[a-z0-9]+://", file)) {
        ## S3, or other file on the web
        return(addBatch(dataset, url=file, ...))
    } else {
        ## Local file. Send it as file upload
        return(addBatch(dataset, source=createSource(file), ...))
    }
}

#' @importFrom httr upload_file
createSource <- function (file, ...) {
    crPOST(sessionURL("sources"),
        body=list(uploaded_file=upload_file(file)), ...)
}

setMethod("initialize", "BatchCatalog", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(names(.Object@index))]
    return(.Object)
})

setMethod("imported", "BatchCatalog", function (x) {
    index(x) <- Filter(function (a) isTRUE(a$status == "imported"), index(x))
    return(x)
})

setMethod("pending", "BatchCatalog", function (x) {
    index(x) <- Filter(function (a) !isTRUE(a$status == "imported"), index(x))
    return(x)
})

#' @rdname describe-catalog
#' @export
setMethod("names", "BatchCatalog", function (x) urls(x))

#' Remove batches from a dataset
#'
#' Sometimes append operations do not succeed, whether due to conflicts between
#' the two datasets or other server-side issues. Failed appends can leave behind
#' "error" status batch records, which can cause confusion. This function lets
#' you delete batches that don't match the status or statuses you want to keep.
#' @param dataset CrunchDataset
#' @param keep character batch status(es) you want to keep. By default, batches
#' that don't have either "imported" or "appended" status will be deleted.
#' @return \code{dataset} with the undesired batches removed.
#' @export
cleanseBatches <- function (dataset, keep=c("imported", "appended")) {
    bat.cat <- batches(dataset)
    to.delete <- names(Filter(function (a) !(a$status %in% keep), index(bat.cat)))
    for (u in to.delete) {
        try(crDELETE(u))
    }
    if (length(to.delete)) {
        ## Bust cache
        dropOnly(self(bat.cat))
    }
    return(dataset)
}
