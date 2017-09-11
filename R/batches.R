addBatch <- function (ds, ..., strict=TRUE, first_batch=FALSE, body=list(...)) {
    batches_url <- shojiURL(ds, "catalogs", "batches")
    if (!strict) {
        ## This is apparently deprecated in favor of passing in "strict" differently
        batches_url <- paste0(batches_url, "?strict=0")
    }

    if (first_batch) {
        ## If this is the first batch, it's an "import", so delete the dataset
        ## if it fails--no need to keep a worthless dataset entity around
        do_it <- function (expr) {
            tryCatch(eval(expr), error=function (e) {
                ## We failed to add the batch successfully, so we don't really have
                ## a useful dataset. So delete the entity that was created initially
                with_consent(delete(ds))
                stop(e)
            })
        }
    } else {
        ## Don't print "Result URL" if the job fails because the dataset will
        ## be rolled back and that URL won't exist
        do_it <- suppressMessages
    }
    do_it({
        body <- wrapEntity(body=body)
        suppressMessages(crPOST(batches_url, body=toJSON(body)))
    })
    invisible(refresh(ds))
}

addBatchFile <- function (dataset, file, ...) {
    if (grepl("^[a-z0-9]+://", file)) {
        ## S3, or other file on the web
        if (startsWith(file, "s3")) {
            ## We can post s3 URLs directly
            return(addBatch(dataset, url=file, ...))
        } else {
            ## We have to create a source first
            return(addBatch(dataset, source=createSource(url=file), ...))
        }
    } else {
        ## Local file. Send it as file upload
        return(addBatch(dataset, source=createSource(file), ...))
    }
}

#' @importFrom httr upload_file
createSource <- function (file, url, ...) {
    sources_url <- sessionURL("sources")
    if (!missing(file)) {
        if (file.exists(file)) {
            u <- crPOST(sources_url,
                body=list(uploaded_file=upload_file(file)), ...)
        } else {
            halt("File not found")
        }
    } else if (!missing(url)) {
        u <- crPOST(sources_url, body=toJSON(wrapEntity(location=url, ...)),
            ## TODO: all JSON POSTs/PATCHes/PUTs should declare their content-type
            ## this is just the first place where the backend has rejected without it
            config=add_headers(`Content-Type`="application/json"))
    } else {
        halt("Must provide a file or url to createSource")
    }
    return(u)
}

#' @importFrom methods initialize
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
