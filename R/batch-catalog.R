init.BatchCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(names(.Object@index))]
    return(.Object)
}
setMethod("initialize", "BatchCatalog", init.BatchCatalog)

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
