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
