setMethod("initialize", "DatasetCatalog", init.sortCatalog)

setMethod("active", "DatasetCatalog", function(x) {
    index(x) <- Filter(function(a) !isTRUE(a$archived), index(x))
    return(x)
})

setMethod("archived", "DatasetCatalog", function(x) {
    index(x) <- Filter(function(a) isTRUE(a$archived), index(x))
    return(x)
})

#' See who owns these datasets
#'
#' @param x DatasetCatalog
#' @return For `owners`, the URLs of the users or projects that own
#' these datasets. For `ownerNames`, their names.
#' @export
owners <- function(x) getIndexSlot(x, "owner_id")

#' @rdname owners
#' @export
ownerNames <- function(x) getIndexSlot(x, "owner_display_name")

#' @rdname crunch-extract
#' @export
setMethod("[[", c("DatasetCatalog", "numeric"), function(x, i, ...) {
    getTuple(x, i, DatasetTuple)
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("DatasetCatalog", "character", "missing", "DatasetTuple"),
    function(x, i, j, value) {
        index(x)[[i]] <- value@body
        return(x)
    }
)
