setMethod("initialize", "DatasetCatalog", init.sortCatalog)

setMethod("active", "DatasetCatalog", function(x) {
    index(x) <- Filter(function(a) !isTRUE(a$archived), index(x))
    return(x)
})

setMethod("archived", "DatasetCatalog", function(x) {
    index(x) <- Filter(function(a) isTRUE(a$archived), index(x))
    return(x)
})

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
