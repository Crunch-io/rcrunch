init.BatchCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(names(.Object@index))]
    return(.Object)
}
setMethod("initialize", "BatchCatalog", init.BatchCatalog)

setMethod("imported", "BatchCatalog", function (x) {
    x@index <- Filter(function (a) isTRUE(a$status == "imported"), x@index)
    return(x)
})

setMethod("pending", "BatchCatalog", function (x) {
    x@index <- Filter(function (a) !isTRUE(a$status == "imported"), x@index)
    return(x)
})
