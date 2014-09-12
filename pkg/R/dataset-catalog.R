init.DatasetCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(selectFrom("name", .Object@index))]
    return(.Object)
}
setMethod("initialize", "DatasetCatalog", init.DatasetCatalog)

setMethod("active", "DatasetCatalog", function (x) {
    index(x) <- Filter(function (a) !isTRUE(a$archived), index(x))
    ## Not implementing user check now. 
    # me <- sessionURL("user_url")
    # if (!is.null(me)) {
    #     index(x) <- Filter(function (a) a$owner_id == me, index(x))
    # }
    return(x)
})

setMethod("archived", "DatasetCatalog", function (x) {
    index(x) <- Filter(function (a) isTRUE(a$archived), index(x))
    ## Not implementing user check now. 
    # me <- sessionURL("user_url")
    # if (!is.null(me)) {
    #     index(x) <- Filter(function (a) a$owner_id == me, index(x))
    # }
    return(x)
})

setMethod("[[", c("DatasetCatalog", "character"), function (x, i, ...) {
    DatasetTuple(index_url=self(x), entity_url=i, body=index(x)[[i]])
})
setMethod("[[", c("DatasetCatalog", "ANY"), function (x, i, ...) {
    DatasetTuple(index_url=self(x), entity_url=urls(x)[i],
        body=index(x)[[i]])
})

setMethod("names", "DatasetCatalog", function (x) {
    vapply(index(x), function (a) a$name, character(1), USE.NAMES=FALSE)
})