init.DatasetCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(selectFrom("name", .Object@index))]
    return(.Object)
}
setMethod("initialize", "DatasetCatalog", init.DatasetCatalog)

setMethod("active", "DatasetCatalog", function (x) {
    x@index <- Filter(function (a) !isTRUE(a$archived), x@index)
    ## Not implementing user check now. 
    # me <- sessionURL("user_url")
    # if (!is.null(me)) {
    #     x@index <- Filter(function (a) a$owner_id == me, x@index)
    # }
    return(x)
})

setMethod("archived", "DatasetCatalog", function (x) {
    x@index <- Filter(function (a) isTRUE(a$archived), x@index)
    ## Not implementing user check now. 
    # me <- sessionURL("user_url")
    # if (!is.null(me)) {
    #     x@index <- Filter(function (a) a$owner_id == me, x@index)
    # }
    return(x)
})

setMethod("[[", c("DatasetCatalog", "character"), function (x, i, ...) {
    DatasetTuple(index_url=self(x), entity_url=i, body=x@index[[i]])
})
setMethod("[[", c("DatasetCatalog", "ANY"), function (x, i, ...) {
    DatasetTuple(index_url=self(x), entity_url=names(x@index)[i],
        body=x@index[[i]])
})