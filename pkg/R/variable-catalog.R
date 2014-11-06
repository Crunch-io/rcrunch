init.VariableCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    # print(.Object)
    # print(names(.Object@index))
    .Object@index <- lapply(.Object@index, function (x, b) {
        for (i in c("subvariables", "subvariables_catalog")) {
            if (!is.null(x[[i]])) {
                x[[i]] <- absolutizeURLs(x[[i]], b)
            }
        }
        return(x)
    }, b=.Object@self)
    # print(.Object)
    h_url <- .Object@views$hierarchical_order
    if (!is.null(h_url)) {
        .Object@order <- VariableOrder(GET(h_url))
    }
    # print(names(.Object@index))
    return(.Object)
}
setMethod("initialize", "VariableCatalog", init.VariableCatalog)

setMethod("active", "VariableCatalog", function (x) {
    index(x) <- selectFromWhere(!isTRUE(discarded),
        index(x)[intersect(entities(ordering(x)), urls(x))],
        simplify=FALSE)
    return(x)
})

setMethod("hidden", "VariableCatalog", function (x) {
    index(x) <- selectFromWhere(isTRUE(discarded), index(x), simplify=FALSE)
    return(x)
})

setMethod("[<-", c("VariableCatalog", "character", "missing", "VariableCatalog"), function (x, i, j, value) {
    ## Validate!
    index(x)[i] <- index(value)[i]
    ## No save, I don't think. PATCH outside this fn? 
    return(x)
})

setMethod("[[", c("VariableCatalog", "character"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=i, body=index(x)[[i]])
})
setMethod("[[", c("VariableCatalog", "ANY"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=urls(x)[i],
        body=index(x)[[i]])
})
setMethod("[[<-", c("VariableCatalog", "character", "missing", "VariableTuple"), 
    function (x, i, j, value) {
        index(x)[[i]] <- value@body
        return(x)
    })
setMethod("[[<-", c("VariableCatalog", "character", "missing", "CrunchVariable"), 
   function (x, i, j, value) {
       stopifnot(i == self(value))
       x[[i]] <- tuple(value)
       return(x)
   })

setMethod("names", "VariableCatalog", function (x) {
    vapply(index(x), function (a) a[["name"]], character(1), USE.NAMES=FALSE)
})
setMethod("names<-", "VariableCatalog", function (x, value) {
    mapSetIndexSlot(x, "name", value)
})
##' @export
setMethod("aliases", "VariableCatalog", function (x) {
    vapply(index(x), function (a) a[["alias"]], character(1), USE.NAMES=FALSE)
})
##' @export
setMethod("aliases<-", "VariableCatalog", function (x, value) {
    mapSetIndexSlot(x, "alias", value)
})