init.VariableCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@order <- do.call(VariableGrouping,
        GET(.Object@views$hierarchical_order)$groups)
    return(.Object)
}
setMethod("initialize", "VariableCatalog", init.VariableCatalog)

setMethod("ordering", "VariableCatalog", function (x) x@order)

setMethod("active", "VariableCatalog", function (x) {
    x@index <- selectFromWhere(!isTRUE(discarded),
        x@index[intersect(entities(ordering(x)), names(x@index))],
        simplify=FALSE)
    return(x)
})

setMethod("hidden", "VariableCatalog", function (x) {
    x@index <- selectFromWhere(isTRUE(discarded), x@index, simplify=FALSE)
    return(x)
})

setMethod("[", c("VariableCatalog", "ANY"), function (x, i, ..., drop) {
    x@index[i]
})

setMethod("[[", c("VariableCatalog", "character"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=i, body=x@index[[i]])
})
setMethod("[[", c("VariableCatalog", "ANY"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=names(x@index)[i],
        body=x@index[[i]])
})