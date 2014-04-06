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
    x@index <- x@index[i]
    return(x)
})
setMethod("[<-", c("VariableCatalog", "character", "missing", "VariableCatalog"), function (x, i, j, value) {
    ## Validate!
    x@index[i] <- value@index[i]
    ## No save, I don't think. PATCH outside this fn? 
    return(x)
})

setMethod("[[", c("VariableCatalog", "character"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=i, body=x@index[[i]])
})
setMethod("[[", c("VariableCatalog", "ANY"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=names(x@index)[i],
        body=x@index[[i]])
})
setMethod("[[<-", c("VariableCatalog", "character", "missing", "VariableTuple"), 
    function (x, i, j, value) {
        x@index[[i]] <- value@body
        return(x)
    })
setMethod("[[<-", c("VariableCatalog", "character", "missing", "CrunchVariable"), 
   function (x, i, j, value) {
       stopifnot(i == self(value))
       x[[i]] <- tuple(value)
       return(x)
   })

setMethod("length", "VariableCatalog", function (x) length(x@index))
setMethod("lapply", "VariableCatalog", function (X, FUN, ...) lapply(X@index, FUN, ...))

urls <- function (x) {
    names(x@index)
}