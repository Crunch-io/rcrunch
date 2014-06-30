##' @export
setMethod("subvariables", "CategoricalArrayVariable", function (x) {
    vars <- VariableCatalog(GET(tuple(x)@index_url))
    return(Subvariables(vars[x@body$subvariables]))
})

##' @export
setMethod("subvariables<-", c("CategoricalArrayVariable", "ANY"),
    function (x, value) {
        stop("Can only assign an object of class Subvariables", call.=FALSE)
    })
setMethod("subvariables<-", c("CategoricalArrayVariable", "Subvariables"),
    function (x, value) {
        old <- x@body$subvariables
        new <- names(value@index)
        if (!setequal(old, new)) {
            stop("Can only reorder, not change, subvariables", call.=FALSE)
        }
        return(setCrunchSlot(x, "subvariables", new))
    })

setMethod("names", "Subvariables", function (x) {
    vapply(x@index, function (a) a$name, character(1), USE.NAMES=FALSE)
})

setMethod("names<-", "Subvariables", function (x, value) {
    stopifnot(is.character(value), length(x) == length(value),
        !any(duplicated(value)))
    x@index <- mapply(function (tuple, val) {
            tuple[["name"]] <- val
            return(tuple)
        }, tuple=x@index, val=value, SIMPLIFY=FALSE, USE.NAMES=TRUE)
    PATCH(self(x), body=toJSON(x@index))
    return(x)
})

setMethod("[[", c("Subvariables", "character"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=i, body=x@index[[i]])
})
setMethod("[[", c("Subvariables", "ANY"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=names(x@index)[i],
        body=x@index[[i]])
})