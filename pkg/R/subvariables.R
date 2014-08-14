##' @rdname Subvariables
##' @export
setMethod("subvariables", "CategoricalArrayVariable", function (x) {
    vars <- VariableCatalog(GET(tuple(x)@index_url))
    return(Subvariables(vars[x@body$subvariables]))
})

##' @rdname Subvariables
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

##' @rdname Subvariables
##' @export
setMethod("names", "Subvariables", function (x) {
    vapply(x@index, function (a) a$name, character(1), USE.NAMES=FALSE)
})

##' @rdname Subvariables
##' @export
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
    i <- match(i, names(x))
    if (is.na(i)) return(NULL)
    callNextMethod(x, i, ...)    
})
setMethod("[[", c("Subvariables", "ANY"), function (x, i, ...) {
    out <- VariableTuple(index_url=self(x), entity_url=names(x@index)[i],
        body=x@index[[i]])
    if (!is.null(out)) {
        out <- entity(out)
    }
    return(out)
})
setMethod("$", "Subvariables", function (x, name) x[[name]])

setMethod("[", c("Subvariables", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        stop("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ...)
})

##' @export
as.list.Subvariables <- function (x, ...) lapply(names(x), function (i) x[[i]])

setMethod("[", "CategoricalArrayVariable", function (x, i, ...) {
    return(subvariables(x)[i, ...])
})
setMethod("[[", "CategoricalArrayVariable", function (x, i, ...) {
    return(subvariables(x)[[i, ...]])
})
setMethod("$", "CategoricalArrayVariable", 
    function (x, name) subvariables(x)[[name]])
