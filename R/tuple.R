#' Methods for ShojiTuples
#'
#' ShojiTuples are objects extracted from ShojiCatalogs. They are internally
#' used.
#'
#' @param x a Tuple
#' @param object Same as `x` but for the `alias` method, in order to
#' match the generic from another package.
#' @param name a Tuple slot to get or set
#' @param i In [[, a Tuple slot to get
#' @param ... additional arguments to [[, ignored
#' @param value What to set in a given slot
#' @name tuple-methods
#' @aliases entity
#' @keywords internal
NULL

#' @rdname tuple-methods
#' @export
setMethod("refresh", "ShojiTuple", function (x) {
    dropCache(x@index_url)
    catalog <- ShojiCatalog(crGET(x@index_url))
    tup <- catalog[[x@entity_url]]
    if (is.null(tup)) {
        ## Get the object type from the (sub)class name
        cls <- sub("Tuple$", "", class(x))
        if (cls == "Shoji") cls <- "Object"
        halt(cls, " not found. It may have been deleted.")
    }
    x@body <- tup
    return(x)
})

#' @rdname tuple-methods
#' @export
setMethod("$", "ShojiTuple", function (x, name) x@body[[name]])
#' @rdname tuple-methods
#' @export
setMethod("$<-", "ShojiTuple", function (x, name, value) {
    x@body[[name]] <- value
    return(x)
})
#' @rdname tuple-methods
#' @export
setMethod("[[", "ShojiTuple", function (x, i) x@body[[i]])
#' @rdname tuple-methods
#' @export
setMethod("[[<-", "ShojiTuple", function (x, i, value) {
    x@body[[i]] <- value
    return(x)
})

setTupleSlot <- function (x, name, value) {
    if (!inherits(x, "ShojiTuple")) {
        tuple(x) <- setTupleSlot(tuple(x), name, value)
    } else if (!identical(x[[name]], value)) {
        ## Skip updating if not modified
        x[[name]] <- value
        payload <- toJSON(structure(list(structure(list(value), .Names=name)),
            .Names=x@entity_url))
        crPATCH(x@index_url, body=payload)
    }
    invisible(x)
}

#' @rdname tuple-methods
#' @export
setMethod("self", "ShojiTuple", function (x) x@entity_url)

#' @rdname tuple-methods
#' @export
setMethod("entity", "VariableTuple", function (x) {
    return(VariableEntity(crGET(x@entity_url)))
})

#' @rdname tuple-methods
#' @export
setMethod("entity", "CrunchVariable", function (x) {
    return(VariableEntity(crGET(self(x))))
})
#' @rdname tuple-methods
#' @export
setMethod("entity", "DatasetTuple", function (x) {
    return(as.dataset(crGET(x@entity_url), tuple=x))
})

#' @rdname tuple-methods
#' @export
setMethod("delete", "ShojiTuple", function (x, ...) {
    crDELETE(x@entity_url, drop=dropCache(x@index_url))
})
#' @rdname tuple-methods
#' @export
setMethod("delete", "DatasetTuple", function (x, ...) {
    prompt <- paste0("Really delete dataset ", dQuote(name(x)), "?")
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting dataset")
    }
    out <- callNextMethod()
    invisible(out)
})

#' @rdname tuple-methods
#' @export
setMethod("name", "ShojiTuple", vget("name"))

#' @rdname tuple-methods
#' @export
setMethod("alias", "ShojiTuple", function(object) vget("alias")(object))

#' @rdname tuple-methods
#' @export
setMethod("description", "ShojiTuple", function(x) vget("description")(x) %||% "")

#' @rdname tuple-methods
#' @export
setMethod("notes", "ShojiTuple", function(x) vget("notes")(x) %||% "")

#' @rdname tuple-methods
#' @export
setMethod("name<-", "ShojiTuple",
    function (x, value) setTupleSlot(x, "name", validateNewName(value)))

#' @rdname tuple-methods
#' @export
setMethod("type", "ShojiTuple", vget("type"))
