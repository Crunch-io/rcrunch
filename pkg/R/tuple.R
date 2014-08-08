setMethod("refresh", "IndexTuple", function (x) {
    index.list <- GET(x@index_url)$index
    x@body <- index.list[[x@entity_url]]
    return(x)
})

setMethod("$", "IndexTuple", function (x, name) x@body[[name]]) 
setMethod("$<-", "IndexTuple", function (x, name, value) {
    x@body[[name]] <- value
    return(x)
})
setMethod("[[", "IndexTuple", function (x, i) x@body[[i]])
setMethod("[[<-", "IndexTuple", function (x, i, value) {
    x@body[[i]] <- value
    return(x)
})

setTupleSlot <- function (x, name, value) {
    if (!inherits(x, "IndexTuple")) {
        tuple(x) <- setTupleSlot(tuple(x), name, value)
    } else {
        x[[name]] <- value
        ## NB: no readonly mode. implement later if needed.
        payload <- toJSON(structure(list(x@body), .Names=x@entity_url))
        try(PATCH(x@index_url, body=payload))
    }
    invisible(x)
}

setMethod("entity", "VariableTuple", function (x) {
    return(as.variable(GET(x@entity_url), tuple=x))
})

setMethod("entity", "DatasetTuple", function (x) {
    return(as.dataset(GET(x@entity_url), tuple=x))
})

setMethod("delete", "IndexTuple", function (x) DELETE(x@entity_url))

setMethod("delete", "DatasetTuple", function (x, confirm=interactive(), ...) {
    prompt <- paste0("Really delete dataset ", dQuote(name(x)), "?")
    if (confirm && !askForPermission(prompt)) {
        stop("Must confirm deleting dataset", call.=FALSE)
    }
    out <- callNextMethod()
    updateDatasetList()
    invisible(out)
})

setMethod("name", "IndexTuple", function (x) x$name)
setMethod("type", "IndexTuple", function (x) x$type)