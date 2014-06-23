init.CrunchDataset <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@variables <- getDatasetVariables(.Object)
    ## So they test as identical, force order
    .Object@variables@index <- .Object@variables@index[order(names(.Object@variables@index))]
    .Object@.nrow <- getNrow(.Object)
    return(.Object)
}
setMethod("initialize", "CrunchDataset", init.CrunchDataset)

getDatasetVariables <- function (x) {
    return(VariableCatalog(GET(x@urls$variables_url)))
}

getNrow <- function (dataset, filtered=TRUE) {
    which.count <- ifelse(isTRUE(filtered), "filtered", "total")
    ## use filtered by default because every other request will take the applied filter
    
    summary_url <- dataset@urls$summary_url
    nrows <- as.integer(round(GET(summary_url)$rows[[which.count]]))
    return(nrows)
}

##' Is it?
##' @rdname crunch-is
##' @param x an object
##' @return logical
##' @export 
is.dataset <- function (x) inherits(x, "CrunchDataset")

setDatasetName <- function (x, value) setTupleSlot(x, "name", value)
setDatasetDescription <- function (x, value) {
    setTupleSlot(x, "description", value)
}

##' @export
setMethod("name", "CrunchDataset", function (x) tuple(x)$name)
##' @export
setMethod("name<-", "CrunchDataset", setDatasetName)
##' @export
setMethod("description", "CrunchDataset", function (x) tuple(x)$description)
##' @export
setMethod("description<-", "CrunchDataset", setDatasetDescription)

as.dataset <- function (x, useAlias=default.useAlias(), tuple=DatasetTuple()) {
    out <- CrunchDataset(x)
    out@useAlias <- useAlias
    tuple(out) <- tuple
    return(out)
}

##' @export
setMethod("dim", "CrunchDataset", function (x) c(x@.nrow, length(active(x@variables))))

namekey <- function (dataset) ifelse(dataset@useAlias, "alias", "name")

##' @export
setMethod("names", "CrunchDataset", function (x) {
    findVariables(x, key=namekey(x), value=TRUE)
})

variableNames <- function (x) {
    findVariables(x, key="name", value=TRUE)
}

##' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@variables <- active(x@variables)[i]
    readonly(x) <- TRUE ## we don't want to overwrite the big object accidentally
    return(x)
})
##' @export
setMethod("[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        stop("Undefined columns selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})
##' @export
setMethod("[[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    out <- active(x@variables)[[i]]
    if (!is.null(out)) {
        out <- try(entity(out), silent=TRUE)
        if (is.error(out)) {
            stop(attr(out, "condition")$message, call.=FALSE)
        }
    }
    return(out)
})
##' @export
setMethod("[[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    stopifnot(length(i) == 1)
    i <- match(i, names(x))
    if (is.na(i)) return(NULL)
    callNextMethod(x, i, ..., drop=drop)
})
##' @export
setMethod("$", "CrunchDataset", function (x, name) x[[name]])

.addVariableSetter <- function (x, i, value) {
    if (i %in% names(x)) {
        stop("Cannot currently overwrite existing Variables with [[<-",
            call.=FALSE)
    }
    addVariable(x, values=value, name=i, alias=i)
}

.updateVariableSetter <- function (x, i, value) {
    ## Confirm that i matches namekey(value)
    if (i != tuple(value)[[namekey(x)]]) {
        stop("Cannot overwrite one Variable with another", call.=FALSE)
    }
    x@variables[[self(value)]] <- value
    return(x)
}

##' @export
setMethod("[[<-", c("CrunchDataset", "character", "missing", "CrunchVariable"), 
    .updateVariableSetter)
setMethod("[[<-", c("CrunchDataset", "ANY", "missing", "CrunchVariable"), 
    function (x, i, value) .updateVariableSetter(x, names(x)[i], value))
setMethod("[[<-", c("CrunchDataset", "character", "missing", "ANY"), .addVariableSetter)
setMethod("[[<-", c("CrunchDataset", "ANY"), function (x, i, value) {
    stop("Only character (name) indexing supported for [[<-", call.=FALSE)
})
##' @export
setMethod("$<-", c("CrunchDataset"), function (x, name, value) {
    x[[name]] <- value
    return(x)
})

setMethod("[<-", c("CrunchDataset", "ANY", "missing", "list"), 
    function (x, i, j, value) {
        stopifnot(length(i) == length(value), 
            all(vapply(value, is.variable, logical(1))))
        for (z in seq_along(i)) {
            x[[i[z]]] <- value[[z]]
        }
        return(x)
    })

## TODO: add [<-.CrunchDataset, CrunchDataset/VariableCatalog

##' Get the dataset's weight
##' @param x a Dataset
##' @return a Variable if there is a weight, else NULL
##' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    w <- x@body$weight
    if (!is.null(w)) {
        w <- entity(x@variables[[w]])
    }
    return(w)
}

##' Set the dataset's weight
##' @param x a Dataset
##' @param value a Variable to set as weight, or NULL to remove the existing weight
##' @return x, modified accordingly
##' @export
`weight<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        stop("Weight must be a Variable or NULL")
    }
    x <- setCrunchSlot(x, "weight", value)
    return(x)
}

is.variable.tuple <- function (x) {
    is.list(x) && all(c("name", "alias", "type", "id") %in% names(x))
}

setMethod("tuple", "CrunchDataset", function (x) x@tuple)
setMethod("tuple<-", "CrunchDataset", function (x, value) {
    x@tuple <- value
    return(x)
})

setMethod("refresh", "CrunchDataset", function (x) {
    as.dataset(GET(self(x)), useAlias=x@useAlias, tuple=refresh(tuple(x)))
})

##' @export
setMethod("delete", "CrunchDataset", function (x) {
    out <- callNextMethod()
    updateDatasetList()
    invisible(out)
})

##' @S3method as.list CrunchDataset
as.list.CrunchDataset <- function (x, ...) {
    lapply(seq_along(active(x@variables)), function (i) x[[i]])
}

batches <- function (x) BatchCatalog(GET(x@catalogs$batches))