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
    varcat_url <- x@urls$variables_url
    if (substr(varcat_url, nchar(varcat_url), nchar(varcat_url)) == "/") {
        ## To work around test fixtures
        ## Add query params
        print("here")
        return(VariableCatalog(GET(varcat_url, 
            query=list(relative="on", nosubvars=1))))
    } else {
        return(VariableCatalog(GET(varcat_url)))
    }
}

getNrow <- function (dataset, filtered=TRUE) {
    which.count <- ifelse(isTRUE(filtered), "filtered", "total")
    ## use filtered by default because every other request will take the applied filter
    
    summary_url <- dataset@urls$summary_url
    nrows <- as.integer(round(GET(summary_url)$unweighted[[which.count]]))
    return(nrows)
}

##' Is it?
##' @rdname crunch-is
##' @param x an object
##' @return logical
##' @export 
is.dataset <- function (x) inherits(x, "CrunchDataset")

setDatasetName <- function (x, value) {
    out <- setTupleSlot(x, "name", value)
    updateDatasetList() ## could just modify rather than refresh
    invisible(out)
}
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
setMethod("dim", "CrunchDataset",
    function (x) c(x@.nrow, length(variables(x))))

namekey <- function (dataset) ifelse(dataset@useAlias, "alias", "name")

##' @export
setMethod("names", "CrunchDataset", function (x) {
    findVariables(x, key=namekey(x), value=TRUE)
})

##' Dataset weights
##' @param x a Dataset
##' @param value a Variable to set as weight, or NULL to remove the existing weight
##' @return For the getter, a Variable if there is a weight, else NULL. For the setter, x, modified accordingly
##' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    w <- x@body$weight
    if (!is.null(w)) {
        w <- entity(allVariables(x)[[w]])
    }
    return(w)
}

##' @rdname weight 
##' @export
`weight<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        halt("Weight must be a Variable or NULL")
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

##' @rdname refresh
##' @export
setMethod("refresh", "CrunchDataset", function (x) {
    as.dataset(GET(self(x)), useAlias=x@useAlias, tuple=refresh(tuple(x)))
})

##' @rdname delete
##' @export
setMethod("delete", "CrunchDataset", 
    function (x, confirm=interactive() | is.readonly(x), ...) {
        out <- delete(tuple(x), confirm=confirm)
        invisible(out)
    })

##' @export
as.list.CrunchDataset <- function (x, ...) {
    lapply(seq_along(variables(x)), function (i) x[[i]])
}

batches <- function (x) BatchCatalog(GET(x@catalogs$batches))

joins <- function (x) ShojiCatalog(GET(x@catalogs$joins))

setDatasetVariables <- function (x, value) {
    v <- urls(value)
    x@variables[v] <- value
    if (!identical(ordering(x@variables), ordering(value))) {
        ordering(x@variables) <- ordering(value)
    }
    return(x)
}

##' @export
setMethod("variables", "CrunchDataset", function (x) active(allVariables(x)))
##' @export
setMethod("variables<-", c("CrunchDataset", "VariableCatalog"),
    setDatasetVariables)
##' @export
setMethod("allVariables", "CrunchDataset", function (x) x@variables)
##' @export
setMethod("allVariables<-", c("CrunchDataset", "VariableCatalog"), 
    setDatasetVariables)
