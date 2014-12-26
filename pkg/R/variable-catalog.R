init.VariableCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    # print(.Object)
    # print(names(.Object@index))
    .Object@index <- lapply(.Object@index, function (x, b) {
        if ("subvariables" %in% names(x)) {
            ## unlist, for jsonlite
            x[["subvariables"]] <- absolutizeURLs(unlist(x[["subvariables"]]),
                b)
        }
        if ("subvariables_catalog" %in% names(x)) {
            x[["subvariables_catalog"]] <- absolutizeURLs(x[["subvariables_catalog"]], b)
        }
        # for (i in c("subvariables", "subvariables_catalog")) {
        #     if (!is.null(x[[i]])) {
        #         x[[i]] <- absolutizeURLs(x[[i]], b)
        #     }
        # }
        return(x)
    }, b=.Object@self)
    # print(.Object)
    h_url <- .Object@views$hierarchical_order
    if (!is.null(h_url)) {
        o <- crGET(h_url, query=list(relative="on"))
        # print(o)
        .Object@order <- VariableOrder(o)
    }
    # print(names(.Object@index))
    return(.Object)
}
setMethod("initialize", "VariableCatalog", init.VariableCatalog)

setMethod("active", "VariableCatalog", function (x) {
    index(x) <- selectFromWhere(!isTRUE(discarded),
        index(x)[intersect(urls(ordering(x)), urls(x))],
        simplify=FALSE)
    return(x)
})

setMethod("hidden", "VariableCatalog", function (x) {
    index(x) <- selectFromWhere(isTRUE(discarded), index(x), simplify=FALSE)
    return(x)
})

##' @rdname catalog-extract
##' @export
setMethod("[<-", c("VariableCatalog", "character", "missing", "VariableCatalog"), function (x, i, j, value) {
    ## Validate!
    index(x)[i] <- index(value)[i]
    ## No save, I don't think. PATCH outside this fn? 
    return(x)
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("VariableCatalog", "character"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=i, body=index(x)[[i]])
})
##' @rdname catalog-extract
##' @export
setMethod("[[", c("VariableCatalog", "ANY"), function (x, i, ...) {
    VariableTuple(index_url=self(x), entity_url=urls(x)[i],
        body=index(x)[[i]])
})
##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("VariableCatalog", "character", "missing", "VariableTuple"), 
    function (x, i, j, value) {
        index(x)[[i]] <- value@body
        return(x)
    })
##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("VariableCatalog", "character", "missing", "CrunchVariable"), 
    function (x, i, j, value) {
        stopifnot(i == self(value))
        x[[i]] <- tuple(value)
        return(x)
    })

##' Get and set names, aliases on Catalog-type objects
##' 
##' These methods let you get and set names and aliases for variables in a
##' Dataset's catalog, or within \code{\link{Subvariables}} in an array 
##' variable. They work like the base R names methods.
##'
##' Note that the \code{names} method on a Dataset returns the aliases of its
##' variables by default. See the vignette on variables for more information.
##'
##' @param x a VariableCatalog, Subvariables, or similar object
##' @param value For the setters, an appropriate-length character vector to
##' assign
##' @return Getters return the character object in the specified slot; setters
##' return \code{x} duly modified.
##' @export
##' @aliases describe-catalog aliases aliases<-
##' @seealso Subvariables Categories base::names
##' @rdname describe-catalog
setMethod("names", "VariableCatalog", function (x) {
    vapply(index(x), function (a) a[["name"]], character(1), USE.NAMES=FALSE)
})
##' @export
##' @rdname describe-catalog
setMethod("names<-", "VariableCatalog", function (x, value) {
    mapSetIndexSlot(x, "name", value)
})
##' @export
##' @rdname describe-catalog
setMethod("aliases", "VariableCatalog", function (x) {
    vapply(index(x), function (a) a[["alias"]], character(1), USE.NAMES=FALSE)
})
##' @export
##' @rdname describe-catalog
setMethod("aliases<-", "VariableCatalog", function (x, value) {
    mapSetIndexSlot(x, "alias", value)
})