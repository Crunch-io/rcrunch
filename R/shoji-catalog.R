init.ShojiCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absoluteURL(names(.Object@index), .Object@self)
    return(.Object)
}
setMethod("initialize", "ShojiCatalog", init.ShojiCatalog)

is.shojiCatalog <- function (x) inherits(x, "ShojiCatalog")

setIndexSlot <- function (x, i, value) {
    if (length(value) == 1) value <- rep(value, length(x))
    stopifnot(length(x) == length(value))

    old <- index(x)
    index(x) <- mapply(function (a, v) {
        a[[i]] <- v
        return(a)
    }, a=index(x), v=value, SIMPLIFY=FALSE)
    to.update <- dirtyElements(old, index(x))
    if (any(to.update)) {
        ## Make sure certain fields are [] in the JSON
        ensure <- c("subvariables")
        payload <- lapply(index(x)[to.update], function (p) {
            p <- p[i] ## Let's only PATCH the field we're changing
            these <- intersect(ensure, names(p))
            if (length(these)) p[these] <- lapply(p[these], I)
            return(p)
        })
        crPATCH(self(x), body=toJSON(payload))
    }
    return(x)
}

dirtyElements <- function (x, y) {
    !mapply(identical, x, y, USE.NAMES=FALSE, SIMPLIFY=TRUE)
}

getIndexSlot <- function (x, i, what=character(1)) {
    vapply(index(x), function (a) a[[i]], what, USE.NAMES=FALSE)
}

##' @rdname catalog-extract
##' @export
setMethod("[", c("ShojiCatalog", "character"), function (x, i, ...) {
    w <- match(i, urls(x))
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, value)
})
##' @rdname catalog-extract
##' @export
setMethod("[", c("ShojiCatalog", "numeric"), function (x, i, ...) {
    bad <- abs(as.integer(i)) > length(x)
    if (any(bad)) {
        halt("Subscript out of bounds: ", i[bad])
    }
    callNextMethod(x, i, value)
})
##' @rdname catalog-extract
##' @export
setMethod("[", c("ShojiCatalog", "logical"), function (x, i, ...) {
    if (length(i) > length(x)) {
        halt("Subscript out of bounds: got ", length(i), " logicals, need ",
            length(x))
    }
    index(x) <- index(x)[i]
    return(x)
})
##' @rdname catalog-extract
##' @export
setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    index(x) <- index(x)[i]
    return(x)
})
##' @rdname catalog-extract
##' @export
setMethod("[[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    index(x)[[i]]
})
##' Length of Catalog
##' @param x a Catalog
##' @return Integer: the number of elements in the index list
##' @name catalog-length
NULL

##' @rdname catalog-length
##' @export
setMethod("length", "ShojiCatalog", function (x) length(index(x)))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(index(X), FUN, ...))

##' Get the body of a Catalog
##'
##' The core of Catalog data is in its "index". These methods get and set that
##' slot.
##' @param x a Catalog (VariableCatalog, Subvariables, or similar object)
##' @param value For the setters, an appropriate-length list to
##' assign
##' @return Getters return the list object in the "index" slot; setters
##' return \code{x} duly modified.
##' @aliases index index<-
##' @name index
NULL

##' @rdname index
##' @export
setMethod("index", "ShojiCatalog", function (x) x@index)
##' @rdname index
##' @export
setMethod("index<-", "ShojiCatalog", function (x, value) {
    x@index <- value
    return(x)
})

##' Get the URLs contained in a Catalog or Order object
##'
##' Sometimes it is useful to extract flattened vector of URLs from more
##' complex objects for purposes like subsetting or doing set comparisons.
##'
##' @param x a Catalog, Order, or Group object
##' @return A character vector of URLs
##' @aliases urls
##' @keywords internal
##' @rdname urls
##' @export
setMethod("urls", "ShojiCatalog", function (x) names(index(x)))

##' @export
as.list.ShojiCatalog <- function (x, ...) lapply(names(index(x)), function (i) x[[i]])

##' Utility to get a more human-readable view of a Shoji Catalog
##'
##' @param x ShojiCatalog or subclass
##' @param keys character vector of attribute names from each catalog tuple to
##' include in the result. Default is TRUE, which means all.
##' @param rownames See \code{\link[base]{data.frame}}, the \code{row.names}
##' argument, to which this is passed in \code{data.frame}. The difference here
##' is that if \code{rownames} is explicitly set as \code{NULL}, the resulting
##' object will not have row names set. By default, row names will be the URLs
##' of the catalog tuples.
##' @param ... additional arguments passed to \code{data.frame}
##' @return a \code{data.frame} view of the catalog
##' @export
catalogToDataFrame <- function (x, keys=TRUE, rownames, ...) {
    default.rownames <- missing(rownames)
    if (default.rownames) {
        rownames <- NULL
    }
    out <- data.frame(do.call(rbind, lapply(index(x), function (a) a[keys])),
        row.names=rownames, ...)
    if (default.rownames) {
        rownames(out) <- NULL
    }
    return(out)
}
