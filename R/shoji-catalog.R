setMethod("initialize", "ShojiCatalog", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absoluteURL(names(.Object@index), .Object@self)
    return(.Object)
})

is.catalog <- function (x) inherits(x, "ShojiCatalog")

getIndexSlot <- function (x, i, what=character(1), ifnot=NA_character_) {
    getListSlot(index(x), i, what, ifnot)
}

getListSlot <- function (x, i, what=character(1), ifnot=NA_character_) {
    ## Wrapper to vapply over a list data structure and select a value
    vapply(x, function (a) a[[i]] %||% ifnot, what, USE.NAMES=FALSE)
}

setIndexSlot <- function (x, i, value, unique=FALSE) {
    if (length(value) == 1) value <- rep(value, length(x))
    stopifnot(length(x) == length(value))

    old <- index(x)
    index(x) <- mapply(function (a, v) {
        a[[i]] <- v
        return(a)
    }, a=index(x), v=value, SIMPLIFY=FALSE)
    if (unique) {
        ## Check to see if any of the value is duplicated after updating
        newvals <- getIndexSlot(x, i) ## Assumes "character". Revisit if need unique for non-char
        dups <- duplicated(newvals)
        if (any(dups)) {
            halt("Duplicate values not permitted: ",
                serialPaste(unique(newvals[dups])))
        }
    }
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

#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "character"), function (x, i, ...) {
    w <- whichNameOrURL(x, i)
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "numeric"), function (x, i, ...) {
    bad <- abs(as.integer(i)) > length(x)
    if (any(bad)) {
        halt("Subscript out of bounds: ", capture.output(dput(i[bad])))
    }
    callNextMethod(x, i, value)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "logical"), function (x, i, ...) {
    if (length(i) > length(x)) {
        halt("Subscript out of bounds: got ", length(i), " logicals, need ",
            length(x))
    }
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    index(x) <- index(x)[i]
    return(x)
})
#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    ## Note that this returns a bare list, not an IndexTuple
    index(x)[[i]]
})

getTuple <- function (x, i, Constructor=IndexTuple, ...) {
    b <- index(x)[[i]]
    if (is.null(b)) return(NULL)
    Constructor(index_url=self(x), entity_url=urls(x)[i], body=b)
}

getEntity <- function (x, i, Constructor=ShojiEntity, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    if (is.na(url)) {
        halt("subscript out of bounds: ", i)
    }
    return(Constructor(crGET(url)))
}

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1L)
    w <- whichNameOrURL(x, i)
    if (is.na(w)) {
        return(NULL)
    }
    return(x[[w]])
})

#' @rdname catalog-extract
#' @export
setMethod("$", "ShojiCatalog", function (x, name) x[[name]])

#' @rdname catalog-extract
#' @export
setMethod("$<-", "ShojiCatalog", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

#' @rdname catalog-extract
#' @export
setMethod("[<-", c("ShojiCatalog", "ANY", "missing", "ShojiCatalog"),
    function (x, i, j, value) {
        index(x)[i] <- index(value)[i]
        ## Assume that PATCHing has happened outside this function
        return(x)
    })

#' Length of Catalog
#' @param x a Catalog
#' @return Integer: the number of elements in the index list
#' @name catalog-length
NULL

whichNameOrURL <- function (x, i, secondary=names(x)) {
    w <- match(i, secondary)
    if (any(is.na(w))) {
        w <- match(i, urls(x))
    } else {
        ## Warn if duplicated
        dups <- i %in% secondary[duplicated(secondary)]
        if (any(dups)) {
            bads <- i[dups]
            msg <- ifelse(length(bads) > 1,
                " do not uniquely identify elements. Returning the first matches",
                " does not uniquely identify elements. Returning the first match")
            warning(i, msg, call.=FALSE)
        }
    }
    return(w)
}

#' @rdname catalog-length
#' @export
setMethod("length", "ShojiCatalog", function (x) length(index(x)))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(index(X), FUN, ...))

#' Get the body of a Catalog
#'
#' The core of Catalog data is in its "index". These methods get and set that
#' slot.
#' @param x a Catalog (VariableCatalog, Subvariables, or similar object)
#' @param value For the setters, an appropriate-length list to
#' assign
#' @return Getters return the list object in the "index" slot; setters
#' return \code{x} duly modified.
#' @aliases index index<-
#' @name index
NULL

#' @rdname index
#' @export
setMethod("index", "ShojiCatalog", function (x) x@index)
#' @rdname index
#' @export
setMethod("index<-", "ShojiCatalog", function (x, value) {
    x@index <- value
    return(x)
})

#' Get the URLs contained in a Catalog or Order object
#'
#' Sometimes it is useful to extract flattened vector of URLs from more
#' complex objects for purposes like subsetting or doing set comparisons.
#'
#' @param x a Catalog, Order, or Group object
#' @return A character vector of URLs
#' @aliases urls
#' @keywords internal
#' @rdname urls
#' @export
setMethod("urls", "ShojiCatalog", function (x) names(index(x)))

#' @rdname urls
#' @export
setMethod("urls", "list", function (x) vapply(x, self, character(1))) ## Assumes list of entities

#' @rdname urls
#' @export
setMethod("urls", "character", function (x) x) ## Assumes already are URLs

#' @rdname describe-catalog
#' @export
setMethod("names", "ShojiCatalog", function (x) getIndexSlot(x, "name"))
#' @export
#' @rdname describe-catalog
setMethod("names<-", "ShojiCatalog", function (x, value) {
    setIndexSlot(x, "name", value, unique=TRUE)
})

#' @rdname describe-catalog
#' @export
setMethod("emails", "ShojiCatalog", function (x) getIndexSlot(x, "email"))

#' @export
as.list.ShojiCatalog <- function (x, ...) lapply(names(index(x)), function (i) x[[i]])

#' Utility to get a more human-readable view of a Shoji Catalog
#'
#' @param x ShojiCatalog or subclass
#' @param keys character vector of attribute names from each catalog tuple to
#' include in the result. Default is TRUE, which means all.
#' @param rownames See \code{\link[base]{data.frame}}, the \code{row.names}
#' argument, to which this is passed in \code{data.frame}. The difference here
#' is that if \code{rownames} is explicitly set as \code{NULL}, the resulting
#' object will not have row names set. By default, row names will be the URLs
#' of the catalog tuples.
#' @param ... additional arguments passed to \code{data.frame}
#' @return a \code{data.frame} view of the catalog
#' @export
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
