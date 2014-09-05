init.ShojiCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absolutizeURLs(names(.Object@index), .Object@self)
    return(.Object)
}
setMethod("initialize", "ShojiCatalog", init.ShojiCatalog)

absolutizeURLs <- function (urls, base) {
    ## Detect if we have relative urls, and then concatenate if so
    if (length(urls) && ## if there is anything to munge
        !any(substr(urls, 1, 4) == "http") && ## the urls don't start with http
        substr(base, nchar(base), nchar(base)) == "/") { ## because of test mock
            urls <- paste0(base, urls)
        }
    return(urls)
}

is.shojiCatalog <- function (x) inherits(x, "ShojiCatalog")

setIndexSlot <- function (x, i, value) {
    x@index <- lapply(x, function (a) {
        a[[i]] <- value
        return(a)
    })
    PATCH(self(x), body=toJSON(x@index))
    return(x)
}

mapSetIndexSlot <- function (x, i, value) {
    if (length(value) == 1) value <- rep(value, length(x))
    stopifnot(length(x) == length(value))
    
    old <- x@index
    x@index <- mapply(function (a, v) {
        a[[i]] <- v
        return(a)
    }, a=x@index, v=value, SIMPLIFY=FALSE)
    to.update <- dirtyElements(old, x@index)
    if (any(to.update)) {
        PATCH(self(x), body=toJSON(x@index[to.update]))
    }
    return(x)
}

dirtyElements <- function (x, y) {
    !mapply(identical, x, y, USE.NAMES=FALSE, SIMPLIFY=TRUE)
}

setMethod("[", c("ShojiCatalog", "character"), function (x, i, ..., drop) {
    w <- match(i, names(x@index))
    if (any(is.na(w))) {
        halt("Undefined elements selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, value)
})
setMethod("[", c("ShojiCatalog", "numeric"), function (x, i, ..., drop) {
    bad <- abs(as.integer(i)) > length(x)
    if (any(bad)) {
        halt("Subscript out of bounds: ", i[bad])
    }
    callNextMethod(x, i, value)
})
setMethod("[", c("ShojiCatalog", "logical"), function (x, i, ..., drop) {
    if (length(i) > length(x)) {
        halt("Subscript out of bounds: got ", length(i), " logicals, need ",
            length(x))
    }
    x@index <- x@index[i]
    return(x)
})
setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ..., drop) {
    x@index <- x@index[i]
    return(x)
})
setMethod("[[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    x@index[[i]]
})
setMethod("length", "ShojiCatalog", function (x) length(x@index))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(X@index, FUN, ...))

urls <- function (x) {
    names(x@index)
}

##' @export
as.list.ShojiCatalog <- function (x, ...) lapply(names(x@index), function (i) x[[i]])