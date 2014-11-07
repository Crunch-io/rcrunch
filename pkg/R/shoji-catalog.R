init.ShojiCatalog <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    names(.Object@index) <- absolutizeURLs(names(.Object@index), .Object@self)
    return(.Object)
}
setMethod("initialize", "ShojiCatalog", init.ShojiCatalog)

is.shojiCatalog <- function (x) inherits(x, "ShojiCatalog")

setIndexSlot <- function (x, i, value) {
    index(x) <- lapply(x, function (a) {
        a[[i]] <- value
        return(a)
    })
    crPATCH(self(x), body=toJSON(index(x)))
    return(x)
}

mapSetIndexSlot <- function (x, i, value) {
    if (length(value) == 1) value <- rep(value, length(x))
    stopifnot(length(x) == length(value))
    
    old <- index(x)
    index(x) <- mapply(function (a, v) {
        a[[i]] <- v
        return(a)
    }, a=index(x), v=value, SIMPLIFY=FALSE)
    to.update <- dirtyElements(old, index(x))
    if (any(to.update)) {
        crPATCH(self(x), body=toJSON(index(x)[to.update]))
    }
    return(x)
}

dirtyElements <- function (x, y) {
    !mapply(identical, x, y, USE.NAMES=FALSE, SIMPLIFY=TRUE)
}

setMethod("[", c("ShojiCatalog", "character"), function (x, i, ..., drop) {
    w <- match(i, urls(x))
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
    index(x) <- index(x)[i]
    return(x)
})
setMethod("[", c("ShojiCatalog", "ANY"), function (x, i, ..., drop) {
    index(x) <- index(x)[i]
    return(x)
})
setMethod("[[", c("ShojiCatalog", "ANY"), function (x, i, ...) {
    index(x)[[i]]
})
setMethod("length", "ShojiCatalog", function (x) length(index(x)))
setMethod("lapply", "ShojiCatalog", function (X, FUN, ...) lapply(index(X), FUN, ...))

##' @export
setMethod("index", "ShojiCatalog", function (x) x@index)
##' @export
setMethod("index<-", "ShojiCatalog", function (x, value) {
    x@index <- value
    return(x)
})

urls <- function (x) names(index(x))

##' @export
as.list.ShojiCatalog <- function (x, ...) lapply(names(index(x)), function (i) x[[i]])