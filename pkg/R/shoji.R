init.Shoji <- function (.Object, ...) {
    slots <- slotNames(.Object)
    dots <- list(...)
    if (length(dots) && is.shojiObject(dots[[1]])) {
        for (i in slots) slot(.Object, i) <- slot(dots[[1]], i)
    } else {
        for (i in slots) if (!is.null(dots[[i]])) slot(.Object, i) <- dots[[i]]
    }
    return(.Object)
}
setMethod("initialize", "ShojiObject", init.Shoji)

is.shoji.like <- function (x) {
    is.list(x) && "element" %in% names(x) && substr(as.character(x$element), 1, 5) == "shoji"
}

##' @S3method is shoji
##' @importFrom methods is
is.shoji <- function (x) inherits(x, "shoji")

setOldClass("shoji")
    
setAs("shoji", "ShojiObject", function (from) do.call("ShojiObject", from))
as.shojiObject <- function (x) as(x, "ShojiObject")

is.shojiObject <- function (x) inherits(x, "ShojiObject")

## 'refresh' method that GETs self url, and does new(Class, ...)
.cr.shoji.refresh <- function (x) {
    Class <- class(x)  ## in case x is a subclass of ShojiObject
    return(as(GET(self(x)), Class))
}

##' @export
setMethod("self", "ShojiObject", function (x) x@self)

##' @export
setMethod("refresh", "ShojiObject", .cr.shoji.refresh)
setMethod("refresh", "CrunchDataset", function (x) {
    ua <- x@useAlias
    out <- callNextMethod()
    out@useAlias <- ua
    return(out)
})

##' @export
setMethod("delete", "ShojiObject", function (x) invisible(DELETE(self(x))))
##' @export
setMethod("delete", "CrunchDataset", function (x) {
    out <- callNextMethod()
    updateDatasetList()
    invisible(out)
})
##' @export
setMethod("delete", "ANY", function (x) stop("'delete' only valid for Crunch objects"))

##' Base setter for Crunch objects
##' @param x a ShojiObject or subclass thereof
##' @param i character the slot name to update
##' @param value whatever the new value of that slot should be
##' @return x modified accordingly. If \code{x} isn't read-only, it will also
##' post the edit to the Crunch server.
setCrunchSlot <- function (x, i, value) {
    slot(x, "body")[[i]] <- value
    if (!is.readonly(x)) {
        body <- structure(list(value), .Names=i)
        payload <- toJSON(body)
        PUT(self(x), body=payload)
    }
    return(x)
}

is.readonly <- function (x) isTRUE(x@readonly) && !is.null(self(x))
setReadonly <- function (x, value) {
    x@readonly <- as.logical(value)
    x
}
##' @export
setMethod("readonly<-", "ShojiObject", setReadonly)

