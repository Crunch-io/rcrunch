init.Shoji <- function (.Object, ...) {
    slots <- slotNames(.Object)
    dots <- list(...)
    ## Different cases are so you can call the class constructor directly 
    ## with different inputs
    if (length(dots) && is.shojiObject(dots[[1]])) {
        ## Init from a parent class, e.g. CrunchObject(ShojiObject(x))
        slots <- intersect(slots, slotNames(dots[[1]]))
        for (i in slots) {
            slot(.Object, i) <- slot(dots[[1]], i)
        }
    } else if (length(dots) && is.shoji(dots[[1]])) {
        ## Init straight from API response, e.g. CrunchObject(GET(x))
        .Object <- do.call("init.Shoji", c(.Object=.Object, dots[[1]], ...))
    } else {
        ## Init from kwargs, e.g. CrunchObject(body=list, urls=list())
        ## Should this be open for all cases? I.e. init with a ShojiObject and
        ## ... args?
        for (i in slots) {
            if (!is.null(dots[[i]])) {
                slot(.Object, i) <- dots[[i]]
            }
        }
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

setAs("shoji", "ShojiObject", function (from) {
    cl <- ifelse(from$element == "shoji:catalog", "ShojiCatalog", "ShojiObject")
    return(do.call(cl, from))
})
as.shojiObject <- function (x) as(x, "ShojiObject")

is.shojiObject <- function (x) inherits(x, "ShojiObject")

##' @export
setMethod("self", "ShojiObject", function (x) x@self)

##' @export
setMethod("refresh", "ShojiObject", function (x) {
    Class <- class(x)  ## in case x is a subclass of ShojiObject
    return(do.call(Class, GET(self(x))))
})

##' @export
setMethod("delete", "ShojiObject", function (x) invisible(DELETE(self(x))))
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
        PATCH(self(x), body=payload)
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
