init.Shoji <- function(.Object, ...) {
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
        ## Init straight from API response, e.g. CrunchObject(crGET(x))
        .Object <- do.call("init.Shoji", c(.Object = .Object, dots[[1]], ...))
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
    is.list(x) && "element" %in% names(x) && startsWith(as.character(x$element), "shoji")
}

#' @rdname crunch-is
#' @export
#' @importFrom methods is
is.shoji <- function(x) inherits(x, "shoji")

is.shojiObject <- function(x) inherits(x, "ShojiObject")

#' Get the URL of this object
#' @param x a Crunch object
#' @return the URL for \code{x}
#' @aliases self
#' @name self
NULL

#' @rdname self
#' @export
setMethod("self", "ShojiObject", function(x) x@self)

#' @rdname describe-entity
#' @export
setMethod("name", "ShojiObject", function(x) x@body$name)

#' @rdname refresh
#' @export
setMethod("refresh", "ShojiObject", function(x) {
    dropCache(self(x))
    Class <- class(x) ## in case x is a subclass of ShojiObject
    return(do.call(Class, crGET(self(x))))
})

#' Base setter for Crunch objects
#' @param x a ShojiObject or subclass thereof
#' @param i character the slot name to update
#' @param value whatever the new value of that slot should be
#' @return x modified accordingly.
#' @keywords internal
setEntitySlot <- function(x, i, value) {
    ## Check if we have actual changes to send. Wrap both sides in I()
    ## in case "value" is already wrapped
    if (!identical(I(slot(x, "body")[[i]]), I(value))) {
        slot(x, "body")[[i]] <- value
        body <- structure(list(value), .Names = i)
        payload <- toJSON(body)
        crPATCH(self(x), body = payload)
        if (is.dataset(x)) {
            # Update the tuple in place too
            # This is hacky; we should probably make datasets not involve tuples
            if (i %in% names(tuple(x)@body)) {
                tuple(x)[[i]] <- value
            }
            # Also drop cache for the dataset's containing project index
            dropOnly(shojiURL(x, "catalogs", "project"))
        }
    }
    return(x)
}

#' Get a resource URL from a Shoji Object
#'
#' @param x a shojiObject
#' @param collection one of c("catalogs", "views", "fragments", "orders")
#' @param key character name of the URL to get from `collection`
#' @param mustWork logical: error if the URL is not found? Default is `TRUE`
#' @return The indicated URL, or if it does not exist and `mustWork` is not
#' `TRUE`, `NULL`.
#' @export
#' @keywords internal
#' @importFrom httpcache logMessage
shojiURL <- function(x,
                     collection = c("catalogs", "views", "fragments", "orders"),
                     key,
                     mustWork=TRUE) {

    if (is.variable(x) || inherits(x, "ShojiTuple")) {
        x <- entity(x) ## Get the *Entity (e.g. VariableEntity)
        logMessage("INFO", "GET entity in shojiURL")
    }
    if (!is.shojiObject(x)) {
        halt("Cannot get Shoji URL from object of class ", dQuote(class(x)))
    }
    collection <- match.arg(collection)
    urls <- slot(x, collection)
    out <- urls[[key]]
    if (is.null(out) && mustWork) {
        halt("No URL ", dQuote(key), " in collection ", dQuote(collection))
    }
    return(out)
}

wrapEntity <- function(..., body = list(...)) {
    list(element = "shoji:entity", body = body)
}

wrapCatalog <- function(...) list(element = "shoji:catalog", ...)

wrapCatalogIndex <- function(...) wrapCatalog(index = as.list(...))
