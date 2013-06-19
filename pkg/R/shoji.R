is.shoji.like <- function (x) {
    is.list(x) && all(c("element", "self", "description") %in% names(x))
}

##' @S3method is shoji
##' @importFrom methods is
is.shoji <- function (x) inherits(x, "shoji")

setOldClass("shoji")

##' Given a collections URL, get the entity URLs
getShojiCollectionURLs <- function (x) {
    GET(x)$entities
}

##' Given a set of entity URLs, get their content
getShojiCollectionContents <- function (entities, namekey=NULL) {
    out <- lapply(entities, GET)
    if (!is.null(namekey)) names(out) <- selectFrom(namekey, out)
    return(out)
}

##' Get all the entities from a collection
getShojiCollection <- function (x, namekey=NULL) {
    getShojiCollectionContents(getShojiCollectionURLs(x), namekey=namekey)
}
    
setAs("shoji", "ShojiObject", function (from) do.call("ShojiObject", from))
as.shojiObject <- function (x) as(x, "ShojiObject")

is.shojiObject <- function (x) inherits(x, "ShojiObject")

## 'refresh' method that GETs self url, and does new(Class, ...)
.cr.shoji.refresh <- function (x) {
    Class <- class(x)  ## in case x is a subclass of ShojiObject
    return(as(GET(self(x)), Class))
}

setGeneric("self", function (x) standardGeneric("self"))
setMethod("self", "ShojiObject", function (x) x@self)

setGeneric("refresh", function (x) standardGeneric("refresh"))
setMethod("refresh", "ShojiObject", .cr.shoji.refresh)

##' Base setter for Crunch objects
##' @param x a ShojiObject or subclass thereof
##' @param i character the slot name to update
##' @param value whatever the new value of that slot should be
##' @return x modified accordingly. If \code{x} isn't read-only, it will also
##' post the edit to the Crunch server.
setCrunchSlot <- function (x, i, value) {
    slot(x, "body")[[i]] <- value
    if (!is.readonly(x)) {
        PUT(self(x), body=toJSON(structure(value, .Names=i)))
    }
    return(x)
}

attributeURL <- function (x, attribute) {
    url <- self(x)
    if (substr(url, nchar(url), nchar(url)) != "/") url <- paste0(url, "/")
    return(paste0(url, attribute))
}

is.readonly <- function (x) isTRUE(x@readonly) && !is.null(self(x))
setReadonly <- function (x, value) {
    x@readonly <- as.logical(value)
    x
}
setGeneric("readonly<-", function (x, value) standardGeneric("readonly<-"))
setMethod("readonly<-", "ShojiObject", setReadonly)

