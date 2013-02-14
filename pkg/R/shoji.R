is.shoji.like <- function (x) {
    is.list(x) && all(c("element", "self", "description") %in% names(x))
}

##' @S3method is shoji
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
    return(as(GET(x@self), Class))
}

setGeneric("refresh", function (x) standardGeneric("refresh"))
setMethod("refresh", "ShojiObject", .cr.shoji.refresh)


