##' Filter entities for a dataset
##'
##' @param x a CrunchDataset
##' @return an object of class FilterCatalog containing references to Filter
##' entities usable in the web application.
##' @export
setMethod("filters", "CrunchDataset", function (x) {
    FilterCatalog(crGET(shojiURL(x, "catalogs", "filters")))
})

setMethod("filters<-", "CrunchDataset", function (x, value) x)

##' @rdname describe-catalog
##' @export
setMethod("names", "FilterCatalog", function (x) getIndexSlot(x, "name"))

##' @rdname describe
##' @export
setMethod("name", "CrunchFilter", function (x) x@body$name)

##' View and modify Filter entity attributes
##'
##' @param x a CrunchFilter
##' @param value an attribute to set
##' @return For \code{is.public}, a logical value for whether the filter is
##' flagged as shared with all dataset viewers. (Its setter thus takes a
##' logical value as well.)
##' @name filter-methods
##' @export
setMethod("is.public", "CrunchFilter", function (x) x@body$is_public)

##' @rdname filter-methods
##' @export
setMethod("is.public<-", "CrunchFilter", function (x, value) {
    setEntitySlot(x, "is_public", value)
})


##' @rdname catalog-extract
##' @export
setMethod("[[", c("FilterCatalog", "character"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    z <- match(i, names(x))
    if (is.na(z)) {
        return(NULL)
    }
    return(x[[z]])
})

##' @rdname catalog-extract
##' @export
setMethod("[[", c("FilterCatalog", "numeric"), function (x, i, ...) {
    stopifnot(length(i) == 1)
    url <- urls(x)[i]
    return(CrunchFilter(crGET(url)))
})

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("FilterCatalog", "character", "missing", "CrunchLogicalExpr"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## TODO: update filter with new expression
            halt("Cannot (yet) modify filter")
        } else {
            ## Creating a new filter
            u <- crPOST(self(x), body=toJSON(list(name=i,
                expression=zcl(value))))
            return(refresh(x))
        }
    })

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("FilterCatalog", "character", "missing", "CrunchFilter"),
    function (x, i, j, value) {
        if (i %in% names(x)) {
            ## Assume server update of the entity already happened in a
            ## separate request. So just refresh.
            return(refresh(x))
        } else {
            ## Unlikely to be here given current CrunchFilter implementation
            ## (only comes from extracting from catalog)
            halt("Unsupported")
        }
    })

##' @rdname catalog-extract
##' @export
setMethod("[[<-", c("FilterCatalog", "numeric", "missing", "CrunchFilter"),
    function (x, i, j, value) {
        if (i %in% seq_len(length(x))) {
            ## See above.
            ## Assume server update of the entity already happened in a
            ## separate request. So just refresh.
            return(refresh(x))
        } else {
            ## Unlikely to be here given current CrunchFilter implementation
            ## (only comes from extracting from catalog)
            halt("Unsupported")
        }
    })

setMethod("appliedFilters", "CrunchDataset", function (x) {
    out <- ShojiOrder(crGET(shojiURL(x, "views", "applied_filters")))
    return(out@graph)
})

setMethod("appliedFilters<-", c("CrunchDataset", "CrunchFilter"),
    function (x, value) {
        b <- toJSON(list(
            graph=I(list(self(value)))
        ))
        crPUT(shojiURL(x, "views", "applied_filters"), body=b)
        return(x)
    })

.getActiveFilter <- function (x) {
    f <- expr <- x@filter
    if (inherits(f, "CrunchLogicalExpr")) {
        ## To check for an empty filter expression, get the @expression
        ## Can't assume f is CrunchLogicalExpr because x could be CrunchExpr
        expr <- f@expression
    }
    if (!length(expr)) {
        ## No active filter. Return NULL
        f <- NULL
    }
    return(f)
}

.setActiveFilter <- function (x, value) {
    if (is.null(value)) {
        ## Set an empty CrunchLogicalExpr
        value <- CrunchLogicalExpr()
    }
    x@filter <- value
    return(x)
}

setMethod("activeFilter", "CrunchDataset", .getActiveFilter)
setMethod("activeFilter<-", "CrunchDataset", .setActiveFilter)

setMethod("activeFilter", "CrunchVariable", .getActiveFilter)
setMethod("activeFilter<-", "CrunchVariable", .setActiveFilter)

setMethod("activeFilter", "Subvariables", .getActiveFilter)
setMethod("activeFilter<-", "Subvariables", .setActiveFilter)

setMethod("activeFilter", "CrunchExpr", .getActiveFilter)


##' View and set exclusion filters
##'
##' Exclusion filters express logic that defines a set of rows that should be
##' dropped from the dataset. The rows aren't permanently deleted---you can
##' recover them at any time by removing the exclusion filter---but they are
##' omitted from all views and calculations, as if they had been deleted.
##'
##' Note that exclusion filters work opposite from how "normal" filters work.
##' That is, a regular filter expression defines the subset of rows to operate
##' on: it says "keep these rows." An exclusion filter defines which rows to
##' omit. Applying a filter expression as a query filter will have the
##' opposite effect if applied as an exclusion. Indeed, applying it as both
##' query filter and exclusion at the same time will result in 0 rows.
##'
##' @param x a Dataset
##' @param value an object of class \code{CrunchLogicalExpr}, or \code{NULL}
##' @return \code{exclusion} returns a \code{CrunchFilter} if there is one,
##' else \code{NULL}. The setter returns \code{x} with the filter set.
##' @export
exclusion <- function (x) {
    stopifnot(is.dataset(x))
    ef <- crGET(shojiURL(x, "fragment", "exclusion"))
    if (length(ef$body$expression)) {
        ## We have a non-empty filter
        return(CrunchLogicalExpr(expression=ef$body$expression))
    } else {
        return(NULL)
    }
}

##' @rdname exclusion
##' @export
`exclusion<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (inherits(value, "CrunchLogicalExpr")) {
        payload <- zcl(value)
    } else if (is.null(value)) {
        payload <- emptyObject()
    } else {
        halt(dQuote("value"), " must be a CrunchLogicalExpr or NULL, not ",
            dQuote(class(value)))
    }
    crPATCH(shojiURL(x, "fragment", "exclusion"),
        body=toJSON(list(expression=payload)))
    dropCache(self(x))
    return(x)
}
