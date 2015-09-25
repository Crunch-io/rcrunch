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


setMethod("appliedFilters", "CrunchDataset", function (x) {
    ShojiView(crGET(shojiURL(x, "views", "applied_filters")))@value$groups[[1]]$entities
})

setMethod("appliedFilters<-", c("CrunchDataset", "CrunchFilter"),
    function (x, value) {
        b <- toJSON(list(
            groups=I(list(
                list(
                    group="default",
                    entities=I(list(self(value)))
                )
            ))
        ))
        crPUT(shojiURL(x, "views", "applied_filters"), body=b)
        return(x)
    })

setMethod("activeFilter", "CrunchDataset", function (x) x@filter)

setMethod("activeFilter<-", c("CrunchDataset", "CrunchLogicalExpr"), 
    function (x, value) {
        x@filter <- value
        return(x)
    })

setMethod("activeFilter", "CrunchVariable", function (x) x@filter)

setMethod("activeFilter<-", c("CrunchVariable", "CrunchLogicalExpr"), 
    function (x, value) {
        x@filter <- value
        return(x)
    })

setMethod("activeFilter", "Subvariables", function (x) x@filter)

setMethod("activeFilter<-", c("Subvariables", "CrunchLogicalExpr"), 
    function (x, value) {
        x@filter <- value
        return(x)
    })

setMethod("activeFilter<-", c("ANY", "NULL"), 
    function (x, value) {
        ## Backstop method for refreshing a variable not extracted from a dataset. Variable may have NULL filter because object can't require CrunchLogicalExpr due to cyclic dependencies.
        activeFilter(x) <- CrunchLogicalExpr()
        return(x)
    })

filterSyntax <- function (x) {
    ## Wrapper to contain API complexity when sending filter_syntax as query parameter
    
    f <- zcl(x)
    if (!length(f)) {
        ## No filter in the R session. So supply an "all" filter to override any
        ## "applied filter" on the server
        ## TODO: make backend take a proper null
        f <- zfunc("not", zfunc("==", zfunc("row"), -1))
    }
    ## Wrap and return
    ## TODO: shouldn't have to wrap in expression object and supply id
    return(list(expression=f, id="dont_require_id"))
}

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
    if (length(ef$body)) {
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
        crPUT(shojiURL(x, "fragment", "exclusion"),
            body=toJSON(list(name="exclusion", expression=zcl(value))))
        }
    else if (is.null(value)) {
        crDELETE(shojiURL(x, "fragment", "exclusion"))
    } else {
        halt(dQuote("value"), " must be a CrunchLogicalExpr or NULL, not ",
            dQuote(class(value)))
    }
    dropCache(self(x))
    return(x)
}