#' Get or set a dataset's filters
#'
#' You can build and save filters in the Crunch web app, and these filters
#' are stored in a `FilterCatalog`. This function allows you to retrieve
#' and modify those filters.
#'
#' @param x a CrunchDataset
#' @param value for the setter, a FilterCatalog
#' @return an object of class FilterCatalog containing references to Filter
#' entities usable in the web application. (Setter returns the Dataset.)
#' @name filter-catalog
#' @aliases filters filters<-
NULL

#' @rdname filter-catalog
#' @export
setMethod("filters", "CrunchDataset", function (x) {
    FilterCatalog(crGET(shojiURL(x, "catalogs", "filters")))
})

#' @rdname filter-catalog
#' @export
setMethod("filters<-", "CrunchDataset", function (x, value) x)

#' View and modify "public" attribute
#'
#' View and modify whether all dataset viewers have access to the dataset. This
#' will return `FALSE` if the dataset is in draft.
#'
#' @param x a Crunch object
#' @param value an attribute to set
#' @return For `is.public`, a logical value for whether the object is
#' flagged as shared with all dataset viewers. (Its setter thus takes a
#' logical value as well.) Catalogs of datasets return a vector of logicals
#' corresponding to the length of the catalog, while entities return a single value.
#' @name is-public
#' @aliases is.public<- is.public
NULL

#' @rdname is-public
#' @export
setMethod("is.public", "CrunchFilter", function (x) x@body$is_public)

#' @rdname is-public
#' @export
setMethod("is.public<-", "CrunchFilter", function (x, value) {
    setEntitySlot(x, "is_public", value)
})

#' @rdname catalog-extract
#' @export
setMethod("[[", c("FilterCatalog", "numeric"), function (x, i, ...) {
    getEntity(x, i, CrunchFilter, ...)
})

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("FilterCatalog", "character", "missing", "CrunchLogicalExpr"),
    function (x, i, j, value) {
        stopifnot(length(i) == 1)
        if (i %in% names(x)) {
            crPATCH(urls(x)[match(i, names(x))],
                body=toJSON(list(expression=zcl(value))))
            ## Editing expression doesn't require invalidating the catalog
            return(x)
        } else {
            ## Creating a new filter
            f <- .newFilter(i, value, catalog_url=self(x))
            return(refresh(x))
        }
    })

#' Create a new filter
#'
#' This function creates a new filter for a CrunchDataset. You can achieve the same results
#' by assigning into a dataset's filters catalog using[filters()], but this may be a more natural
#' way to think of the action, particularly when you want to do something with
#' the filter entity after you create it.
#' @param name character name for the filter
#' @param expression CrunchLogicalExpr with which to make a filter entity
#' @param catalog FilterCatalog in which to create the new filter. May also
#' provide a dataset entity. If omitted, the function will attempt to infer the
#' dataset (and thus its FilterCatalog) from the contents of `expression`.
#' @param ... Additional filter attributes to set for instance `is_public`.
#' @return A `CrunchFilter` object.
#' @export
newFilter <- function (name, expression, catalog=NULL, ...) {
    if (is.null(catalog)) {
        obj <- ShojiEntity(crGET(datasetReference(expression)))
        catalog_url <- shojiURL(obj, "catalogs", "filters")
    } else if (is.dataset(catalog)) {
        catalog_url <- shojiURL(catalog, "catalogs", "filters")
    } else if (inherits(catalog, "FilterCatalog")) {
        catalog_url <- self(catalog)
    } else {
        halt("Cannot create a filter entity on an object of class ",
            class(catalog))
    }
    u <- .newFilter(name, expression, catalog_url, ...)
    invisible(CrunchFilter(crGET(u)))
}

## Internal function to do the POSTing, both in [[ and in newFilter
.newFilter <- function (name, expression, catalog_url, ...) {
    crPOST(catalog_url, body=toJSON(list(name=name,
        expression=zcl(expression), ...)))
}

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("FilterCatalog", "numeric", "missing", "CrunchLogicalExpr"),
    function (x, i, j, value) {
        stopifnot(length(i) == 1)
        if (i %in% seq_along(urls(x))) {
            crPATCH(urls(x)[i],
                body=toJSON(list(expression=zcl(value))))
            ## Editing expression doesn't require invalidating the catalog
            return(x)
        } else {
            halt("Subscript out of bounds: ", i)
        }
    })

#' @rdname catalog-extract
#' @export
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

#' @rdname catalog-extract
#' @export
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
    } else {
        ## Pretend that the filter of a CrunchExpr can be CrunchLogicalExpr
        f <- CrunchLogicalExpr(expression=expr,
            dataset_url=datasetReference(x) %||% "")
    }
    if (!length(expr)) {
        ## No active filter. Return NULL
        f <- NULL
    }
    return(f)
}

.setActiveFilter <- function (x, value) {
    if (!inherits(value, "CrunchLogicalExpr")) {
        value <- CrunchLogicalExpr(expression=value %||% list(),
            dataset_url=datasetReference(x) %||% "")
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
setMethod("activeFilter<-", "CrunchExpr", function (x, value) {
    ## CrunchExpr @filter can't be CrunchLogicalExpr bc of cyclical deps
    ## so it's the @expression of that (list)
    if (is.null(value)) {
        value <- list()
    } else if (inherits(value, "CrunchLogicalExpr")) {
        value <- value@expression
    }
    x@filter <- value
    return(x)
})

setMethod("activeFilter", "list", function (x) NULL)


## See dataset-extract.R for .updateActiveFilter

#' View and set exclusion filters
#'
#' Exclusion filters express logic that defines a set of rows that should be
#' dropped from the dataset. The rows aren't permanently deleted---you can
#' recover them at any time by removing the exclusion filter---but they are
#' omitted from all views and calculations, as if they had been deleted.
#'
#' Note that exclusion filters work opposite from how "normal" filters work.
#' That is, a regular filter expression defines the subset of rows to operate
#' on: it says "keep these rows." An exclusion filter defines which rows to
#' omit. Applying a filter expression as a query filter will have the
#' opposite effect if applied as an exclusion. Indeed, applying it as both
#' query filter and exclusion at the same time will result in 0 rows.
#'
#' @param x a Dataset
#' @param value an object of class \code{CrunchLogicalExpr}, or \code{NULL}
#' @return \code{exclusion} returns a \code{CrunchFilter} if there is one,
#' else \code{NULL}. The setter returns \code{x} with the filter set.
#' @export
exclusion <- function (x) {
    stopifnot(is.dataset(x))
    ef <- crGET(shojiURL(x, "fragment", "exclusion"))
    e <- ef$body$expression
    if (length(e)) {
        ## We have a non-empty filter
        ## Server is returning variable IDs. Make them into URLs
        ## TODO: remove this
        e <- idsToURLs(e, variableCatalogURL(x))
        return(CrunchLogicalExpr(expression=e))
    } else {
        return(NULL)
    }
}

idsToURLs <- function (expr, base_url) {
    ## Recurse, looking for every variable: id and make it variable: url
    if (is.list(expr)) {
        if (length(expr) == 1 &&
            identical(names(expr), "variable") &&
            !endsWith(expr[["variable"]], "/")) {
            ## This is a variable ref that is an id. Absolutize.
            expr[["variable"]] <- absoluteURL(paste0("./", expr, "/"),
                base_url)
            return(expr)
        } else {
            ## Recurse.
            return(lapply(expr, idsToURLs, base_url))
        }
    } else {
        return(expr)
    }
}

#' @rdname exclusion
#' @export
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

setMethod("expr", "CrunchFilter",
    function (x) {
        ## TODO: remove this when server sends URLs instead of ids
        e <- idsToURLs(x@body$expression,
            absoluteURL("../../variables/", self(x)))
        if (length(e)) {
            return(CrunchLogicalExpr(expression=e))
        } else {
            return(NULL)
        }
    })
