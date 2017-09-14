init.sortCatalog <- function (.Object, ...) {
    ## Sort a ShojiCatalog by the object's "names" (as defined by its names method)
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(names(.Object))]
    return(.Object)
}

setMethod("initialize", "DatasetCatalog", init.sortCatalog)

setMethod("active", "DatasetCatalog", function (x) {
    index(x) <- Filter(function (a) !isTRUE(a$archived), index(x))
    return(x)
})

setMethod("archived", "DatasetCatalog", function (x) {
    index(x) <- Filter(function (a) isTRUE(a$archived), index(x))
    return(x)
})

#' See who owns these datasets
#'
#' @param x DatasetCatalog
#' @return For `owners`, the URLs of the users or projects that own
#' these datasets. For `ownerNames`, their names.
#' @export
owners <- function (x) {
    getIndexSlot(x, "owner_id")
}

#' @rdname owners
#' @export
ownerNames <- function (x) {
    getIndexSlot(x, "owner_display_name")
}

#' @rdname archive-and-publish
#' @export
setMethod("is.archived", "DatasetCatalog",
    function (x) getIndexSlot(x, "archived", logical(1)))
#' @rdname archive-and-publish
#' @export
setMethod("is.draft", "DatasetCatalog", function (x) !is.published(x))
#' @rdname archive-and-publish
#' @export
setMethod("is.published", "DatasetCatalog",
    function (x) getIndexSlot(x, "is_published", logical(1), ifnot=TRUE))

#' @rdname archive-and-publish
#' @export
setMethod("is.archived<-", c("DatasetCatalog", "logical"), function (x, value) {
    setIndexSlot(x, "archived", value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.draft<-", c("DatasetCatalog", "logical"), function (x, value) {
    setIndexSlot(x, "is_published", !value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.published<-", c("DatasetCatalog", "logical"), function (x, value) {
    setIndexSlot(x, "is_published", value)
})

#' Extract and modify subsets of Catalog-type objects
#'
#' @param x a Catalog object
#' @param i which catalog elements to extract
#' @param name for `$`, the same as `i` for `[[`
#' @param j Invalid
#' @param drop Invalid
#' @param ... additional arguments
#' @param value For updating, an object of the appropriate class and size to
#' insert
#' @return A subset of `x` if extracting, otherwise `x` duly modified
#' @name catalog-extract
NULL

#' @rdname catalog-extract
#' @export
setMethod("[[", c("DatasetCatalog", "numeric"), function (x, i, ...) {
    getTuple(x, i, DatasetTuple)
})

#' @rdname catalog-extract
#' @export
setMethod("[[<-", c("DatasetCatalog", "character", "missing", "DatasetTuple"),
    function (x, i, j, value) {
        index(x)[[i]] <- value@body
        return(x)
    })

#' Get and set names, aliases on Catalog-type objects
#'
#' These methods let you get and set names and aliases for variables in a
#' Dataset's catalog, or within [`Subvariables`] in an array
#' variable. They work like the base R names methods.
#'
#' Note that the Dataset `names` method returns the aliases of its
#' variables by default. This is controlled by
#' `getOption("crunch.namekey.dataset")`, which is "alias" by default.
#' Set `options(crunch.namekey.dataset="name")` if you wish to use
#' variable names. See the variables vignette for more information.
#'
#' @param x a VariableCatalog, Subvariables, or similar object
#' @param value For the setters, an appropriate-length character vector to
#' assign
#' @return Getters return the character object in the specified slot; setters
#' return `x` duly modified.
#' @aliases describe-catalog aliases aliases<- descriptions descriptions<- types emails timestamps
#' @seealso [`Subvariables`] [`Categories`] [`base::names`] `vignette("variables", package="crunch")`
#' @name describe-catalog
NULL
