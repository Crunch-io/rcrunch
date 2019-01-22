setMethod("initialize", "MultitableCatalog", init.sortCatalog)

#' Multitable entities for a dataset
#'
#' @param x a CrunchDataset
#' @param value for the assignment method, a MultitableCatalog
#' @return an object of class MultitableCatalog containing references to
#' Multitable entities. (Setter returns the Dataset.)
#' @name multitable-catalog
#' @aliases multitables multitables<-
NULL

#' @rdname multitable-catalog
#' @export
setMethod("multitables", "CrunchDataset", function(x) {
    MultitableCatalog(crGET(shojiURL(x, "catalogs", "multitables")))
})

#' @rdname multitable-catalog
#' @export
setMethod("multitables<-", "CrunchDataset", function(x, value) x)

#' @rdname catalog-extract
#' @export
setMethod("[[", c("MultitableCatalog", "numeric"), function(x, i, ...) {
    getEntity(x, i, Multitable, ...)
})

#' @rdname catalog-extract
#' @export
setMethod(
    "[[<-", c("MultitableCatalog", "character", "missing", "formula"),
    function(x, i, j, value) {
        stopifnot(length(i) == 1)
        w <- match(i, names(x))
        ## Creating a new filter if there are no matching names
        if (is.na(w)) {
            f <- newMultitable(
                formula = value,
                data = loadDataset(datasetReference(x)),
                name = i
            )
            return(refresh(x))
        }
        callNextMethod(x, w, value = value)
    }
)

#' @rdname catalog-extract
#' @export
setMethod(
    "[[<-", c("MultitableCatalog", "numeric", "missing", "formula"),
    function(x, i, j, value) {
        stopifnot(length(i) == 1)

        ds <- loadDataset(datasetReference(x))
        template <- formulaToQuery(value, data = ds)

        if (i %in% seq_along(urls(x))) {
            payload <- makeMultitablePayload(template = template)
            crPATCH(urls(x)[i], body = toJSON(payload))
            return(x)
        } else {
            halt("subscript out of bounds: ", i)
        }
    }
)

makeMultitablePayload <- function(template, ...) {
    template <- lapply(template$dimensions, function(x) list(query = x))

    return(wrapEntity(template = template, ...))
}

#' @rdname catalog-extract
#' @export
setMethod(
    "[[<-", c("MultitableCatalog", "ANY", "missing", "NULL"),
    function(x, i, j, value) {
        stopifnot(length(i) == 1)
        if (is.character(i) && !i %in% names(x)) {
            return()
        } else if (is.numeric(i) && !i %in% seq_along(urls(x))) {
            return()
        }
        delete(x[[i]])
        invisible(NULL)
    }
)

#' @rdname describe
#' @export
setMethod("name<-", "Multitable", function(x, value) {
    setEntitySlot(x, "name", value)
})

#' @export
#' @rdname describe-catalog
setMethod("names<-", "MultitableCatalog", function(x, value) {
    setIndexSlotOnEntity(x, "name", value)
})

#' @rdname is-public
#' @export
setMethod("is.public", "MultitableCatalog", function(x) {
    getIndexSlot(x, "is_public", what = logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public<-", "MultitableCatalog", function(x, value) {
    setIndexSlotOnEntity(x, "is_public", value, what = logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public", "Multitable", function(x) x@body$is_public)

#' @rdname is-public
#' @export
setMethod("is.public<-", "Multitable", function(x, value) {
    stopifnot(is.TRUEorFALSE(value))
    setEntitySlot(x, "is_public", value)
})

#' @rdname dataset-reference
setMethod("datasetReference", "Multitable", function(x) {
    datasetReference(self(x))
})

#' @rdname dataset-reference
setMethod("datasetReference", "MultitableCatalog", function(x) {
    datasetReference(self(x))
})

#' Create a new Multitable
#'
#' Multitables, or "banners" or "crossbreaks", define a set of variables or
#' or query expressions to crosstab with as a unit. They are used in the Crunch
#' web app to display tables side by side, as well as to define one dimension
#' of a tab book.
#' @param formula an object of class 'formula' object with the
#' cross-classifying variables separated by '+' on the right-hand side.
#' Following how [stats::formula()] works in R, it should start
#' with "~". Variables on left-hand side of the formula have no meaning in this
#' function.
#' @param data an object of class `CrunchDataset` in which to create the
#' multitable, and to which the variables referenced in `formula` belong.
#' @param name character name to give the new multitable object. If omitted,
#' a default name will be derived from `formula`.
#' @param ... Additional multitable attributes to set. Options include
#' `is_public`.
#' @return An object of class `Multitable`
#' @seealso [`stats::formula`]
#' @examples
#' \dontrun{
#' m <- newMultitable(~ gender + age4 + marstat, data=ds)
#' name(m) # [1] "gender + age4 + marstat"
#' }
#' @export
newMultitable <- function(formula, data, name, ...) {
    ## Validate inputs
    if (missing(formula)) {
        halt("Must provide a formula")
    }
    if (missing(data) || !is.dataset(data)) {
        halt(dQuote("data"), " must be a Dataset")
    }

    template <- formulaToQuery(formula, data)
    if (missing(name)) {
        name <- RHS_string(formula)
    }

    payload <- makeMultitablePayload(template = template, name = name)
    u <- crPOST(shojiURL(data, "catalogs", "multitables"),
        body = toJSON(payload)
    )

    invisible(Multitable(crGET(u)))
}

#' Import a Multitable
#'
#' Allows you to import a multitable from a different dataset
#' @param data a `CrunchDataset` in which to create the multitable
#' @param multitable an object of class `Multitable` that you want copied
#' to the new dataset
#' @param ... Additional multitable attributes to set. Options include `name` and
#' `is_public`.
#' @return An object of class `Multitable`
#' @examples
#' \dontrun{
#' m <- newMultitable(~ gender + age4 + marstat, data=ds)
#' copied_m <- importMultitable(another_ds, m)
#' name(copied_m) # [1] "gender + age4 + marstat"
#' }
#' @export
importMultitable <- function(data, multitable, ...) {
    payload <- wrapEntity(multitable = self(multitable), ...)

    u <- crPOST(shojiURL(data, "catalogs", "multitables"), body = toJSON(payload))
    invisible(Multitable(crGET(u)))
}
