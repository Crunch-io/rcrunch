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
setMethod("multitables", "CrunchDataset", function (x) {
    MultitableCatalog(crGET(shojiURL(x, "catalogs", "multitables")))
})

#' @rdname multitable-catalog
#' @export
setMethod("multitables<-", "CrunchDataset", function (x, value) x)

#' @rdname catalog-extract
#' @export
setMethod("[[", c("MultitableCatalog", "numeric"), function (x, i, ...) {
    getEntity(x, i, Multitable, ...)
})

#' @rdname describe
#' @export
setMethod("name<-", "Multitable", function (x, value) {
    setEntitySlot(x, "name", value)
})

#' @export
#' @rdname describe-catalog
setMethod("names<-", "MultitableCatalog", function (x, value) {
    setIndexSlotOnEntity(x, "name", value)
})

#' @rdname is-public
#' @export
setMethod("is.public", "MultitableCatalog", function (x) {
    getIndexSlot(x, "is_public", what=logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public<-", "MultitableCatalog", function (x, value) {
    setIndexSlotOnEntity(x, "is_public", value, what=logical(1))
})

#' @rdname is-public
#' @export
setMethod("is.public", "Multitable", function (x) x@body$is_public)

#' @rdname is-public
#' @export
setMethod("is.public<-", "Multitable", function (x, value) {
    setEntitySlot(x, "is_public", value)
})

#' Create a new Multitable
#'
#' Multitables, or "banners" or "crossbreaks", define a set of variables or
#' or query expressions to crosstab with as a unit. They are used in the Crunch
#' web app to display tables side by side, as well as to define one dimension
#' of a tab book.
#' @param formula an object of class 'formula' object with the
#' cross-classifying variables separated by '+' on the right-hand side.
#' Following how \code{\link[stats]{formula}} works in R, it should start
#' with "~". Variables on left-hand side of the formula have no meaning in this
#' function.
#' @param data an object of class \code{CrunchDataset} in which to create the
#' multitable, and to which the variables referenced in \code{formula} belong.
#' @param name character name to give the new multitable object. If omitted,
#' a default name will be derived from \code{formula}.
#' @param ... Additional multitable attributes to set. Options include
#' \code{is_public}.
#' @return An object of class \code{Multitable}
#' @seealso \code{\link[stats]{formula}}
#' @examples
#' \dontrun{
#' m <- newMultitable(~ gender + age4 + marstat, data=ds)
#' name(m) # [1] "gender + age4 + marstat"
#' }
#' @export
newMultitable <- function (formula, data, name, ...) {
    ## Validate inputs
    if (missing(formula)) {
        halt("Must provide a formula")
    }
    if (missing(data) || !is.dataset(data)) {
        halt(dQuote("data"), " must be a Dataset")
    }

    template <- formulaToQuery(formula, data)
    if (missing(name)) {
        name <- formulaRHS(formula)
    }

    payload <- list(
        element="shoji:entity",
        body=list(
            name=name,
            template=lapply(template$dimensions,
                function (x) list(query=x, variable=findVariableReferences(x)))
        )
    )
    u <- crPOST(shojiURL(data, "catalogs", "multitables"), body=toJSON(payload))
    invisible(Multitable(crGET(u)))
}

tabBook <- function (multitable, dataset, weight=crunch::weight(dataset),
                    format=c("json", "xlsx"), filename, ...) {

    f <- match.arg(format)
    accept <- list(
        json="application/json",
        xlsx="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )[[f]]
    if (missing(filename)) {
        filename <- paste(name(multitable), f, sep=".")
    }

    if (!is.null(weight)) {
        weight <- self(weight)
    }
    body <- list(
        filter=zcl(activeFilter(dataset)),
        weight=weight,
        options=list(...)
    )
    ## Add this after so that if it is NULL, the "where" key isn't present
    body$where <- variablesFilter(dataset)

    tabbook_url <- shojiURL(multitable, "views", "tabbook")
    result <- crPOST(tabbook_url, config=add_headers(`Accept`=accept),
        body=toJSON(body))
    download.file(result, filename, quiet=TRUE, method="curl") ## Note outside of auth. Ok because file is in s3 with token
    invisible(filename)
}
