#' Subvariables in Array Variables
#'
#' Multiple-response and categorical-array variables are higher order variables which
#' are made up of sets of subvariables. These methods allow you to retrieve and change
#' the subvariables of a multiple-response or categorical-array variable.
#'
#' Subvariables can be accessed from array variables (including multiple
#' response) with the `subvariables` method. They can be assigned back
#' with the `subvariables<-` setter, but there are limitations to what
#' is supported. Specifically, you can reorder subvariables, but you cannot
#' add or remove subvariables by `subvariables<-` assignment. See
#' [`deleteSubvariable`] to remove subvariables from an array.
#'
#' Subvariables have a `names` attribute that can be accessed, showing
#' the display names of the subvariables. These can be set with the
#' `names<-` method.
#'
#' Finally, subvariables can be accessed as regular (categorical) variables
#' with the `$` and `[[` extract methods.
#'
#' See the vignette on array variables for further details and examples.
#'
#' @param x A Variable or Subvariables object
#' @param value For the setters, the appropriate values to set
#'
#' @name Subvariables
#' @aliases Subvariables subvariables subvariables<-
#' @seealso [`describe-catalog`] [`deleteSubvariable`] `vignette("array-variables", package="crunch")`
setGeneric("subvariables", function(x) standardGeneric("subvariables"))

#' @rdname Subvariables
setGeneric(
    "subvariables<-",
    function(x, value) standardGeneric("subvariables<-")
)

#' @rdname Subvariables
#' @export
setMethod("subvariables", "CategoricalArrayVariable", function(x) {
    # TODO: it may be simpler and cleaner to just get the subvars from the
    # entity rather than the subvariables catalog.
    tup <- tuple(x)
    catalog_url <- absoluteURL(tup$subvariables_catalog, base = tup@index_url)
    if (!is.null(tup$subreferences)) {
        out <- subvariables(tup)
    } else {
        vars <- VariableCatalog(crGET(catalog_url))
        out <- Subvariables(vars[subvariableURLs(tup)])
    }
    activeFilter(out) <- activeFilter(x)
    return(out)
})

#' @rdname Subvariables
#' @export
setMethod("subvariables", "CrunchVariable", function(x) NULL)

subvariableURLs <- function(x) {
    return(absoluteURL(unlist(x$subvariables), base = x@index_url))
}

#' @rdname Subvariables
#' @export
setMethod("subvariables", "VariableTuple", function(x) {
    catalog_url <- absoluteURL(x$subvariables_catalog, base = x@index_url) %||% ""
    subvars <- x$subreferences

    # if there is a subvariable element, we need to use that for ordering
    names(subvars) <- paste0(absoluteURL(names(subvars), base = catalog_url), "/")

    return(Subvariables(index = subvars[x$subvariables], self = catalog_url))
})

#' @rdname Subvariables
#' @export
setMethod(
    "subvariables<-", c("CategoricalArrayVariable", "ANY"),
    function(x, value) {
        halt("Can only assign an object of class Subvariables")
    }
)
#' @rdname Subvariables
#' @export
setMethod(
    "subvariables<-", c("CategoricalArrayVariable", "Subvariables"),
    function(x, value) {
        old <- subvariableURLs(tuple(x))
        new <- urls(value)
        if (!setequal(old, new)) {
            halt("Can only reorder, not change, subvariables")
        }
        new <- I(new)
        if (!identical(new, I(old))) {
            body <- list(subvariables = new)
            payload <- toJSON(body)
            crPATCH(self(x), body = payload)
            dropCache(cubeURL(x))
            tuple(x)$subvariables <- new
        }
        return(x)
    }
)

#' @rdname crunch-extract
#' @export
setMethod("[[", c("Subvariables", "character"), function(x, i, ...) {
    ## TODO: drop this in favor of ShojiCatalog method (have to pass in namekey)
    i <- match(i, names(x))
    if (is.na(i)) {
        return(NULL)
    }
    return(x[[i, ...]])
})
#' @rdname crunch-extract
#' @export
setMethod("[[", c("Subvariables", "numeric"), function(x, i, ...) {
    out <- callNextMethod(x, i, ...)
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter = activeFilter(x))
    }
    return(out)
})

#' @rdname crunch-extract
#' @export
setMethod("[", c("Subvariables", "character"), function(x, i, ...) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("Subvariables", "character", "missing", "CrunchVariable"),
    function(x, i, value) {
        i <- match(i, names(x))
        if (is.na(i)) {
            ## Maybe we changed the name and that's what we're assigning back.
            ## Check URLs instead.
            i <- match(self(value), urls(x))
        }
        if (is.na(i)) {
            halt("subscript out of bounds")
        }
        x[[i]] <- value ## "callNextMethod"
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("Subvariables", "ANY", "missing", "CrunchVariable"),
    function(x, i, value) {
        if (self(value) != urls(x)[i]) {
            halt("Cannot add or remove subvariables")
        }
        index(x)[[self(value)]] <- tuple(value)@body
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("Subvariables", "ANY", "missing", "NULL"),
    function(x, i, value) {
        halt("Cannot add or remove subvariables")
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("Subvariables", "ANY", "missing", "ANY"),
    function(x, i, value) {
        halt("Can only assign Variables into an object of class Subvariables")
    }
)

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("Subvariables", "character", "missing", "Subvariables"),
    function(x, i, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("Subvariables", "ANY", "missing", "Subvariables"),
    function(x, i, value) {
        inbound <- vapply(value, function(a) self(a), character(1))
        if (!all(inbound %in% urls(x)[i])) {
            halt("Cannot add or remove subvariables")
        }
        index(x)[i] <- index(value)
        names(index(x))[i] <- inbound
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("Subvariables", "ANY", "missing", "ANY"),
    function(x, i, value) {
        halt("Can only assign Variables into an object of class Subvariables")
    }
)

#' @export
as.list.Subvariables <- function(x, ...) lapply(names(x), function(i) x[[i]])


#' @rdname describe-catalog
#' @export
setMethod("names", "CategoricalArrayVariable", function(x) {
    getIndexSlot(subvariables(x), namekey(x))
})

## NOTE:
## (1) The [ method returns Subvariables, not a subsetted array variable
## (2) As a result, you cannot filter and select subvariables
## (e.g. ds$array[ds$gender == "Male", c("subvar1", "subvar3")])

#' @rdname crunch-extract
#' @export
setMethod("[", c("CategoricalArrayVariable", "character"), function(x, i, ...) {
    w <- match(i, names(x)) ## TODO: use whichNameOrURL
    if (any(is.na(w))) {
        halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
    }
    return(subvariables(x)[w, ...])
})
#' @rdname crunch-extract
#' @export
setMethod("[", c("CategoricalArrayVariable", "missing", "ANY"), function(x, i, j, ...) {
    return(subvariables(x)[j, ...])
})
#' @rdname crunch-extract
#' @export
setMethod(
    "[", c("CategoricalArrayVariable", "missing", "character"),
    function(x, i, j, ...) {
        return(x[j])
    }
)

#' @rdname crunch-extract
#' @export
setMethod("[[", c("CategoricalArrayVariable", "ANY"), function(x, i, ...) {
    return(subvariables(x)[[i, ...]])
})
#' @rdname crunch-extract
#' @export
setMethod("[[", c("CategoricalArrayVariable", "character"), function(x, i, ...) {
    i <- match(i, names(x))
    if (is.na(i)) {
        return(NULL)
    }
    return(x[[i, ...]])
})

#' @rdname crunch-extract
#' @export
setMethod("$", "CategoricalArrayVariable", function(x, name) x[[name]])


#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("CategoricalArrayVariable", "ANY", "missing", "ANY"),
    function(x, i, value) {
        subvariables(x)[[i]] <- value
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("CategoricalArrayVariable", "character", "missing", "ANY"),
    function(x, i, value) {
        i <- match(i, names(x))
        if (is.na(i)) {
            ## Maybe we changed the name and that's what we're assigning back.
            ## Check URLs instead.
            i <- match(self(value), urls(subvariables(x)))
        }
        if (is.na(i)) {
            halt("subscript out of bounds")
        }
        subvariables(x)[[i]] <- value ## "callNextMethod"
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod("$<-", "CategoricalArrayVariable", function(x, name, value) {
    x[[name]] <- value
    return(x)
})
