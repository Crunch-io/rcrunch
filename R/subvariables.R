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
#' @seealso [`subvars-extract`] [`describe-catalog`] [`deleteSubvariable`] `vignette("array-variables", package="crunch")`
NULL

#' @rdname Subvariables
#' @export
setMethod("subvariables", "CategoricalArrayVariable", function (x) {
    tup <- tuple(x)
    catalog_url <- absoluteURL(tup$subvariables_catalog, base=tup@index_url)
    vars <- VariableCatalog(crGET(catalog_url))
    out <- Subvariables(vars[subvariables(tup)])
    activeFilter(out) <- activeFilter(x)
    return(out)
})

#' @rdname Subvariables
#' @export
setMethod("subvariables", "VariableTuple", function (x) {
    ## Return subvariable *urls* from a Tuple, properly formatted and absolute
    return(absoluteURL(unlist(x$subvariables), base=x@index_url))
})

#' @rdname Subvariables
#' @export
setMethod("subvariables<-", c("CategoricalArrayVariable", "ANY"),
    function (x, value) {
        halt("Can only assign an object of class Subvariables")
    })
#' @rdname Subvariables
#' @export
setMethod("subvariables<-", c("CategoricalArrayVariable", "Subvariables"),
    function (x, value) {
        old <- subvariables(tuple(x))
        new <- urls(value)
        if (!setequal(old, new)) {
            halt("Can only reorder, not change, subvariables")
        }
        new <- I(new)
        if (!identical(new, I(old))) {
            body <- list(subvariables=new)
            payload <- toJSON(body)
            crPATCH(self(x), body=payload)
            dropCache(cubeURL(x))
            tuple(x)$subvariables <- new
        }
        return(x)
    })

#' Extract and modify subsets of subvariables
#'
#' @param x Subvariables or an array Variable which contains subvariables
#' @param i which subvariables to extract
#' @param name For `$`, the name (not alias) of the subvariable to
#' extract
#' @param j Invalid
#' @param drop Invalid
#' @param ... additional arguments
#' @param value For updating, a CrunchExpr
#' @return A subset of `x` if extracting, otherwise `x` duly modified
#' @name subvars-extract
NULL

#' @rdname subvars-extract
#' @export
setMethod("[[", c("Subvariables", "character"), function (x, i, ...) {
    ## TODO: drop this in favor of ShojiCatalog method (have to pass in namekey)
    i <- match(i, names(x))
    if (is.na(i)) return(NULL)
    return(x[[i, ...]])
})
#' @rdname subvars-extract
#' @export
setMethod("[[", c("Subvariables", "numeric"), function (x, i, ...) {
    out <- callNextMethod(x, i, ...)
    if (!is.null(out)) {
        out <- CrunchVariable(out, filter=activeFilter(x))
    }
    return(out)
})

#' @rdname subvars-extract
#' @export
setMethod("[", c("Subvariables", "character"), function (x, i, ...) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
    }
    return(x[w])
})

#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("Subvariables", "character", "missing", "CrunchVariable"),
    function (x, i, value) {
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
    })
#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("Subvariables", "ANY", "missing", "CrunchVariable"),
    function (x, i, value) {
        if (self(value) != urls(x)[i]) {
            halt("Cannot add or remove subvariables")
        }
        index(x)[[self(value)]] <- tuple(value)@body
        return(x)
    })
#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("Subvariables", "ANY", "missing", "NULL"),
    function (x, i, value) {
        halt("Cannot add or remove subvariables")
    })
#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("Subvariables", "ANY", "missing", "ANY"),
    function (x, i, value) {
        halt("Can only assign Variables into an object of class Subvariables")
    })

#' @rdname subvars-extract
#' @export
setMethod("[<-", c("Subvariables", "character", "missing", "Subvariables"),
    function (x, i, value) {
        w <- match(i, names(x))
        if (any(is.na(w))) {
            halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
        }
        callNextMethod(x, w, value)
    })
#' @rdname subvars-extract
#' @export
setMethod("[<-", c("Subvariables", "ANY", "missing", "Subvariables"),
    function (x, i, value) {
        inbound <- vapply(value, function (a) self(a), character(1))
        if (!all(inbound %in% urls(x)[i])) {
            halt("Cannot add or remove subvariables")
        }
        index(x)[i] <- index(value)
        names(index(x))[i] <- inbound
        return(x)
    })
#' @rdname subvars-extract
#' @export
setMethod("[<-", c("Subvariables", "ANY", "missing", "ANY"),
    function (x, i, value) {
        halt("Can only assign Variables into an object of class Subvariables")
    })

#' @export
as.list.Subvariables <- function (x, ...) lapply(names(x), function (i) x[[i]])


#' @rdname describe-catalog
#' @export
setMethod("names", "CategoricalArrayVariable", function (x) {
    getIndexSlot(subvariables(x), namekey(x))
})

## NOTE:
## (1) The [ method returns Subvariables, not a subsetted array variable
## (2) As a result, you cannot filter and select subvariables
## (e.g. ds$array[ds$gender == "Male", c("subvar1", "subvar3")])

#' @rdname subvars-extract
#' @export
setMethod("[", c("CategoricalArrayVariable", "character"), function (x, i, ...) {
    w <- match(i, names(x)) ## TODO: use whichNameOrURL
    if (any(is.na(w))) {
        halt("Undefined subvariables selected: ", serialPaste(i[is.na(w)]))
    }
    return(subvariables(x)[w, ...])
})
#' @rdname subvars-extract
#' @export
setMethod("[", c("CategoricalArrayVariable", "missing", "ANY"), function (x, i, j, ...) {
    return(subvariables(x)[j, ...])
})
#' @rdname subvars-extract
#' @export
setMethod("[", c("CategoricalArrayVariable", "missing", "character"), function (x, i, j, ...) {
    return(x[j])
})

#' @rdname subvars-extract
#' @export
setMethod("[[", c("CategoricalArrayVariable", "ANY"), function (x, i, ...) {
    return(subvariables(x)[[i, ...]])
})
#' @rdname subvars-extract
#' @export
setMethod("[[", c("CategoricalArrayVariable", "character"), function (x, i, ...) {
    i <- match(i, names(x))
    if (is.na(i)) return(NULL)
    return(x[[i, ...]])
})

#' @rdname subvars-extract
#' @export
setMethod("$", "CategoricalArrayVariable", function (x, name) x[[name]])


#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("CategoricalArrayVariable", "ANY", "missing", "ANY"),
    function (x, i, value) {
        subvariables(x)[[i]] <- value
        return(x)
    })
#' @rdname subvars-extract
#' @export
setMethod("[[<-",
    c("CategoricalArrayVariable", "character", "missing", "ANY"),
    function (x, i, value) {
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
    })
#' @rdname subvars-extract
#' @export
setMethod("$<-", "CategoricalArrayVariable", function (x, name, value) {
    x[[name]] <- value
    return(x)
})
