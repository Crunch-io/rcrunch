cubeDims <- function (cube) {
    ## This function wraps up all of the quirks of the Crunch cube API and
    ## standardizes the various ways that metadata about the dimensions is
    ## represented in it.

    ## First, grab the row/col/etc. labels from the cube
    dimnames <- lapply(cube$result$dimensions, function (a) {
        ## Collect the variable metadata about the dimensions
        tuple <- a$references
        tuple$type <- a$type$class
        if (tuple$type == "enum" && "subreferences" %in% names(tuple)) {
            tuple$type <- "multiple_response"
        }
        tuple$categories <- a$type$categories

        if (tuple$type == "boolean") {
            ## TODO: server should provide enumeration
            return(list(
                name=c("FALSE", "TRUE"),
                any.or.none=c(FALSE, FALSE),
                missing=c(FALSE, FALSE),
                references=tuple
            ))
        }
        ## If enumerated, will be "elements", not "categories"
        d <- a$type$categories %||% a$type$elements
        return(list(
            name=vapply(d, elementName, character(1)),
            any.or.none=vapply(d, elementIsAnyOrNone, logical(1)),
            missing=vapply(d, function (el) isTRUE(el$missing), logical(1)),
            references=tuple
        ))
    })
    names(dimnames) <- vapply(dimnames, function (x) x$references$alias,
        character(1))
    return(CubeDims(dimnames))
}

elementName <- function (el) {
    ## Given a category or element in a cube dim, what's its name?
    out <- el$value
    if (is.null(out)) {
        ## This is probably categorical. Try "name" instead of "value".
        out <- el$name
    } else if (is.list(out)) {
        if (length(out) == 2 && is.null(names(out))) {
            ## el$value is bin boundaries, as in a binned numeric.
            out <- paste(unlist(out), collapse="-")
        } else {
            ## This is probably a subvariable. Look for its name.
            out <- out$references$name
        }
    }
    if (is.null(out)) {
        ## Damn. You may be here because you're hitting missing values in an
        ## array or multiple response, or the __any__ or __none__ values.
        ## Bail out.
        out <- "<NA>"
    }
    out <- as.character(out)
    return(out)
}

elementIsAnyOrNone <- function (el) {
    is.list(el$value) && ## Element has $value and value is a list
        "id" %in% names(el$value) && ## "value" has names (is not bin)
        el$value$id %in% c("__any__", "__none__")
}

#' Methods on Cube objects
#'
#' These methods provide an `array`-like interface to the CrunchCube
#' object.
#'
#' @param x a CrunchCube or its CubeDims component.
#'
#' @return Generally, the same shape of result that each of these functions
#' return when applied to an `array` object.
#' @name cube-methods
#' @aliases cube-methods dimensions
#' @seealso [`cube-computing`] [`base::array`]
NULL

#' @rdname cube-methods
#' @export
setMethod("dimnames", "CubeDims", function (x) {
    lapply(x, function (a) a$name)
})

#' @rdname cube-methods
#' @export
setMethod("dim", "CubeDims",
    function (x) vapply(dimnames(x), length, integer(1), USE.NAMES=FALSE))

#' @rdname cube-methods
#' @export
setMethod("is.na", "CubeDims", function (x) lapply(x, function (a) a$missing))

anyOrNone <- function (x) {
    lapply(x, function (a) a$any.or.none)
}

#' @rdname cube-methods
#' @export
setMethod("dimensions", "CrunchCube", function (x) {
    dims <- x@dims
    selecteds <- is.selectedDimension(dims)
    return(dims[!selecteds])
})

setMethod("[", "CubeDims", function (x, i, ...) {
    return(CubeDims(x@.Data[i], names=x@names[i]))
})

is.selectedDimension <- function (dims) {
    is.it <- function (x, dim) {
        x$type == "categorical" && length(dim$name) == 3 && dim$name[1] == "Selected"
    }
    selecteds <- mapply(is.it, x=index(variables(dims)), dim=dims@.Data)
    names(selecteds) <- dims@names
    return(selecteds)
}

#' @rdname cube-methods
#' @export
setMethod("variables", "CubeDims", function (x) {
    VariableCatalog(index=lapply(x@.Data, vget("references")))
})

#' @rdname cube-methods
#' @export
setMethod("variables", "CrunchCube", function (x) variables(dimensions(x)))
