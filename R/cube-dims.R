cubeDims <- function (cube) {
    ## This function wraps up all of the quirks of the Crunch cube API and
    ## standardizes the various ways that metadata about the dimensions is
    ## represented in it.

    ## First, grab the row/col/etc. labels from the cube
    dimnames <- lapply(cube$result$dimensions, function (a) {
        ## Collect the variable metadata about the dimensions
        tuple <- cubeVarReferences(a)
        if (tuple$type == "boolean") {
            ## TODO: delete this when boolean support is removed
            return(list(
                name=c("FALSE", "TRUE"),
                any.or.none=c(FALSE, FALSE),
                missing=c(FALSE, FALSE),
                references=tuple
            ))
        }
        ## If enumerated, will be "elements", not "categories"
        d <- tuple$categories %||% a$type$elements
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

cubeVarReferences <- function (x) {
    ## Extract ZZ9-ish metadata from a cube dimension or measure and return
    ## in a way that looks like what comes in a variable catalog
    tuple <- x$references
    tuple$type <- x$type$class
    if (tuple$type == "enum" && "subreferences" %in% names(tuple)) {
        tuple$type <- "subvariable_items"
    }

    if (!is.null(tuple$subreferences)) {
        # inject subreference names into the tuple if they exist. Default to ""
        # if null (for backwards compatibilitiy with cubes that have __any__
        # __all__ and __none__)
        tuple$subvariables <- vapply(tuple$subreferences,
                                     function (x) x$alias %||% "",
                                     character(1))

        if (is.null(names(tuple$subreferences))) {
            # if there are no names for the subvariable elements, fake urls from
            # the aliases
            names(tuple$subreferences) <- tuple$subvariables
        }

        # add a trailling slash to match how urls will look.
        tuple$subvariables <- paste0(tuple$subvariables, "/")
    }

    tuple$categories <- x$type$categories
    ## Sniff for 3VL
    if (!is.null(tuple$categories) && is.3vl(Categories(data=tuple$categories))) {
        ## Make this look like an R logical does when it is tabulated
        tuple$categories[[1]]$name <- "TRUE"
        tuple$categories[[2]]$name <- "FALSE"
        ## TODO: Put FALSE first, like in R
        ## But note that you'd also have to aperm the data arrays...
        # tuple$categories <- tuple$categories[c(2, 1, 3)]
    }
    return(tuple)
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
#' @param i used with `[` to extract a dimension
#' @param j not used
#' @param ... not used
#' @param drop not used
#' @param value for `dimensions<-` a `CubeDims` object to overwrite a CrunchCube
#' dimensions
#'
#' @return Generally, the same shape of result that each of these functions
#' return when applied to an `array` object.
#' @name cube-methods
#' @aliases cube-methods dimensions dimensions<- measures
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

#' @rdname cube-methods
#' @export
setMethod("dimensions<-", c("CrunchCube", "CubeDims"), function (x, value) {
    dims <- x@dims
    selecteds <- is.selectedDimension(dims)
    x@dims[!selecteds] <- value
    return(invisible(x))
})

#' @rdname cube-methods
#' @export
setMethod("[", "CubeDims", function (x, i, ...) {
    return(CubeDims(x@.Data[i], names=x@names[i]))
})

is.selectedDimension <- function (dims) {
    is.it <- function (x, dim, MRaliases) {
        maybe <- x$alias %in% MRaliases &&
            x$type == "categorical" &&
            length(dim$name) == 3
        if (maybe) {
            cats <- Categories(data=x$categories)
            ## Unlike the strict is.3vl, this doesn't compare cat names because
            ## they've already been munged to TRUE/FALSE
            maybe <- setequal(ids(cats), c(-1, 0, 1)) &&
                sum(is.selected(cats)) == 1 &&
                sum(is.na(cats)) == 1
        }
        return(maybe)
    }
    vars <- variables(dims)
    # We only need to check if the categories are the magical Selected
    # categories if there is an MR somewhere with the same alias
    MRaliases <- aliases(vars)[types(vars) == "subvariable_items"]

    # determine which dimensions are selected MR dimensions
    selecteds <- mapply(is.it, x=index(vars), dim=dims@.Data,
                        MoreArgs=list(MRaliases=MRaliases))
    names(selecteds) <- dims@names
    return(selecteds)
}

# determine if a dimension is from the selected_array of a multiple response
is.selectedArrayDim <- function (dim) {
    if (!is.null(dim$any.or.none)) {
        return(any(dim$any.or.none))
    }

    return(FALSE)
}
