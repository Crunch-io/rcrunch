cubeDims <- function(cube) {
    ## This function wraps up all of the quirks of the Crunch cube API and
    ## standardizes the various ways that metadata about the dimensions is
    ## represented in it.

    ## First, grab the row/col/etc. labels from the cube
    dimnames <- lapply(cube$result$dimensions, function(a) {
        ## Collect the variable metadata about the dimensions
        tuple <- cubeVarReferences(a)
        ## If enumerated, will be "elements", not "categories"
        d <- tuple$categories %||% a$type$elements
        return(list(
            name = vapply(d, elementName, character(1)),
            missing = vapply(d, function(el) isTRUE(el$missing), logical(1)),
            references = tuple
        ))
    })

    names(dimnames) <- vapply(
        dimnames, function(x) x$references$alias,
        character(1)
    )

    return(CubeDims(dimnames))
}

cubeVarReferences <- function(x) {
    ## Extract ZZ9-ish metadata from a cube dimension or measure and return
    ## in a way that looks like what comes in a variable catalog
    tuple <- x$references
    tuple$type <- x$type$class
    if (tuple$type == "enum" && "subreferences" %in% names(tuple)) {
        tuple$type <- "subvariable_items"
    }

    if (!is.null(tuple$subreferences)) {
        # Inject subreference names into the tuple if they exist.
        tuple$subvariables <- vapply(
            tuple$subreferences,
            function(x) x$alias %||% "",
            character(1)
        )

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
    if (!is.null(tuple$categories) && is.3vl(Categories(data = tuple$categories))) {
        ## Make this look like an R logical does when it is tabulated
        tuple$categories[[1]]$name <- "TRUE"
        tuple$categories[[2]]$name <- "FALSE"
        ## TODO: Put FALSE first, like in R
        ## But note that you'd also have to aperm the data arrays...
        # tuple$categories <- tuple$categories[c(2, 1, 3)]
    }
    return(tuple)
}

elementName <- function(el) {
    ## Given a category or element in a cube dim, what's its name?
    out <- el$value
    if (is.null(out)) {
        ## This is probably categorical. Try "name" instead of "value".
        out <- el$name
    } else if (is.list(out)) {
        if (length(out) == 2 && is.null(names(out))) {
            ## el$value is bin boundaries, as in a binned numeric.
            out <- paste(unlist(out), collapse = "-")
        } else {
            ## This is probably a subvariable. Look for its name.
            out <- out$references$name
        }
    }
    out <- as.character(out %||% "<NA>")
    return(out)
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
setMethod("dimnames", "CubeDims", function(x) {
    lapply(x, function(a) a$name)
})

#' @rdname cube-methods
#' @export
setMethod(
    "dim", "CubeDims",
    function(x) vapply(dimnames(x), length, integer(1), USE.NAMES = FALSE)
)

#' @rdname cube-methods
#' @export
setMethod("is.na", "CubeDims", function(x) lapply(x, function(a) a$missing))

#' @rdname cube-methods
#' @export
setMethod("dimensions", "CrunchCube", function(x) {
    dims <- x@dims
    selecteds <- is.selectedDimension(dims)
    return(dims[!selecteds])
})

#' @rdname cube-methods
#' @export
setMethod("dimensions<-", c("CrunchCube", "CubeDims"), function(x, value) {
    dims <- x@dims
    selecteds <- is.selectedDimension(dims)
    x@dims[!selecteds] <- value
    return(invisible(x))
})

#' @rdname cube-methods
#' @export
setMethod("[", "CubeDims", function(x, i, ...) {
    return(CubeDims(x@.Data[i], names = x@names[i]))
})

is.selectedDimension <- function(dims) getDimTypes(dims) == "mr_selections"

#' Get dimension type
#'
#' This function returns the specific type of each cube dimension. This is useful
#' when cubes contain categorical array or multiple response variables because it
#' identifies the dimensions of the cube which refer to the different parts of
#' array variable:
#' - `ca_items`: Categorical array items
#' - `ca_categories`: The categories of the categorical array
#' - `mr_items`: Multiple response options or items
#' - `mr_selections`: The selection status for a multiple response variable
#'
#' @param x a CrunchCube or CubeDims object
#'
#' @return a character vector of dimension types, similar to `types()`, except that
#' the array variable types are more specific.
#' @export
#' @keywords internal
getDimTypes <- function(x) {
    if (inherits(x, "CrunchCube")) {
        x <- x@dims
    }

    what_dim_is_it <- function(one_var, array_aliases) {
        dim_type <- type(one_var)

        if (alias(one_var) %in% array_aliases) {
            # we are in an array, we need to figure out if this is a multiple
            # response or not
            array_cat_dim <- vars[aliases(vars) == alias(one_var) & types(vars) == "categorical"]

            # if we can't find the matching categories dimension we might have a
            # subset cube, so simply return the dim_type un-identified (this
            # might could actually just be "ca_items")
            if (length(array_cat_dim) < 1) {
                return(dim_type)
            }

            # if this is a variable crossed by itself, then array_cat_dim will
            # actually have two copies of the categories dimension. We take the
            # first one becasue it should be identical to all the others. If it
            # isn't this might produce weird results.
            # TODO: Investigate checking by ID
            array_cats <- categories(array_cat_dim[[1]])

            # if we meet these conditions, we are actually a multiple response
            # and should label ourself as such.
            # Unlike the strict is.3vl, this doesn't compare cat names because
            # they've already been munged to TRUE/FALSE
            is.MR <- length(array_cats) == 3 &&
                setequal(ids(array_cats), c(-1, 0, 1)) &&
                sum(is.selected(array_cats)) == 1 &&
                sum(is.na(array_cats)) == 1

            if (is.MR) {
                # MRs have mr_items or mr_selections
                if (dim_type == "subvariable_items") {
                    dim_type <- "mr_items"
                } else if (dim_type == "categorical") {
                    dim_type <- "mr_selections"
                }
            } else {
                # categorical arrays have ca_items or ca_categories
                if (dim_type == "subvariable_items") {
                    dim_type <- "ca_items"
                } else if (dim_type == "categorical") {
                    dim_type <- "ca_categories"
                }
            }
        }

        return(dim_type)
    }

    vars <- variables(x)
    array_aliases <- aliases(vars)[types(vars) == "subvariable_items"]
    out <- vapply(vars, what_dim_is_it, character(1), array_aliases)

    names(out) <- names(vars)
    return(out)
}

# determine if a dimension is from the selected_array of a multiple response
is.selectedArrayDim <- function (dim) {
    if (!is.null(dim$any.or.none)) {
        return(any(dim$any.or.none))
    }

    return(FALSE)
}
