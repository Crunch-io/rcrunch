#' Create variables useful for determining whether a row's values are suspicious
#'
#' `rowDistinct()` finds the number of unique values given per row of variables in an array
#' `CrunchVariable`. `straightlineResponse()` returns a `selection` variable that indicates
#' whether the responses are identical. When a row has all columns that are missing of the
#' same type, it will return `Selected`, but will missing if any other number of values is missing
#' (or there are multiple types of missing).
#'
#' @param x A `CrunchVariable`that is an an array, that unique values should be counted across.
#' @param ... Optional attributes, like `name`, to set on the new variable (passed to `VarDef()`)
#' @param na.rm Whether to count missing data as a separate category (all missing categories will be
#' lumped together)
#'
#' @return A Variable Definition, which can be used to create a new `CrunchVariable`
#' @export
rowDistinct <- function(x, ..., na.rm = TRUE) {
    if (!is.Array(x) & !inherits(x, "Subvariables")) halt("x must be an array variable")
    if (inherits(x, "Subvariables")) {
        # https://github.com/Crunch-io/rcrunch/issues/457
        halt("`rowDistinct` does not yet support subsets of array subvariables")
    }

    if (na.rm) {
        unique_func <- function(x) length(unique(as.character(x[!is.na(x)])))
    } else {
        unique_func <- function(x) length(unique(as.character(x)))
    }

    VarDef(
        apply(as.vector(x), 1, unique_func),
        ...
    )
}

#' @export
#' @rdname rowDistinct
straightlineResponse <- function(x, ...) {
    if (!is.Array(x) & !inherits(x, "Subvariables")) halt("x must be an array variable")
    if (is.Array(x)) x <- subvariables(x)

    # Don't have variable objects (because allowing subsetting of array vars), so use urls
    # and construct zfunc here rather than relying on `==`
    subvar_urls <- urls(x)
    if (length(subvar_urls) == 1) stop("Array must have more than 1 subvariable.")
    VarDef(
        Reduce(
            `&`,
            lapply(subvar_urls[-1], function(sv) varurl_equal(sv, subvar_urls[1], x))
        ),
        ...
    )
}

varurl_equal <- function(url1, url2, ref) {
    CrunchLogicalExpr(
        expression = zfunc("==", list(variable = url1), list(variable = url2))
    )
}
