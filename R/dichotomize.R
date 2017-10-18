#' Indicate how categories represent a dichotomized value
#'
#' Multiple Response variables are essentially Categorical Arrays in which one
#' or more categories are set as "selected". These methods allow you set that state.
#'
#' `dichotomize` lets you specify which categories are "selected", while
#' `undichotomize` strips that selection information. Dichotomize converts
#' a Categorical Array to a Multiple Response, and undichotomize does the reverse.
#'
#' @param x Categories or a Variable subclass that has Categories
#' @param i For the `dichotomize` methods, the numeric or logical indices
#' of the categories to mark as "selected", or if character, the Category
#' "names". Note that unlike some other categorical variable methods,
#' numeric indices are positional, not with reference to category ids.
#' @return Categories or the Variable, (un)dichotomized accordingly
#' @name dichotomize
#' @aliases dichotomize is.dichotomized undichotomize
#' @seealso [`describe-category`]
NULL

#' @rdname dichotomize
#' @export
setMethod("is.dichotomized", "Categories",
    function (x) any(vapply(x, is.selected, logical(1))))

.dichotomize.categories <- function (x, i) {
    ## Internal method for dichtomizing Categories (or lists)
    selections <- is.selected(x)
    selections[i] <- TRUE
    is.selected(x) <- selections
    return(x)
}

#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "numeric"), .dichotomize.categories)
#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "logical"), .dichotomize.categories)
#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "character"), function (x, i) {
    ind <- names(x) %in% i
    if (!any(ind)) {
        halt("Category not found") ## make nicer error message
    }
    return(dichotomize(x, ind))
})

#' @rdname dichotomize
#' @export
setMethod("undichotomize", "Categories", function (x) {
    is.selected(x) <- FALSE
    return(x)
})

.dichotomize.var <- function (x, i) {
    newcats <- dichotomize(categories(x), i)
    categories(x) <- newcats
    if (is.dichotomized(newcats)) {
        ## Do this to avoid needing to refresh the variable catalog
        x@tuple@body$type <- "multiple_response"
    }
    invisible(CrunchVariable(tuple(x)))
}
.undichotomize.var <- function (x) {
    categories(x) <- undichotomize(categories(x))
    ## Do this to avoid needing to refresh the variable catalog
    x@tuple@body$type <- "categorical_array"
    invisible(CrunchVariable(tuple(x)))
}

#' @rdname dichotomize
#' @export
setMethod("dichotomize", "CategoricalVariable", .dichotomize.var)
#' @rdname dichotomize
#' @export
setMethod("dichotomize", "CategoricalArrayVariable", .dichotomize.var)
#' @rdname dichotomize
#' @export
setMethod("undichotomize", "CategoricalVariable", .undichotomize.var)
#' @rdname dichotomize
#' @export
setMethod("undichotomize", "CategoricalArrayVariable", .undichotomize.var)
