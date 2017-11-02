#' Indicate how categories represent a dichotomized value
#'
#' Multiple Response variables are essentially Categorical Arrays in which one
#' or more categories are set as "selected". These methods allow you set that state.
#'
#' `dichotomize` lets you specify which categories are "selected", while
#' `undichotomize` strips that selection information. Dichotomize converts
#' a Categorical Array to a Multiple Response, and undichotomize does the reverse.
#'
#' @param categories Categories or a Variable subclass that has Categories
#' @param selection For the `dichotomize` methods, the numeric or logical indices
#' of the categories to mark as "selected", or if character, the Category
#' "names". Note that unlike some other categorical variable methods,
#' numeric indices are positional, not with reference to category ids.
#' @return Categories or the Variable, (un)dichotomized accordingly
#' @name dichotomize
#' @aliases dichotomize is.dichotomized undichotomize
#' @seealso [`describe-category`]
#' @examples
#' \dontrun{
#' ds$sub1 <-  factor(1:nrow(ds))
#' ds$sub2 <-  factor(1:nrow(ds))
#' ds$arr <- makeArray(ds[, c("sub1", "sub2")], "array")
#' ds$arr <- dichotomize(ds$arr, 3)
#' class(ds$arr)
#' ds$arr <- undichotomize(ds$arr)
#' class(ds$arr)
#' }
NULL

#' @rdname dichotomize
#' @export
setMethod("is.dichotomized", "Categories",
    function (categories) any(vapply(categories, is.selected, logical(1))))

.dichotomize.categories <- function (categories, selection) {
    ## Internal method for dichtomizing Categories (or lists)
    is.selected(categories[selection]) <- TRUE
    return(categories)
}

#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "numeric"), .dichotomize.categories)
#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "logical"), .dichotomize.categories)
#' @rdname dichotomize
#' @export
setMethod("dichotomize", c("Categories", "character"), function (categories, selection) {
    ind <- names(categories) %in% selection
    if (!any(ind)) {
        halt("Category not found") ## make nicer error message
    }
    return(dichotomize(categories, ind))
})

#' @rdname dichotomize
#' @export
setMethod("undichotomize", "Categories", function (categories) {
    is.selected(categories) <- FALSE
    return(categories)
})

.dichotomize.var <- function (categories, selection) {
    newcats <- dichotomize(categories(categories), selection)
    categories(categories) <- newcats
    if (is.dichotomized(newcats)) {
        ## Do this to avoid needing to refresh the variable catalog
        categories@tuple@body$type <- "multiple_response"
    }
    invisible(CrunchVariable(tuple(categories)))
}
.undichotomize.var <- function (categories) {
    categories(categories) <- undichotomize(categories(categories))
    ## Do this to avoid needing to refresh the variable catalog
    categories@tuple@body$type <- "categorical_array"
    invisible(CrunchVariable(tuple(categories)))
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
