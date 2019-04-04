#' Indicate how categories represent a dichotomized value
#'
#' Multiple Response variables are Categorical Arrays in which one
#' or more categories are set as "selected". These methods allow you to view
#' and set that attribute.
#'
#' `dichotomize` lets you specify which categories are "selected", while
#' `undichotomize` strips that selection information. Dichotomize converts
#' a Categorical Array to a Multiple Response, and undichotomize does the reverse.
#' `is.dichotomized` reports whether categories have any selected values.
#'
#' `is.selected` is lower level and maps more directly onto the "selected"
#' attributes of categories. The best illustration of this difference is that
#' `is.selected(categories(var))` returns a logical vector, a value for each
#' category, while `is.dichotomized(categories(var))` returns a single
#' `TRUE/FALSE` value.
#'
#' @param x Categories or a Variable subclass that has Categories
#' @param i For the `dichotomize` methods, the numeric or logical indices
#' of the categories to mark as "selected", or if character, the Category
#' "names". Note that unlike some other categorical variable methods,
#' numeric indices are positional, not with reference to category ids.
#' @param value For `is.selected<-`,
#' A logical vector indicating whether the category should be selected.
#' For a single category the value should be either `TRUE` or `FALSE`. To change the
#' selection status for a `Categories` object, supply a logical vector which is the
#' same length as the number of categories.
#' @return Categories or the Variable, (un)dichotomized accordingly
#' @name dichotomize
#' @aliases dichotomize is.dichotomized undichotomize is.selected is.selected<-
#' @seealso [`describe-entity`]
#' @examples
#' \dontrun{
#' ds <- newExampleDataset()
#' is.MR(ds$allpets)
#' is.dichotomized(categories(ds$allpets))
#' is.selected(categories(ds$allpets))
#' ds$allpets <- undichotomize(ds$allpets)
#' is.CA(ds$allpets)
#' ds$allpets <- dichotomize(ds$allpets, "selected")
#' is.MR(ds$allpets)
#' }
setGeneric("is.dichotomized", function(x) standardGeneric("is.dichotomized"))
#' @rdname dichotomize
setGeneric("dichotomize", function(x, i) standardGeneric("dichotomize"))
#' @rdname dichotomize
setGeneric("undichotomize", function(x) standardGeneric("undichotomize"))
#' @rdname dichotomize
setGeneric("is.selected", function(x) standardGeneric("is.selected"))
#' @rdname dichotomize
setGeneric("is.selected<-", function(x, value) standardGeneric("is.selected<-"))

#' @rdname dichotomize
#' @export
setMethod("is.dichotomized", "Categories", function(x) any(is.selected(x)))

.dichotomize.categories <- function(x, i) {
    ## Internal method for dichtomizing Categories (or lists)
    is.selected(x[i]) <- TRUE
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
setMethod("dichotomize", c("Categories", "character"), function(x, i) {
    ind <- names(x) %in% i
    if (!any(ind)) {
        halt("Category not found") ## make nicer error message
    }
    return(dichotomize(x, ind))
})

#' @rdname dichotomize
#' @export
setMethod("undichotomize", "Categories", function(x) {
    is.selected(x) <- FALSE
    return(x)
})

.dichotomize.var <- function(x, i) {
    newcats <- dichotomize(categories(x), i)
    categories(x) <- newcats
    if (is.dichotomized(newcats)) {
        ## Do this to avoid needing to refresh the variable catalog
        x@tuple@body$type <- "multiple_response"
    }
    invisible(CrunchVariable(tuple(x)))
}
.undichotomize.var <- function(x) {
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

#' @rdname dichotomize
#' @export
setMethod("is.selected", "Categories", function(x) {
    structure(vapply(x, is.selected, logical(1), USE.NAMES = FALSE), .Names = names(x))
})

#' @rdname dichotomize
#' @export
setMethod("is.selected<-", "Categories", function(x, value) {
    if (is.TRUEorFALSE(value)) {
        value <- rep(value, length(x))
    }
    if (length(value) != length(x)) {
        halt(
            "You supplied ", length(value), " logical values for ", length(x),
            " Categories."
        )
    }

    x@.Data <- mapply(function(x, value) {
        is.selected(x) <- value
        return(x)
    }, x = x@.Data, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    return(x)
})

#' @rdname dichotomize
#' @export
setMethod("is.selected", "Category", function(x) isTRUE(x$selected))
#' @rdname dichotomize
#' @export
setMethod("is.selected<-", "Category", function(x, value) {
    if (!is.TRUEorFALSE(value)) {
        halt("Value must be either TRUE or FALSE.")
    }
    x$selected <- value
    return(x)
})
