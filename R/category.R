is.category <- function (x) inherits(x, "Category")

setValidity("Category", function (object) {
    is.cat <- all(c("id", "name") %in% names(object))
    if (!all(is.cat)) {
        val <- "Not a category"
    } else {
        val <- TRUE
    }
    return(val)
})

init.Category <- function (.Object, ...) {
    .Object <- callNextMethod()
    ## Make sure category elements are sorted so that identical categories are
    ## evaluated identically. Order doesn't matter for object, but R lists are
    ## ordered.
    s <- order(.Object@names)
    .Object@.Data <- .Object@.Data[s]
    .Object@names <- .Object@names[s]
    return(.Object)
}
setMethod("initialize", "Category", init.Category)

#' Access Category fields directly
#'
#' Don't do this. Instead, use the category setters.
#'
#' @param x a Category
#' @param name a field within \code{x}
#' @param value a value for that field to update
#' @return \code{$} returns the value of the desired field. Setter
#' returns \code{x} duly modified.
#' @seealso \code{\link{describe-category}}
#' @name category-extract
NULL

#' Category attributes
#'
#' Functions to access and set category attributes.
#'
#' @param x a Category
#' @param value For the setters, an appropriate value to set
#' @return `name` returns character; `value` and `id` return
#' numeric; value but not id may be `NA`; `is.selected` returns
#' logical indicating whether this Category is a "selected" dichotomy. Setters
#' return `x` duly modified.
#' @name describe-category
#' @aliases value value<- id is.selected describe-category
#' @seealso [`Categories`] [`dichotomize`]
NULL

setValue <- function (x, value) {
    value_to_set <- suppressWarnings(as.numeric(value))
    if (is.na(value_to_set) && !is.na(value)) {
        halt("Category values must be numeric")
    }
    x[["numeric_value"]] <- value_to_set
    return(x)
}

#' @rdname describe-category
#' @export
setMethod("value", "Category", function (x) {
    v <- as.numeric(x[["numeric_value"]])
    return(ifelse(is.null(v), NA_real_, v))
})

#' @rdname describe-category
#' @export
setMethod("value<-", "Category", setValue)

#' @rdname describe-category
#' @export
setMethod("is.selected", "Category", function (x) isTRUE(x$selected))
#' @rdname is-selected-categories
#' @export
setMethod("is.selected<-", "Category", function (x, value) {
    if (!is.TRUEorFALSE(value)) {
        halt("Value must be either TRUE or FALSE.")
    }
    x$selected <- value
    return(x)
})

#' @rdname is-na-categories
#' @export
setMethod("is.na", "Category", function (x) isTRUE(x$missing))

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("Category", "logical"), function (x, value) {
    stopifnot(length(value) == 1)
    x$missing <- isTRUE(value)
    return(x)
})
