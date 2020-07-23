is.category <- function(x) inherits(x, "Category")

setValidity("Category", function(object) {
    is.cat <- all(c("name") %in% names(object))
    if (!all(is.cat)) {
        val <- "Not a category"
    } else {
        val <- TRUE
    }
    return(val)
})

init.Category <- function(.Object, ...) {
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

setValue <- function(x, value) {
    value_to_set <- suppressWarnings(as.numeric(value))
    if (is.na(value_to_set) && !is.na(value)) {
        halt("Category values must be numeric")
    }
    x[["numeric_value"]] <- value_to_set
    return(x)
}

#' @rdname describe-entity
#' @export
setMethod("value", "Category", function(x) {
    v <- as.numeric(x[["numeric_value"]])
    return(ifelse(is.null(v), NA_real_, v))
})

#' @rdname describe-entity
#' @export
setMethod("value<-", "Category", setValue)

setDate <- function(x, value) {
    # Relies on server validation for acceptable formats
    # Consider adding strftime/strptime support if users
    # are used to working with dates (most are probably not)
    value_to_set <- suppressWarnings(as.character(value))
    x[["date"]] <- value_to_set
    return(x)
}

#' @export
setMethod("dates", "Category", function(x) {
    v <- as.character(x[["date"]])
    return(ifelse(is.null(v), NA_character_, v))
})

#' @rdname describe-entity
#' @export
setMethod("dates<-", "Category", setDate)


#' @rdname is-na-categories
#' @export
setMethod("is.na", "Category", function(x) isTRUE(x$missing))

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("Category", "logical"), function(x, value) {
    stopifnot(length(value) == 1)
    x$missing <- isTRUE(value)
    return(x)
})
