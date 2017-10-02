is.abstract.category <- function (x) inherits(x, "AbsCat")

setName <- function (x, value) {
    x[["name"]] <- validateNewName(value)
    return(x)
}

validateNewName <- function (val) {
    if (!is.character(val)) {
        halt('Names must be of class "character"')
    }
    if (any(is.na(val))) {
        halt('Names must be non-missing')
    }
    invisible(val)
}

setValue <- function (x, value) {
    value_to_set <- suppressWarnings(as.numeric(value))
    if (is.na(value_to_set) && !is.na(value)) {
        halt("Category values must be numeric")
    }
    x[["numeric_value"]] <- value_to_set
    return(x)
}

getName <- function (x) {
    n <- x[["name"]]
    return(ifelse(is.null(n), NA_character_, n))
}

#' @rdname describe-category
#' @export
setMethod("name", "AbsCat", getName)
#' @rdname describe-category
#' @export
setMethod("name<-", "AbsCat", setName)
#' @rdname describe-category
#' @export
setMethod("name<-", "NULL", function (x, value) {
    halt('Cannot set name on NULL')
})

#' @rdname describe-category
#' @export
setMethod("value", "AbsCat", function (x) {
    v <- x[["numeric_value"]]
    return(ifelse(is.null(v), NA_real_, as.numeric(v)))
})
#' @rdname describe-category
#' @export
setMethod("value<-", "AbsCat", setValue)
#' @rdname describe-category
#' @export
setMethod("id", "AbsCat", function (x) as.integer(x[["id"]]))

#' @rdname describe-category
#' @export
setMethod("is.selected", "AbsCat", function (x) isTRUE(x$selected))

#' @rdname is-na-categories
#' @export
setMethod("is.na", "AbsCat", function (x) isTRUE(x$missing))

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("AbsCat", "logical"), function (x, value) {
    stopifnot(length(value) == 1)
    x$missing <- isTRUE(value)
    return(x)
})


setMethod("initialize", "AbsCats", function (.Object, ...) {
    # list of object constructors to use (based on the class being initialized)
    # for Categories, use Category
    # for Insertions use Insertion
    cat_consts <- list(AbsCats = AbsCat,
                       Categories = Category,
                       Insertions = Insertion)

    .Object@.Data <- lapply(..1, function (x) {
        try(cat_consts[[class(.Object)]](data=x), silent=TRUE)
    })
    validObject(.Object)
    return(.Object)
})
