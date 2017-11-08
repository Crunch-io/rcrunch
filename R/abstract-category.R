is.abstract.category <- function (x) inherits(x, "AbsCat")

#' @rdname category-extract
#' @export
setMethod("$", "AbsCat", function (x, name) x[[name]])
#' @rdname category-extract
#' @export
setMethod("$<-", "AbsCat", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

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
    v <- as.numeric(x[["numeric_value"]])
    return(ifelse(is.null(v), NA_real_, v))
})
#' @rdname describe-category
#' @export
setMethod("value<-", "AbsCat", setValue)
#' @rdname describe-category
#' @export
setMethod("id", "AbsCat", function (x) {
    i <- as.integer(x[["id"]])
    return(ifelse(is.null(i), NA_integer_, i))
})

#' @rdname describe-category
#' @export
setMethod("is.selected", "AbsCat", function (x) isTRUE(x$selected))
#' @rdname is-selected-categories
#' @export
setMethod("is.selected<-", "AbsCat", function (x, value) {
    if (!is.TRUEorFALSE(value)) {
        halt("Value must be either TRUE or FALSE.")
    }
    x$selected <- value
    return(x)
})


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

#' @rdname Insertions
#' @export
setMethod("args", "AbsCat", function (x) {
    func <- function(x)
        if (is.null(func)) {
            return(NA)
        }
    if (all(is.null(x[["args"]]))) {
        return(NA)
    }
    return(x[["args"]])
})

#' @rdname Insertions
#' @export
setMethod("anchor", "AbsCat", function (x) {
    n <- x[["anchor"]]
    return(ifelse(is.null(n), NA_integer_, as.integer(n)))
})

#' @rdname Insertions
#' @export
setMethod("func", "AbsCat", function (x) {
    f <- x[["function"]]

    return(ifelse(is.null(f), NA_character_, f))
})


#' @rdname Insertions
#' @export
setMethod("func", "NULL", function (x) NA_character_)

#' @rdname Insertions
#' @export
setMethod("anchor", "NULL", function (x) NA_character_)
