is.AbstractCategory <- function (x) inherits(x, "AbstractCategory")

#' @rdname category-extract
#' @export
setMethod("$", "AbstractCategory", function (x, name) x[[name]])
#' @rdname category-extract
#' @export
setMethod("$<-", "AbstractCategory", function (x, name, value) {
    x[[name]] <- value
    return(x)
})

############################################
## Abstract Category general methods
############################################

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

getName <- function (x) {
    n <- x[["name"]]
    return(ifelse(is.null(n), NA_character_, n))
}

#' @rdname describe-category
#' @export
setMethod("name", "AbstractCategory", getName)
#' @rdname describe-category
#' @export
setMethod("name<-", "AbstractCategory", setName)
#' @rdname describe-category
#' @export
setMethod("name<-", "NULL", function (x, value) {
    halt('Cannot set name on NULL')
})

#' @rdname describe-category
#' @export
setMethod("id", "AbstractCategory", function (x) {
    i <- as.integer(x[["id"]])
    return(ifelse(is.null(i), NA_integer_, i))
})
