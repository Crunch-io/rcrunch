is.abstract.category <- function (x) inherits(x, "AbsCat")

is.abstract.categories <- function (x) inherits(x, "AbsCats")

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

#' @rdname Categories
#' @export
setMethod("names", "AbsCats", function (x) {
    n <- vapply(x, name, character(1))
    return(n)
})

#' @rdname Categories
#' @export
setMethod("ids", "AbsCats", function (x) vapply(x, id, integer(1)))

setMethod("lapply", "AbsCats", function (X, FUN, ...) {
    X@.Data <- lapply(X@.Data, FUN, ...)
    return(X)
})

#' is.na for Categories
#'
#' Crunch categorical variables allow you to set multiple categories as missing.
#' For instance, you might have "not answered" and "doesn't know" both coded as
#' missing. This function returns a logical vector of all dataset entries that
#' fall into any of the missing categories. It also allows you to append
#' additional categories to the list of missing categories using the setter.
#'
#' @param x Categories or a single Category
#' @param value To change the missingness of categories, supply either:
#' 1. a logical vector of equal length of the categories (or length 1 for the
#' Category method); or
#' 1. the names of the categories to mark as missing.
#' If supplying the latter, any categories already indicated as missing will
#' remain missing.
#' @return Getters return logical, a named vector in the case of the Categories
#' method; setters return `x` duly modified.
#' @name is-na-categories
NULL

#' @rdname is-na-categories
#' @aliases is-na-categories
#' @export
setMethod("is.na", "AbsCats", function (x) structure(vapply(x, is.na, logical(1), USE.NAMES=FALSE), .Names=names(x)))



#' @rdname Categories
#' @export
setMethod("[", c("AbsCats", "ANY"), function (x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[", c("AbsCats", "character"), function (x, i, ...) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        halt("subscript out of bounds: ", serialPaste(i[is.na(indices)]))
    }
    callNextMethod(x, i=indices)
})

#' @rdname Categories
#' @export
setMethod("[", c("AbsCats", "numeric"), function (x, i, ...) {
    invalid.indices <- setdiff(abs(i), seq_along(x@.Data))
    if (length(invalid.indices)) {
        halt("subscript out of bounds: ", serialPaste(invalid.indices))
    }
    x@.Data <- x@.Data[i]
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[<-", c("AbsCats", "character"), function (x, i, ..., value) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        # if there are no matches, add it on to the end
        indices <- i
    }
    x@.Data[indices] <- value
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[[", c("AbsCats", "character"), function (x, i, ...) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        halt("subscript out of bounds: ", serialPaste(i[is.na(indices)]))
    }
    callNextMethod(x, i=indices)
})

#' @rdname Categories
#' @export
setMethod("[[<-", c("AbsCats", "character"), function (x, i, ..., value) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        # if there are no matches, add it on to the end
        x@.Data[[i]] <- value
        return(x)
    }
    callNextMethod(x, i=indices, value)
})

# a version of modifyList that doesn't recurse into the absCats themselves
modifyCats <- function (x, val) {
    stopifnot(is.abstract.categories(x), is.abstract.categories(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    for (v in vnames) {
        x[[v]] <- val[[v]]
    }

    return(x)
}
