is.abstract.categories <- function (x) inherits(x, "AbsCats")

#' @rdname Categories
#' @export
setMethod("values", "AbsCats", function (x) vapply(x, value, numeric(1)))

setNames <- function (x, value) {
    if (is.null(value) || !is.character(value)) {
        halt('Names must be of class "character"')
    }
    if (!identical(length(x), length(value))) {
        halt("Invalid names: supplied ", length(value), " names for ",
             length(x), " categories")
    }
    if (any(is.na(value))) {
        halt("Category names must be non-missing")
    }
    x[] <- mapply(setName, x, value=value, SIMPLIFY=FALSE)
    return(x)
}
setValues <- function (x, value) {
    x[] <- mapply(setValue, x[], value=value, SIMPLIFY=FALSE)
    return(x)
}

#' @rdname Categories
#' @export
setMethod("names<-", "AbsCats", setNames)
#' @rdname Categories
#' @export
setMethod("values<-", "AbsCats", setValues)

#' @rdname is-na-categories
#' @aliases is-na-categories
#' @export
setMethod("is.na", "AbsCats", function (x) structure(vapply(x, is.na, logical(1), USE.NAMES=FALSE), .Names=names(x)))

#' @rdname is-selected-categories
#' @export
setMethod("is.selected", "AbsCats", function (x) structure(vapply(x, is.selected, logical(1), USE.NAMES=FALSE), .Names=names(x)))

#' @rdname is-selected-categories
#' @export
setMethod("is.selected<-", "AbsCats", function (x, value) {
    if (is.TRUEorFALSE(value)) {
        value <- rep(value, length(x))
    }
    if (length(value) != length(x)) {
        halt("You supplied ", length(value), " logical values for ", length(x), " Categories.")
    }

    x@.Data <- mapply(function (x, value) {
        is.selected(x) <- value
        return(x)
    }, x=x@.Data, value=value, USE.NAMES=FALSE, SIMPLIFY=FALSE)
    return(x)
})

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("AbsCats", "character"), function (x, value) {
    ix <- match(value, names(x))
    out <- handleMissingCategoryLookup(ix, value, strict=TRUE)
    x[ix] <- lapply(x[ix], `is.na<-`, value=TRUE)
    return(x)
})

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("AbsCats", "logical"), function (x, value) {
    stopifnot(length(x) == length(value))
    x@.Data <- mapply(function (x, value) {
        is.na(x) <- value
        return(x)
    }, x=x@.Data, value=value, USE.NAMES=FALSE, SIMPLIFY=FALSE)
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

setMethod("lapply", "AbsCats", function (X, FUN, ...) {
    X@.Data <- lapply(X@.Data, FUN, ...)
    return(X)
})


#' @rdname Insertions
#' @export
setMethod("anchors", "AbsCats", function (x) {
    f <- vapply(x, anchor, integer(1))
    return(f)
})


#' @rdname Insertions
#' @export
setMethod("funcs", "AbsCats", function (x) {
    f <- vapply(x, func, character(1))
    return(f)
})

# TODO: concatenateAbsCats